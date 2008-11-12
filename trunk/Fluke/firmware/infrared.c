#include "lpc210x.h"
#include "lpcUART.h"
#include "vic.h"
#include "fluke.h"
#include "infrared.h"

#include "vision.h"

extern unsigned char rle[RLE_BUFFER_SIZE+4] __attribute__((aligned(4)));

#define IR_BUFFER_SIZE 256
#define MSG_BUFFER_SIZE 32

uint32_t* ir_buffer;

volatile int producer_index = 0;
volatile int consumer_index = 0;

uint8_t* msg_buffer;
uint8_t msg_size = 0;

void ir_rx_init()
{

  ir_buffer = (uint32_t*)rle;
  msg_buffer = (uint8_t*)(rle + IR_BUFFER_SIZE*4);;
 
  // enable capture on IRIN (pin 0.10) uses timer 1
  PINSEL0 |= (2 << 20);
  
  // setup prescalar of TIMER1 such that a tick is 0.1 usecs (or 10 ticks per usec)
  TIMER1_PR   = 0x5;     // set prescalar 
  
  // reset TIMER 1
  TIMER1_TCR  = 0x2;     // reset counter

  // configure capture register
  
  // 0 Capture on capture[0] rising edge When one, a sequence of 0
  // then 1 on capture[0] will cause CR0 to be loaded with the
  // contents of the TC. When zero this feature is disabled.
  
  // 1 Capture on capture[0] falling edge When one, a sequence of 1
  // then 0 on capture[0] will cause CR0 to be loaded with the
  // contents of TC. When zero this feature is disabled. 

  // 2 Interrupt on capture[0] event When one, a CR0 load due to a
  // capture[0] event will generate an interrupt. When zero this
  // feature is disabled.

  TIMER1_CCR |= 0x7;     

  // interrupt us every 5 milliseconds to make sure we catch
  // the end of the packet
  //TIMER1_MR0 = 50000;
  //TIMER1_MCR = 0x3;   // reset the tiemr on match, and interrupt

  // start TIMER1
  TIMER1_TCR  = 0x1;      

  // add the handler for TIMER1
  vic_add_handler(VIC_TIMER1, ir_irq);

}

void ir_rx_disable()
{
  // disable capture on IRIN (pin 0.10) uses timer 1
  PINSEL0 &= ~(2 << 20);
  TIMER1_CCR &= ~0x7;
}

void ir_irq()
{  
  //static int current_time = 0;
  //static int last_time = 0;
  static int next_write = 0;
  //static int delta = 0;
  
  //current_time = TIMER1_CR0;  
  
  //delta = current_time - last_time;
  
  next_write = producer_index + 1;  
  
  if (next_write == IR_BUFFER_SIZE)
    {
      next_write = 0;
    }
  
  if (next_write != consumer_index)
    { 
      ir_buffer[producer_index] = TIMER1_CR0; //delta; //current_time - last_time;
      
      producer_index ++;
      if (producer_index == IR_BUFFER_SIZE)
	{
	  producer_index = 0;
	}
    }
  
  //last_time = current_time;

  TIMER1_TCR  = 0x2;       // reset counter
  TIMER1_TCR  = 0x1;       // start counter
  
  TIMER1_IR = (1 << 4);    // clear the interrupt for capture 1.0
  VICVectAddr = 0;
}

int ir_queue_full()
{
  // Since the queue is empty when read==write,
  // the queue is full when the next character written would
  // produce an incorrect queue-empty indication
  // (write+1==read).

  int next_write;

  next_write = producer_index + 1;  
  
  if (next_write == IR_BUFFER_SIZE)
    {
      next_write = 0;
    }
  
  return (next_write == consumer_index); 
}


int ir_queue_write(int data)
{
  int next_write = 0;
  
  next_write = producer_index + 1;  
  
  if (next_write == IR_BUFFER_SIZE)
    {
      next_write = 0;
    }
  
  if (next_write != consumer_index)
    { 
      ir_buffer[producer_index] = data;
      
      producer_index ++;
      if (producer_index == IR_BUFFER_SIZE)
	{
	  producer_index = 0;
	}
      return 0;
    }

  return -1;
}


int ir_queue_read(uint32_t* delta)
{
  (*delta) = 0;

  if (consumer_index != producer_index)
    {
      // read the data
      *delta = ir_buffer[consumer_index];
      
      // Move to the next item in the queue
      consumer_index++;
      
      // might have to move to the next byte
      
      if (consumer_index == IR_BUFFER_SIZE)
	{
	  consumer_index = 0;
	}

      return 1;
    }

  // empty
  return -1;
}


// low level IR processing - serial level
void process_ir_buffer()
{
  static enum {FALLING = 0, RISING = 1} pinstate = FALLING;
  static enum {START_BIT, DATA_BIT, PARITY_BIT, STOP_BIT} bitstate = STOP_BIT;  
  static uint8_t data_bit = 0;
  static uint8_t data = 0;
  static int parity = 0;
  int bits = 0;
  uint32_t delay = 0;

  // the condition where the data bits, parity bits and stop bits
  // are all 1 causing the interrupt not to trigger
  bits = (TIMER1_TC + NOMINAL_HIGH_BIT/2) / NOMINAL_HIGH_BIT;
  if (data_bit + bits > 9)
    {
      ir_queue_write(TIMER1_TC); 
      TIMER1_TCR  = 0x2;       // reset counter
      TIMER1_TCR  = 0x1;       // start counter
      ir_queue_write(1);      // write a dummy to keep bit state consistent
    }
  
  while (ir_queue_read(&delay) >= 0)
    {
      bits = 0;
      // Hiccup, ignore this 
      if (delay < 500) continue;

      if (delay > 50000) 
	{
	  bitstate = STOP_BIT;
	  pinstate = FALLING;
	}
      
      if (pinstate == FALLING)
	{
	  if (delay <= NOMINAL_HIGH_BIT)
	    bits = 1;
	  else	
	    bits = (delay + NOMINAL_HIGH_BIT/2) / NOMINAL_HIGH_BIT;
}
      else
	{
	  if (delay <= NOMINAL_LOW_BIT)
	    bits = 1;
	  else
	    bits = (delay + NOMINAL_LOW_BIT/2) / NOMINAL_LOW_BIT;
	  //bits = delay / NOMINAL_LOW_BIT;
	}	
      
#if DEBUG_IR
      putstr("\r\nPIN: ");
      printdec(pinstate);
      putstr("\tDELAY: ");
      printdec(delay);
      putstr("\tBITS: ");
      printdec(bits);
#endif /* DEBUG_IR */

      if (bitstate == START_BIT && pinstate && bits)
	{
#ifdef DEBUG_IR
	  putstr("\r\nSTART BIT");
#endif
	  bits --;
	  bitstate = DATA_BIT;
	  parity = 0;	  
	  data_bit = 0;
	}
      
      if (bitstate == DATA_BIT && bits)
	{
	  while (data_bit < 8 && bits > 0)
	    {
#ifdef DEBUG_IR
	      putstr("\r\nDATA BIT(");
	      printdec(data_bit);
	      putstr(") ");
	      //printdec(!pinstate);
#endif /*DEBUG_IR*/
	      if (!pinstate)
		{
		  data |= (1 << data_bit);		  
		  parity ++;

		}
	      bits --;
	      data_bit++;
	    }
	  
	  if (data_bit == 8)
	    {
	      bitstate = PARITY_BIT;
	    }
	}
      
      if (bitstate == PARITY_BIT && bits)
	{	      
#ifdef DEBUG_IR
	  putstr("\r\nPARITY BIT");
#endif
	  
	  if ((parity & 0x1) == pinstate)
	    {
	      process_packet(data);
	    }
	  else
	    {
	    }

#if 0
	  putstr("\r\nparity ");
	  putstr("bit =  ");
	  printhex(pinstate);
	  putstr("\t num ones ");
	  printhex(parity);
	  putstr("\tdata ");
	  printhex(data);
#endif


	  data = 0;
	  bitstate = STOP_BIT;
	  bits --;
	}	  
      
      if (bitstate == STOP_BIT && bits && !pinstate)
	{	      
#ifdef DEBUG_IR
	  putstr("\r\nSTOP BIT");
#endif /*DEBUG_IR*/
	  bitstate = START_BIT;
	  bits --;
	}
      
      pinstate = !pinstate;
    }
}


// high-level IR processing packet level
void process_packet(uint8_t data)
{
  if (msg_size < MSG_BUFFER_SIZE)
    {
      msg_buffer[msg_size] = data;
      msg_size++;
    }
}


void emit_zero(uint32_t emitter)
{
  // prescalar should be about 1 usec
  TIMER0_TCR  = 0x2;     // reset counter
  TIMER0_TCR  = 0x1;     // start counter
  while (TIMER0_TC < XMIT_ZERO_TIME)
    {
      // spin
    }
}

void emit_one(uint32_t emitter)
{
  uint32_t total;
  // prescalar should be about 1 usec

  total = 0;
  TIMER0_TCR  = 0x2;     // reset counter
  TIMER0_TCR  = 0x1;     // start counter

  while (total < XMIT_ONE_TIME)
    {
      TIMER0_TCR  = 0x2;     // reset counter
      TIMER0_TCR  = 0x1;     // start counter
      while (TIMER0_TC < 13)
	{
	}
      total += TIMER0_TC;
      IOSET = emitter;

      TIMER0_TCR  = 0x2;     // reset counter
      TIMER0_TCR  = 0x1;     // start counter
  
      while (TIMER0_TC < 12)
	{
	}
      total += TIMER0_TC;
      IOCLR = emitter;
    }
}

void ir_send_byte(uint8_t data, uint32_t emitter)
{
  uint8_t i;
  uint8_t parity = 0;
  int bit;

  // start bit
  emit_one(emitter);

  // 8 data bits LSB->MSB
  for (i = 0; i < 8; i++)
    {
      bit = (data >> i) &  0x1;
      if (bit)
	{
	  emit_zero(emitter);
	  parity ++;
	}	
      else
	{
	  emit_one(emitter);
	}
    }

  // Parity bit
  if (parity & 0x01)
    {
      emit_one(emitter);
    }
  else
    {
      emit_zero(emitter);
    }

  // stop bit
  emit_zero(emitter);  
}





