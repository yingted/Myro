#ifndef __INFRARED__
#define __INFRARED__

// in tenths of a microsecond
// good settings before messing with xmit/bounce
//#define NOMINAL_LOW_BIT    4250 
//#define NOMINAL_HIGH_BIT   3750

#define NOMINAL_LOW_BIT    4250 
#define NOMINAL_HIGH_BIT   4000

#define BIT_VARIANCE       1000

void ir_rx_init();
void ir_rx_disable();
void ir_irq() __attribute__ ((interrupt("IRQ")));
int  ir_queue_full();
int  ir_queue_read(uint32_t* datay);
void process_ir_buffer();
void process_packet(uint8_t data);

void ir_send_byte(uint8_t data, uint32_t emitter);

extern uint8_t* msg_buffer;
extern uint8_t msg_size;
extern uint8_t msg_available;


#endif /* __INFRARED__*/

