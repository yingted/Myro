/******************************************************************************
 *
 * Fluke Scribbler 2 programming code
 * 
 * keith.ohara@gatech.edu
 * January 2011
 * IPRE Fluke Firmware
 *
 * Based on Parallax's PropellerLoader.spin
 * 
 * Assumes Scribbler Reset must be driven high to reset the scribbler
 * (opposite of the PropellerLoader.spin)
 *
 ******************************************************************************/

#include "fluke.h"
#include "scribbler2.h"
#include "uart.h"

//#define SCRIB_DEBUG 1

#define pulse_us 25

//Propeller Commands
#define Shutdown        0
#define LoadRun         1
#define ProgramShutdown 2
#define ProgramRun      3
uint8_t echo, LFSR, ver;

uint32_t command = ProgramRun; // Program and Run from EEPROM


int id_scribbler2(){
  
  int scrib2 = 0;
  scrib2 = program_mode2();
  reset_scribbler2();
  cleanup2();
  return scrib2;
}


void reset_scribbler2()
{  
  IOSET = S_RST;
  usleep(500);
  IOCLR = S_RST;
}

void cleanup2()
{
  uart0Init(B38400, UART_8N1, UART_FIFO_8);  
}

/*
 * Put the scribbler2 into program mode
 */ 
int program_mode2()
{

  int i;
 
#ifdef SCRIB_DEBUG
  putstr("\r\nprogram mode");
  putstr("\r\n");  printbin(PINSEL0);
#endif

  // zero out PINSEL0 so that UART0 acts as regular GPIO
  PINSEL0 &= ~UART0_PINMASK;

  /*'RESn low
    outa[PinRESn] := 0            
    dira[PinRESn] := 1*/
  
  IOSET = S_RST;
     
  /*'P31 high (our TX)
    outa[PinP31] := 1             
    dira[PinP31] := 1*/
  
  IODIR |= S_TXD;
  IOSET = S_TXD;
  
  usleep(500);

  /*'RESn high
    outa[PinRESn] := 1     */
  
  IOCLR = S_RST;

  msleep(100);

  //'output calibration pulses
  //BitsOut(%01, 2)               
  bitsOut(1, 2);
  
  //'send LFSR pattern
  //LFSR := "P"                   
  LFSR = 'P';

  //repeat 250
  //BitsOut(IterateLFSR, 1)
  for (i = 0; i < 250; i++) bitsOut(iterateLFSR(), 1);
  
  //'receive and verify LFSR pattern
  //repeat 250                   
  //  if WaitBit(1) <> IterateLFSR
  //    abort ErrorConnect
  for (i = 0; i < 250; i++){
    if (waitBit(1) != iterateLFSR()){
#ifdef SCRIB_DEBUG
      putstr("FAILED ON LFSR ");
      printdec(i);
      putstr("\n");
#endif
      return 1;
    }
  }

  return 0;
}

int program_scribbler2(unsigned char* buffer, int size)
{  
  int i;
  uint32_t byteCount;  
  
#ifdef SCRIB_DEBUG  
  putstr("\r\nabout to program scribbler2; size = ");
  printdec(size);
#endif
  
  set_led(255);
  
  if (program_mode2()) {
    cleanup2();
    return 1;    
  }
  
  //'receive chip version      
  //repeat 8
  //Ver := WaitBit(1) << 7 + Ver >> 1
  for (i = 0; i < 8; i++) {
    ver = (waitBit(1) << 7) + (ver >> 1);
  }
  
#ifdef SCRIB_DEBUG 
  putstr("version: ");
  printdec(ver);
  putstr("\n");
#endif

  //'if version mismatch, shutdown and abort
  //if Ver <> Version
  //BitsOut(Shutdown, 32)
  //abort ErrorVersion
  
  if (ver != 1){
#ifdef SCRIB_DEBUG
    putstr("VERSION MISMATCH");
#endif
    cleanup2();
    return 1;
  }
  
  //'send command
  //BitsOut(Command, 32)
  bitsOut(command, 32);
  
  //'handle command details
  //if Command          
  if (command){    
    //'send long count
    //ByteCount := byte[CodePtr][8] | byte[CodePtr][9] << 8
    //BitsOut(ByteCount >> 2, 32)
    
    byteCount = buffer[8] | (buffer[9] << 8);
    bitsOut(byteCount >> 2, 32);
    
    //'send bytes
    //repeat ByteCount
    for (i = 0; i < byteCount; i++){
      
      //BitsOut(byte[CodePtr++], 8)
      bitsOut(buffer[i], 8);
    }

  
    //'allow 250ms for positive checksum response
    //if WaitBit(25)
    //abort ErrorChecksum
    if (waitBit(25)){
#ifdef SCRIB_DEBUG      
      putstr("CHECKSUM FAILED\n");
#endif
      cleanup2();
      return 1;
    }
    
    
    //'eeprom program command
    //if Command > 1
    
    if (command > 1){
      
      //'allow 5s for positive program response
      //if WaitBit(500)
      //abort ErrorProgram
      if (waitBit(500)){
#ifdef SCRIB_DEBUG
	putstr("EEPROM1 FAILED\n");
#endif
	cleanup2();
	return 1;
      }
      
      //'allow 2s for positive verify response
      //if WaitBit(200)
      //abort ErrorVerify
      
      if (waitBit(200)){
#ifdef SCRIB_DEBUG       
	putstr("EEPROM2 FAILED\n");	  
#endif
	cleanup2();
	return 1;
      }
    }
  }
  
  cleanup2();
  set_led(0);
  return 0;
}

uint8_t iterateLFSR(){
  uint8_t bit;
  //'get return bit
  //Bit := LFSR & 1
  bit = LFSR & 1;
  
  //'iterate LFSR (8-bit, $B2 taps)
  //LFSR := LFSR << 1 | (LFSR >> 7 ^ LFSR >> 5 ^ LFSR >> 4 ^ LFSR >> 1) & 1
  LFSR = (LFSR << 1) | (((LFSR >> 7) ^ (LFSR >> 5) ^ (LFSR >> 4) ^ (LFSR >> 1)) & 1);    
  
  return bit;
}

void bitsOut(uint32_t value, uint32_t bits)
{
  uint32_t i;
  for (i = 0; i < bits; i++){

    IOCLR = S_TXD;  

    if (value & 1){    
      //'output '1' (1t pulse)      
      //outa[P31] := 0                
      usleep(pulse_us);
      echo = (IOPIN & S_RXD) ? 1 : 0;
    }
    else{	
      //'output '0' (2t pulse)
      //outa[P31] := 0
      //outa[P31] := 0                        
      usleep(pulse_us);
      echo = (IOPIN & S_RXD) ? 1 : 0;
      usleep(pulse_us);
    }
        
    //outa[P31] := 1
    IOSET = S_TXD;     

    usleep(pulse_us);

    value >>= 1;
  }
}
  
 
uint8_t waitBit(uint32_t hundredths){
  uint32_t i;
  uint8_t bit;
  uint8_t priorecho; 
  
  for(i = 0; i < hundredths; i++)
    {
      //'output 1t pulse                        
      bitsOut(1, 1);

      //'sample bit and echo
      //Bit := ina[P30]
      //priorecho := echo
      bit = (IOPIN & S_RXD) ? 1: 0;
      priorecho = echo;
      
      //'output 2t pulse
      bitsOut(0, 1);
      
      //'if echo was low, got bit                                      
      if (!priorecho) return bit;
      
      //'wait 10ms
      msleep(10);
    }

  return 0;
}
