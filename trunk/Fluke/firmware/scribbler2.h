/*******************************************************************************
 *
 * Scribbler 2 programming interface
 * 
 * keith.ohara@gatech.edu
 * January 2011
 * IPRE Fluke Firmware
 *
 ******************************************************************************/

#ifndef __SCRIBBLER2__
#define __SCRIBBLER2__

#include "fluke.h"

void reset_scribbler2();
int id_scribbler2();
int program_scribbler2(unsigned char* buffer, int size);


uint8_t waitBit(uint32_t hundredths);
void bitsOut(uint32_t value, uint32_t bits);
uint8_t iterateLFSR();
int program_mode2();
void cleanup2();
#endif /* __SCRIBBLER__ */


