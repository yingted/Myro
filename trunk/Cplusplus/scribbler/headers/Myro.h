#ifndef __MYRO_H__
#define __MYRO_H__

//This header defines general robot opitions, as well as the construction,
//of a global robot variable. The global robot is created upon calling
//connect and destroyed upon calling disconnect.

#include "Scribbler.h"


Scribbler robot;

#define connect() \
	int status = robot.connect(); \
	if(status < 0) { \
		return -1; \
	} \

#define disconnect() \
	robot.disconnect(); \
	return -1; 

extern void wait(double time);
extern bool timeRemaining(double start_time);


#endif
