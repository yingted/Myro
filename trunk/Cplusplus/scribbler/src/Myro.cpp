#include "Myro.h"
#include <sys/time.h>
#include <time.h>
#include <cmath>
#include <unistd.h>

static struct timeval start_clock_time;
static struct timezone tz;
static int q = 0;

void wait(double time) {
	int utime = (int)(time * pow(10,6));
	if(time != 0)
		usleep(utime);
}

bool timeRemaining(double start_time) {	
	if(!q) {
		q = 1;
		gettimeofday(&start_clock_time, &tz);
		return true;
	}

	bool result = true;

	struct timeval currenttime;
	gettimeofday(&currenttime, &tz);

	if( start_time * pow(10,6) + start_clock_time.tv_usec 
			>= currenttime.tv_usec ) {
		result = false;
		q = 0;
	}
	return result;
}
