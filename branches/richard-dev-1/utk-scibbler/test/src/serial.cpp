#include "serial.h"
#include <termios.h>
#include <stdio.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string>

serial::serial(int baudrate, int block_read) {
	fd = -1;
	this->baudrate = baudrate;
	this->blocking = block_read;
}

serial::~serial() {
}

int serial::connect(const char * device) {
	int status = 0;
	fd = open(device, O_RDWR | O_NOCTTY);
	if(fd < 0) {
		perror(device);
		fd = -1;
		status = -1;
	}
	else {
		struct termios io;
		tcgetattr(fd, &io);
		cfsetospeed(&io, this->baudrate);
		cfsetispeed(&io, this->baudrate);
		cfmakeraw(&io);
		tcsetattr(fd, TCSANOW, &io);
	}
	return status;
}

void serial::disconnect() {
	close(fd);
	fd = -1;
}

int serial::_write(unsigned char * buf, int bytes) {

	if(fd == -1) {
		fprintf(stderr, "serial connection not established\n");
		return -1;
	}
	return write(fd, buf, bytes);
}

int serial::_read(unsigned char * buf, int bytes) {

	if(fd == -1) {
		fprintf(stderr, "serial connection not established\n");
		return -1;
	}
	int results = 0;
	for(; results < bytes; )
		results += read(fd, buf + results, bytes - results);
	return results;
}

void serial::flush_input() {
	tcflush(fd, TCIFLUSH);
}

void serial::flush_output() {
	tcflush(fd, TCOFLUSH);
}
