#include "Robot.h"
#include <math.h>
#include <iostream>

void Robot::stop() {
	this->move(0,0);
}

void Robot::motors(double leftSpeed, double rightSpeed) {
	double trans = (leftSpeed + rightSpeed) / 2.0;
	double rotate = (rightSpeed - leftSpeed) / 2.0;
	this->move(trans, rotate);
}

void Robot::turnLeft(double speed, double time) {
	int utime = (int)(time * pow(10, 6));
	this->move(0, speed);
	if(time != 0) {
		usleep(utime);
		this->stop();
	}
}

void Robot::turnRight(double speed, double time) {
	int utime = (int)(time * pow(10, 6));
	this->move(0, -1 * speed);
	if(time != 0) {
		usleep(utime);
		this->stop();
	}
}

void Robot::turn(double direction, double time) {
	this->move(0, direction);
	if(time != 0) {
		usleep( (int)(time * pow(10, 6)) );
		this->stop();
	}
}

void Robot::turn(std::string direction, double speed, double time) {
	std::string lower_direction;
	for(int i = 0; i < (int)direction.length(); i++)
		lower_direction += tolower(direction.at(i));
	
	if(direction == "left")
		this->move(0, speed);
	else if(direction == "right")
		this->move(0, -speed);
	else if(direction == "straight" || direction == "center")
		this->move(0,0);
	else {
		std::cout << "Invalid Direction\n";
	}
	if(time != 0) {
		usleep( (int)(time * pow(10, 6)) );
		this->stop();
	}
}

void Robot::forward(double speed, double time) {
	int utime = (int)(time * pow(10, 6));
	this->move(speed, 0);
	if(time != 0) {
		usleep(utime);
		this->stop();
	}
}

void Robot::backward(double speed, double time) {
	int utime = (int)(time * pow(10,6));
	this->move(-1 * speed, 0);
	if(time != 0) {
		usleep(utime);
		this->stop();
	}
}
