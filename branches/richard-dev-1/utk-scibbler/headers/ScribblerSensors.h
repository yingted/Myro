#ifndef __SCRIBBLER_SENSORS_H__
#define __SCRIBBLER_SENSORS_H__

#include <vector>
#include <string>

class ScribblerSensors {

	public:

	ScribblerSensors(int dongle);
	~ScribblerSensors();

	private:

	std::vector<std::string> info;
	std::string name;
	std::string password;

	//scribbler sensors
	std::vector<int> scribSensors;

	//dongle sensors
	std::vector<int> obstacles;
	std::vector<int> bright;

}

#endif
