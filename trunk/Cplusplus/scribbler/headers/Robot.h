#ifndef __ROBOT_H__
#define __ROBOT_H__

#include <string>

/**
 * @class Robot
 *
 * @brief A generic robot class that specifies general required
 * functions for basic robot operation. 
 *
 * @Author Richard Edwards
 */
class Robot {

	public:	
	
	/**
	 * Abstract function for connecting to the robot.
	 */
	virtual int connect()=0;

	/**
	 * Abstract function for disconnecting from the robot.
	 */
	virtual int disconnect()=0;

	/**
	 * Abstract function for moving a robot with some
	 * translation velocity, rotational velocity, and possibly 
	 * some z-dimension velocity.
	 */
	virtual void move(double translate, double rotate, double z = 0)=0;

	/**
	 * Abstract function for issuing a translational velocity.
	 */
	virtual void translate(double val)=0; //need more meaningful var names!

	/**
	 * Abstract function for issuing a rotational velocity.
	 */
	virtual void rotate(double val)=0;
	
	void stop();

	/**
	 * Defines a default behavior for issuing speeds to the left
	 * and right motor. However, it requires that your class has 
	 * implemented the move function.
	 */
	void motors(double leftSpeed, double rightSpeed);

	/**
	 * A default behavior for turning the robot left for either
	 * some specified amount of time, or to be executed until another
	 * command is sent.
	 */
	void turnLeft(double speed, double time=0);

	/**
	 * A default behavior for turning the robot right for either
	 * some specified amount of time, or to be executed until another
	 * command is sent.
	 */
	void turnRight(double speed, double time=0);

	/**
	 * A default turn function for telling the robot to turn 
	 * left or right at a preset speed, for some amount of time
	 * or until another command is issued.
	 */
	void turn(double direction, double time=0);

	/**
	 * A default turn function for telling the robot to turn left
	 * or right at a specified speed, for some amount of time
	 * or until another command is issued.
	 */
	void turn(std::string direction, double value, double time=0);

	/**
	 * A default function for telling the robot to move forward
	 * at a specified speed and for some duration of time,
	 * or until another command is sent.
	 */
	void forward(double speed, double time=0);

	/**
	 * A default function for telling the robot to move in reverse
	 * at a specified speed and for some duration of time, or 
	 * until another command is sent.
	 */
	void backward(double speed, double time=0);

	private:
};

#endif
