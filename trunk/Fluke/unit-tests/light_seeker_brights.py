# File: followlight.py
# Date: October 19, 2006
# Created by: Deepak Kumar
# Purpose: Follow the light
#           This program illustrates the basic structure of a robot's brain
#           Each robot brain has to first connect to the robot (using the com
#           port configured for the bluetooth connection).
#           Once connected, the robot can be issued any command...
#
#           In this example, the robot tries to follow a light source.
#           Use a flashlight to direct its beam on one of the robot's
#           left or right light sensors and the robot will turn towards it
#
from myro import *

robot = None

# First connect to the robot...
comPort = "/dev/tty.scribbler"
print "Connecting to robot over", comPort, "...",
robot = Scribbler(comPort)
print "connected!"

robot.darkenCamera()
    
def brain():

    GAIN=0.25

    for i in range(90):

        b = currentTime()
        left = getBright("left")
        right = getBright("right")
        a = currentTime()
        
        print i, (a - b) * 1000.0, left, right
        
        if (left - right) > 0:            
            robot.move(GAIN, GAIN)
            print "Turning left"            
        elif (right - left) > 0:
            robot.move(GAIN, -GAIN)            
            print "Turning right"            
        else:
            if left >= 220000: #0.9: 
                print "Found the light"
                robot.move(GAIN, 0.0)
            else:
                print "search"
                robot.move(GAIN, GAIN)


brain()
