# File: avoid.py
# Date: October 5, 2006
# Created by: Deepak Kumar
# Purpose: Run around without bumping into anything
#           This program illustrates the basic structure of a robot's brain
#           Each robot brain has to first connect to the robot (using the com
#           port configured for the bluetooth connection).
#           Once connected, the robot can be issued any command...
#
#           In this example, the robot senses its environment for any obstacles
#           If it detects on on the left it turns right, if there is an obstacle
#           on the right, it turns left, otherwise it goes forward in a straight line
#
from myro import *

# First connect to the robot...
name = "Scribby"
print "Connecting to", name
robot = Scribbler(name)
#robot = SimScribbler("ID1212")
print "connected!"
time.sleep(1)

def brain():

    while True:
        left, right = robot.read("ir")
        print left, right
        if not left:
            robot.move(0.0, 0.5)
        elif not right:
            robot.move(0.0, -0.5)
        else:
            robot.move(0.5, 0.0)
        
    # end of brain

brain()
