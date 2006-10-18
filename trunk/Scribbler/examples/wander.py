# Date: October 6, 2006
# Created by: Deepak Kumar
# Purpose: Run around bumping into everything(!!)
#           This program illustrates the basic structure of a robot's brain
#           Each robot brain has to first connect to the robot (using the com
#           port configured for the bluetooth connection).
#           Once connected, the robot can be issued any command...
#
#           In this example, the robot tries to move forward at all times.
#           if it runs into something, it senses that through its stall
#           sensor. Then it tries to maneuver out by backing up and twisting.
#
from myro import *
import random
from time import *

# First connect to the robot...
comPort = "com7"
print "Connecting to robot over", comPort, "...",
robot = Scribbler(comPort)
#robot = SimScribbler("ID1212")
print "connected!"
sleep(1)

def brain():

    while True:
        if robot.readStall():
            robot.move(-0.5, 0.5)   # can also try random turns here
            sleep(0.2)
            robot.stop()
        else:
            robot.move(0.5, 0.0)


    # end of brain

brain()
robot.close()
