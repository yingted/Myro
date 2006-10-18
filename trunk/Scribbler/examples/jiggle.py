# File: jiggle.py
# Date: September 29, 2006
# Created by: Deepak Kumar
# Purpose: A first program for the Scribbler!
#           This program illustrates the basic structure of a robot's brain
#           Each robot brain has to first connect to the robot (using the com
#           port configured for the bluetooth connection).
#           Once connected, the robot can be issued any command...
#
#           In this example, the robot spins clockwise for 1 second, beeps
#           and then spins counter-clockwise for 1 second, beeps and stops.
#
from myro import *
import time

# First connect to the robot...
comPort = "com7"
print "Connecting to robot over", comPort, "...",
robot = Scribbler(comPort)
#robot = SimScribbler("ID1212")
print "connected!"
time.sleep(2)

def brain():
   
    # algorithm for jiggling...
    #   jiggle left for 1 second and beep
    robot.rotate(0.5)
    time.sleep(1)
    robot.beep(0.5, 800)
    
    #   jiggle right for 1 second and beep
    robot.rotate(-0.5)
    time.sleep(1)
    robot.beep(0.5, 800)

    # done
    robot.stop()    
    # end of brain

brain()
robot.close()
