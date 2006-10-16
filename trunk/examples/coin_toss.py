# File: cointoss.py
# Date: October 5, 2006
# Created by: Deepak Kumar
# Purpose: The robot simulates a coin toss. It tosses a coin (in its head)
#           if it comes up heads, it moves forward some.
#           if it comes up tails, it moves backwards some.
#           Do this several times, does the robot ever end up in the same spot?
#           Is its coin toss fair?
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

    for i in range(10):
        toss = random.randrange(0,2)    # toss a coin, outcome = 0/1
        if toss:                        # heads
            robot.move(0.5, 0.0)
            sleep(0.5)
            robot.stop()
        else:                           # tails
            robot.move(-0.5, 0.0)
            sleep(0.5)
            robot.stop()

    # end of brain

brain()
robot.close()
