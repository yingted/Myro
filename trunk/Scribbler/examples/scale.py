# File: scale.py
# Date: October 5, 2006
# Created by: Deepak Kumar
# Purpose: Play an entire scale of notes
#

from myro import *
import time

# First connect to the robot...
print "Connecting to robot Scribby ..."
robot = Scribbler("Scribby")
#robot = Scribbler("com1")
#robot = SimScribbler("ID1212")
time.sleep(1)

def brain():    
   
    robot.beep(0.2, 262)    # C4
    robot.beep(0.2, 294)    # D4
    robot.beep(0.2, 330)    # E4
    robot.beep(0.2, 349)    # F4
    robot.beep(0.2, 392)    # G4
    robot.beep(0.2, 440)    # A4
    robot.beep(0.2, 494)    # B4
    robot.beep(0.2, 523)    # C5
    # end of brain

print robot.getName()
robot.setName("Scribby")
print robot.getName()

before = time.clock()
brain()
print(time.clock() - before)
robot.close()
