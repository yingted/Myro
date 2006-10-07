# File: scale.py
# Date: October 5, 2006
# Created by: Deepak Kumar
# Purpose: Play an entire scale of notes
#

from myro import *

# First connect to the robot...
comPort = "com4"
print "Connecting to robot over", comPort, "...",
#robot = Scribbler(comPort)
robot = SimScribbler("ID1212")
print "connected!"
time.sleep(2)

def brain():    
   
    robot.beep(0.5, 262)    # C4
    robot.beep(0.5, 294)    # D4
    robot.beep(0.5, 330)    # E4
    robot.beep(0.5, 349)    # F4
    robot.beep(0.5, 392)    # G4
    robot.beep(0.5, 440)    # A4
    robot.beep(0.5, 494)    # B4
    robot.beep(0.5, 523)    # C5
    # end of brain

brain()
