from myro import *
robot = None

comPort = "/dev/tty.scribbler"
robot = Scribbler(comPort)

for i in range(60):
    left, center, right = robot.get("bright")    
    print left, center, right
    
