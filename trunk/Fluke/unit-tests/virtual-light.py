from myro import *
init("/dev/tty.scribbler")

for i in range(60):
    left, center, right = robot.get("bright")    
    print left, center, right
    
