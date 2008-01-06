from myro import *

init("/dev/tty.scribbler")

darkenCamera()

for i in range(60):
    left, center, right = robot.get("bright")    
    print left, center, right
    
