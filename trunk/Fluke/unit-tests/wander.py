from myro import *
import random
from time import *

initialize("/dev/tty.scribbler")

speed=0.7

def brain():

    setIRPower(135)
    
    for i in range(60):
        r=getObstacle("right")
        l=getObstacle("left")
        c=getObstacle("center")
        print "%d %d %d %d\n" % (i, l, c, r),
        if r > 1000:
            backward(speed, .1)
            rotate(speed)
            sleep(.1)
        elif l > 1000 or c > 9000:
            backward(speed, .1)
            rotate(-speed)
            sleep(.1)
        else:
            forward(speed)

    stop()

    # end of brain

print "battery:", getBattery()
brain()
