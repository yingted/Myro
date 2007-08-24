from dongle import *
import sys

s = serial.Serial("/dev/tty.scribbler", timeout=10)

try:
    s.flushOutput()
    s.flushInput()    

    for i in range(0,3):
        v = get_battery(s)
        print "battery (%d) = %f " % (v, v / 20.9813)

finally:
    s.close()
