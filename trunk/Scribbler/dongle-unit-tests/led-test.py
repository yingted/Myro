from dongle import *

import sys

s = serial.Serial("/dev/tty.scribbler", timeout=10)

try:
    s.flushOutput()
    s.flushInput()    

    for i in range(0, 255, 5):
        print "Setting led to ", i
        set_led1_on(s)
        time.sleep(.5)                
        set_led1_off(s)
        set_led2(s, i)
        time.sleep(.5)

    print "Turning LED off"
    set_led2(s,0)
finally:
    s.close()
