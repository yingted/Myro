from dongle import *
import sys

s = serial.Serial("/dev/tty.scribbler", timeout=10)

try:
    s.flushOutput()
    s.flushInput()
    
    if (len(sys.argv) > 2):
        start = int(sys.argv[1])
        stop = int(sys.argv[2])
    else:
        start = 120
        stop = 145
        
    for level in range(start, stop):
        #print "Using IR power = ", level
        set_ir_power(s, level)
        
        for i in range(0,1):
            print "power %d\tl %d\tm %d\tr %d" % (level,
                                                  get_ir_left(s),
                                                  get_ir_middle(s),
                                                  get_ir_right(s)) 
            #time.sleep(.25)

finally:
    s.close()
