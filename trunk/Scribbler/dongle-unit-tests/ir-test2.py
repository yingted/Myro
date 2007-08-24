from dongle import *

import sys
s = serial.Serial("/dev/tty.scribbler", timeout=10)

try:
    s.flushOutput()
    s.flushInput()
    
    if (len(sys.argv) > 1):
        level = int(sys.argv[1])
    else:
        level = 135

    print "Using IR power = ", level
    set_ir_power(s, level)
    
    while 1:
        print "power %d\tl %d\tm %d\tr %d" % (level,
                                              get_ir_left(s),
                                              get_ir_middle(s),
                                              get_ir_right(s)) 
        #print "l: %d" % get_ir_left(s),
        #print "m: %d" % get_ir_middle(s)
        #print "r: %d" % get_ir_right(s)

finally:
    s.close()
