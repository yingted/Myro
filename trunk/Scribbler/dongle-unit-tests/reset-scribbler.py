from dongle import *

s = serial.Serial("/dev/tty.scribbler", timeout=10)

print "resetting scribbler"
set_reset_scribbler(s)
