from dongle import *

s = serial.Serial("/dev/tty.scribbler", timeout=10)

print "About to program scribbler with the program stored in memory"

set_scribbler_start_program(s, 0)
time.sleep(5)
print "resetting scribbler"
set_reset_scribbler(s)
