from dongle import *

s = serial.Serial("/dev/tty.scribbler", timeout=10)

before = time.time()
img = grab_image(s)
after = time.time();
print "t = ", (after - before)
img.show()
