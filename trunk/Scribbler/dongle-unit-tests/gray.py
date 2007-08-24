from dongle import *

s = serial.Serial("/dev/tty.scribbler", timeout=10)

conf_gray_image(s)
before = time.time()
img = grab_gray_image(s)
after = time.time();
print "t = ", (after - before)
img.show()
