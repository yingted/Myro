from dongle import *

s = serial.Serial("/dev/tty.scribbler", timeout=10)

print "virtual light sensors"

conf_gray_window(s, 0, 0, 0,    84, 191, 1, 1)
conf_gray_window(s, 1, 84,  0, 170, 191, 1, 1)
conf_gray_window(s, 2, 172, 0, 255, 191, 1, 1)

print "left"
g = grab_gray_window(s, 0, 0, 0,    84, 191, 1, 1)
g.show()

print "middle"
g = grab_gray_window(s, 1, 84, 0,  170, 191, 1, 1)
g.show()

print "right"
g = grab_gray_window(s, 2, 172, 0, 255, 191, 1, 1)
g.show()
