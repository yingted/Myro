from dongle import *

s = serial.Serial("/dev/tty.scribbler", timeout=10)

print "virtual light sensors"

conf_window(s, 0, 0, 0,    84, 191, 1, 1)
conf_window(s, 1, 84,  0, 170, 191, 1, 1)
conf_window(s, 2, 172, 0, 255, 191, 1, 1)

print "left"
g = grab_window(s, 0, 0, 0,    84, 191, 1, 1)
g.show()

print "middle"
g = grab_window(s, 1, 84, 0,  170, 191, 1, 1)
g.show()

print "right"
g = grab_window(s, 2, 172, 0, 255, 191, 1, 1)
g.show()

for i in range(0,20):
    print "l %d\tm %d\tr %d" % (get_window_avg(s, 0),
                                get_window_avg(s, 1),
                                get_window_avg(s, 2)) 
    time.sleep(0.5)
