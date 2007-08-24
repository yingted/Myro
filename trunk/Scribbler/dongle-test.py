from dongle import *
from myro import *


robot = Scribbler("/dev/tty.scribbler")

s = robot.ser

print "LED on"
set_led1_on(s)
time.sleep(1.5)
set_led1_off(s)

print "LED 50"
set_led2(s, 50)
time.sleep(1.5)

print "LED 100"
set_led2(s, 100)
time.sleep(1.5)

print "LED 200"
set_led2(s, 200)
time.sleep(1.5)

print "LED 250"
set_led2(s, 250)
time.sleep(1.5)

print "LED 0"
set_led2(s, 0)

for i in range(0,9,3):
    for j in range(0,10,2):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)

print "Writing mem[i:j] = 0"
for i in range(0,9,3):
    for j in range(0,10,2):        
        write_mem(s, i, j, 0)

for i in range(0,9,3):
    for j in range(0,10,2):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)

print "Writing mem[i:j] = i+j"
for i in range(0,9,3):
    for j in range(0,10,2):        
        write_mem(s, i, j, i+j)

for i in range(0,9,3):
    for j in range(0,10,2):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)

print "setting power to 135"
set_ir_power(s, 135)

print "l %d\tm %d\tr %d" % (get_ir_left(s),
                            get_ir_middle(s),
                            get_ir_right(s)) 

v = get_battery(s)
print "battery (%d) = %f " % (v, v / 20.9813)

print "virtual light sensors"

conf_window(s, 0, 0, 0,    84, 191, 1, 1)
conf_window(s, 1, 84, 0,  170, 191, 1, 1)
conf_window(s, 2, 172, 0, 255, 191, 1, 1)

print "l %d\tm %d\tr %d" % (get_window_avg(s, 0),
                            get_window_avg(s, 1),
                            get_window_avg(s, 2)) 

conf_gray_image(s)
before = time.time()
img = grab_gray_image(s)
after = time.time();
print "gray image t = ", (after - before)
img.show()

before = time.time()
img = grab_image(s)
after = time.time();
print "regular image t = ", (after - before)
img.show()
