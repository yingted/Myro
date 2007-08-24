from dongle import *

#from myro import *
#robot = Scribbler("/dev/tty.scribbler")
#s = robot.ser

s = serial.Serial("/dev/tty.scribbler", timeout=10)

for i in range(0,1,1):
    for j in range(0,25*1,1):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)

for i in range(0,1,1):
    print "Erasing page =", i
    erase_mem(s,i)
    time.sleep(1)

for i in range(0,1,1):
    for j in range(0,25*1,1):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)
