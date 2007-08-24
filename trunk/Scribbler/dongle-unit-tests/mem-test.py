from dongle import *
#from myro import *

#robot = Scribbler("/dev/tty.scribbler")
s = serial.Serial("/dev/tty.scribbler", timeout=10)

#s = robot.ser

for i in range(0,6,3):
    for j in range(0,16,2):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)

print "Writing mem[i:j] = 0"
for i in range(0,6,3):
    for j in range(0,16,2):
        write_mem(s, i, j, 0)

for i in range(0,6,3):
    for j in range(0,16,2):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)

for i in range(0,6,3):
    for j in range(0,16,2):
        write_mem(s, i, j, (10*i)+j)
        print "Writing mem[", i, ":", j, "] =", 10*i + j
        
for i in range(0,6,3):
    for j in range(0,16,2):
        print "Reading mem[", i, ":", j, "] =", read_mem(s, i, j)

