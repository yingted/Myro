import time
import sys
import logging
import time
import serial
from numpy import *

ser = serial.Serial(0, timeout=5)  # DSB: had to change from 5 to 0 to work
ser.baudrate = 9600
print ser

ser.flushInput()
ser.flushOutput()

def test():
    data = "a"
    ser.write(data)
    buf = ser.read(1)
    if (buf != data):            # DSB: how to turn off echo?
        print "ERROR: Mismatch"
    buf = ser.read(1)    
    if (buf != data):
        print "ERROR: Mismatch"
        

if __name__ == '__main__':

    try:
        outfile = "home-scrib.txt"
        if (len(sys.argv) == 2):
            outfile = sys.argv[1]

        print "Starting test..."
        a = []
        for i in range(0,10000):
            before = time.clock()
            test()
            a.append(time.clock() - before)

        numpy_a = array(a)
        numpy_a.tofile(outfile, sep='\n', format = "%e")
        
        print "Done!"
    finally:
        ser.close()

