from myro import *
import sys

init()

while 1:
    msg = getIRMessage()    
    if len(msg) > 0:
        #print msg
        for i in msg:
            if i == chr(0x55):
                print 
            print "%.02x" %ord(i),

    wait(0.1)
    
