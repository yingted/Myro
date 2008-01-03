from myro import *

initialize("/dev/tty.scribbler")

for i in range(10):
    b = currentTime()
    p = takePicture("gray")
    a = currentTime()
    show(p)
    print (a - b) * 1000.0
    
