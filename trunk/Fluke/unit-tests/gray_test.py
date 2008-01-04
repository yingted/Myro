from myro import *

initialize("/dev/tty.scribbler5964")

for i in range(100):
    b = currentTime()
    p = takePicture("gray")
    a = currentTime()
    show(p)
    print (a - b) * 1000.0
    
