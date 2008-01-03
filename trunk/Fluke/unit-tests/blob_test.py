from myro import *


initialize("/dev/tty.scribbler")

print "Select your blob box"
show(takePicture())
wait(5)

for i in range(30):
    b = currentTime()
    p = takePicture("blob")
    a = currentTime()
    show(p)
    print (a - b) * 1000.0

    b = currentTime()
    pxs, x, y =  getBlob()
    a = currentTime()
    print (a - b) * 1000.0, pxs, x, y

