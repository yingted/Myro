from myro import *

initialize("/dev/tty.scribbler")

for i in range(2):
    b = currentTime()
    p = takePicture()
    a = currentTime()
    show(p)
    #name = "img-%02d.jpg" % i
    #savePicture(p, name)
    print (a - b) * 1000.0
    
