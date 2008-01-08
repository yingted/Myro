from myro import *

initialize("/dev/tty.scribbler")

avg = 0.0
num = 10

for i in range(num):
    b = currentTime()
    p = takePicture()
    a = currentTime()
    show(p)
    #name = "img-%02d.jpg" % i
    #savePicture(p, name)
    print (a - b) * 1000.0
    avg += (a-b)

print avg / num
    
