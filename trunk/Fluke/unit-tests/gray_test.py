from myro import *

initialize("/dev/tty.scribbler4812")

avg = 0.0
num = 100

for i in range(num):
    b = currentTime()
    p = takePicture("gray")
    a = currentTime()
    show(p)
    #name = "img-%02d.jpg" % i
    #savePicture(p, name)
    print (a - b) * 1000.0
    avg += (a-b)

print avg / num
    
