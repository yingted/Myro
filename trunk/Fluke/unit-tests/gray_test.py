from myro import *

initialize()

for i in range(100):
    b = currentTime()
    p = takePicture("gray")
    a = currentTime()
    show(p)
    print (a - b) * 1000.0
    
