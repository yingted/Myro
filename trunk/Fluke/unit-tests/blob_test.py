from myro import *


initialize("/dev/tty.scribbler")

print "Select your blob box"
show(takePicture())
wait(5)

num = 20
rle_avg = 0.0
blob_avg = 0.0

for i in range(30):
    b = currentTime()
    p = takePicture("blob")
    a = currentTime()
    show(p)
    print (a - b) * 1000.0
    rle_avg += (a-b)
    
    b = currentTime()
    pxs, x, y =  getBlob()
    a = currentTime()
    print (a - b) * 1000.0, pxs, x, y
    blob_avg += (a-b)


print "RLE/blob avg", rle_avg/num
print "getBlob() avg", blob_avg/num
