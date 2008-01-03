from myro import *

initialize("/dev/tty.scribbler")

print "Back LED 205", setLEDBack(205)
wait(2)

print "Back LED 255", setLEDBack(255)
wait(2)

print "Front LED on", setLEDFront(1)
wait(2)

print "Battery", getBattery()

print "Front LED off", setLEDFront(0)
print "Back LED 0", setLEDBack(0)

b = currentTime()
p = takePicture()
a = currentTime()
"Color picture", (a-b)*1000.0, "millseconds"
show(p)
wait(2)

b = currentTime()
p = takePicture("gray")
a = currentTime()
"Gray picture", (a-b)*1000.0, "millseconds"
show(p)
wait(2)

for power in range(129,140,2):
    print "setting power to", power
    setIRPower(power)
    for i in range(6):    
        print "l %d\tm %d\tr %d" % (getObstacle("left"), getObstacle("center"), getObstacle("right"))
        wait(.25)


