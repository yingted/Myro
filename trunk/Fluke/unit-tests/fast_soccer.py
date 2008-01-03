from myro import *

robot = Scribbler("/dev/tty.scribbler")

GO=True

TOO_LEFT = 80.0
TOO_RIGHT = 144.0
GAIN = 0.5

#robot.autoCamera()

#pink ball
robot.conf_rle(y_low=0, y_high=255, u_low=51, u_high=136, v_low=190, v_high=254)

show(takePicture())
print "select the color to track"
#wait(5)
show(takePicture('blob'))
show(takePicture('blob'))
show(takePicture('blob'))
print "starting loop"

lastdir = 'r'

for i in range(40):

    print i

    b = currentTime()
    px_cnt, avg_x, avg_y = getBlob()
    a = currentTime()
    print "Time = ", (a-b)*1000.0
    
    print px_cnt, avg_x
    
    if (px_cnt < 20):
        if GO:
            if lastdir == 'l':
                turnLeft(GAIN)
            else:
                turnRight(GAIN)
            print "searching"
    elif (avg_x < TOO_LEFT):
        lastdir = 'l'
        if GO:
            turnLeft(GAIN)
        print "turning left"
    elif (avg_x > TOO_RIGHT):
        lastdir = 'r'
        if GO:
            turnRight(GAIN)
        print "turning right"
    else:
        print "headed toward the object"
        if GO:
            forward(GAIN)

stop()
