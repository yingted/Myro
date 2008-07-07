from myro import *

print "Welcome, this program follows a colored object..."

configureBlob(0, 255, 40, 120, 0, 255)

# Enter an endless loop.
# In this loop, we will read some sensor values, decide what to do, then do it!
# The loop means we'll keep repeating this cycle over and over again, quickly.
# Known as "sense, think, act"
while(True):
    
    # Sense: Read the "blob" image
    blobImg = takePicture("blob")

    show(blobImg)

    # Think: Perform a calculation on the sensor values to determine the
    # desired motion.

    # Find the blob centroid
    cx = 0
    #cy = 0
    count = 0
    for pix in getPixels(blobImg):
        if getRed(pix) == 255:
            cx = cx + getX(pix)
            #cy = cy + getY(pix)
            count = count + 1
    if count != 0:
        cx = cx / count
    else:
        cx = 255
    #cy = cy / count

    print "cx =", cx, ", count =", count

    # Find the "offset" from center
    offset = 127 - cx

    # Calculate motor speeds
    if offset < 0:
        rightMotor = 1.0
        leftMotor = 1.0 - (-offset / 160.0)
    elif offset > 0:
        leftMotor = 1.0
        rightMotor = 1.0 - (offset / 160.0)
    else:
        leftMotor = 1.0
        rightMotor = 1.0

    # For debugging, print the motor speeds
    print "Motors:", leftMotor, ",", rightMotor

    # Act: Set the motor speeds
    motors(leftMotor, rightMotor)
