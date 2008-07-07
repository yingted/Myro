from myro import *

print "Welcome, this program sings..."

darkenCamera()

song = "a a 2/9; a a 2/9; a a 13/9; REST REST 1/9; a a 2/9; b b 2/9; c6 c6 2/9; e6 c6 1/3; d6 f 2"
beeps = makeSong(song)

# Enter an endless loop.
# In this loop, we will read some sensor values, decide what to do, then do it!
# The loop means we'll keep repeating this cycle over and over again, quickly.
# Known as "sense, think, act"
while(True):
    
    # Sense: Read the image
    blobImg = takePicture("gray")

    show(blobImg)

    # Think: Perform a calculation on the sensor values to determine the
    # desired action.

    # Find the blob count
    count = 0
    for pix in getPixels(blobImg):
        if getRed(pix) > 40 or getBlue(pix) > 40 or getGreen(pix) > 40:
            count = count + 1
    if count != 0:
        sing = 1
    else:
        sing = 0
    #sing = 1

    # Sing
    if sing == 1:
        for b in beeps:
            tone1, tone2, dur = b
            beep(dur, tone1, tone2)

