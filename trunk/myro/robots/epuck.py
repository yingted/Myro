from myro import Robot
from myro.graphics import Picture
import myro.globvars

# this file merges my camera code with spike's EPuck.py code

import serial, time, numpy

#----------------------------------------------------------------------------

# # A complete sample program for the robot
# def goForward(robotID, speed):
#     e = EPuck(robotID)
#     time.sleep(1)
#     while max(e.sensors('distance')) == -1:
#         e.move(speed)
#     e.playSound(5)
#     e.stop()
#     e.close()

class Epuck(Robot):
    # takes in a numerical id and establishes a serial connection
    # to the specified robot.
    def __init__(self, id):
        Robot.__init__(self)
        myro.globvars.robot = self
        self.robotinfo = {"robot": "epuck", "robot-version": "0.1"}
        if isinstance(id, str):
            portname = id
            portnum = int(portname[3:])
            if portnum >= 10:
                portname = r'\\.\COM%d' % (portnum)
        elif isinstance(id, int):
            portname = '/dev/tty.e-puck_%04d-COM1-1' % id
#        self.port = serial.Serial(portname, 38400, timeout=5)
        self.port = serial.Serial(portname, 115200, timeout=5)
        # flushes the communication channel (use write, not send)
        self.port.write('\n')
        self.port.readline()
        while self.port.inWaiting() > 0:
            self.port.read(self.port.inWaiting())
            time.sleep(0.1)
            
        # reset
#         self.send('R')
#         # calibrate sensors
#         self.send('K')
#         # cycles through the LEDs once
#         for n in range(8):
#             self.send('L,%d,1' % n)
#         for n in range(8):
#             self.send('L,%d,0' % n)
        # flash LEDs
        self.clockwiseLEDcycle(0.05)
        self.id = id
        # default camera parameters
        self.setCameraMode('color', 40, 30, 8)
        self.currentTranslate = 0.0
        self.currentRotate = 0.0

    def test(self, s):
        for i in range(4):
            self.forward(s, .5)
            self.turnLeft(s, .3)

    def send(self, msg):
#        print "sending message '%s'" % msg
        self.port.write('%s\n' % msg)
        result = self.port.readline()
        if len(result) == 0 or result[0].upper() != msg[0].upper():
            raise Exception("Bad response: '%s' - check battery" % result.strip())
#         while self.port.inWaiting() > 0:
#             result = self.port.read(self.port.inWaiting())
#             time.sleep(0.1)
#        print "received '%s' with %d bytes left" % (result, self.port.inWaiting())
        return result

    def getCameraMode(self):
        info = self.send('I').split(',')
        if info[1] == '0':
            mode = 'gray'
        else:
            mode = 'color'
        width, height, zoom, bytes = int(info[2]), int(info[3]), int(info[4]), int(info[5])
        if self.id < 1500:
            # pre-June 2008 epucks
            width, height = height, width
        return (mode, width, height, zoom, bytes)

    def setCameraMode(self, mode=None, width=None, height=None, zoom=None):
        if mode is None:
            mode = self.cameraMode
        elif mode not in ('color', 'greyscale', 'grayscale', 'grey', 'gray'):
            raise Exception("Valid modes are 'color' or 'gray'")
        if width is None:
            width = self.cameraWidth
        elif width < 0:
            raise Exception("Bad image width")
        if height is None:
            height = self.cameraHeight
        elif height < 0:
            raise Exception("Bad image height")
        if zoom is None:
            zoom = self.cameraZoom
        elif zoom < 0:
            raise Exception("Bad zoom level")
        if mode == 'color':
            modeNum = 1
        else:
            # gray, grey, grayscale, greyscale
            modeNum = 0
        if self.id < 1500:
            # pre-June 2008 epucks
            self.send('J,%d,%d,%d,%d' % (modeNum, height, width, zoom))
        else:
            self.send('J,%d,%d,%d,%d' % (modeNum, width, height, zoom))
        mode, width, height, zoom, bytes = self.getCameraMode()
        print 'Camera %d set to %dx%d %s (zoom level %d)' % (self.id, width, height, mode, zoom)
        self.cameraMode = mode
        self.cameraWidth = width
        self.cameraHeight = height
        self.cameraZoom = zoom
        self.cameraBytes = bytes

    def takePicture(self, mode=None):
        if mode is None:
            mode = self.cameraMode
        elif mode != self.cameraMode:
            self.setCameraMode(mode)
        self.port.write(chr(183)+chr(0))
        # first 3 bytes: mode, width, height
        # note: if width or height of camera image exceeds 255, then the
        # header width/height values will be wrong.
        dataLength = 3 + self.cameraBytes
        imageData = self.port.read(dataLength)
        if len(imageData) != dataLength:
            raise Exception("Received unexpected amount of camera data (expected %d, got %d)" %
                            (dataLength, len(imageData)))
        #modeNum, w, h = [ord(c) for c in imageData[:3]]
        # ignore header
        data = imageData[3:]
        w = self.cameraWidth
        h = self.cameraHeight
        # width and height values are also reversed for pre-June 2008
        # epucks (ID# < 1500). need to rotate 90 degrees after decoding
        # (see end of method).
        if self.id < 1500:
            w, h = h, w
        picture = Picture()
        if self.cameraMode == 'gray':
            picture.set(w, h, data, 'gray')
        else:
            # color
            buffer = numpy.array([0] * (w * h * 3), 'B')
            j = 0
            for i in xrange(0, len(data), 2):
                high = ord(data[i])   # big endian
                low = ord(data[i+1])
                red = high & 0xF8
                green = ((high & 0x07) << 5) | ((low & 0xE0) >> 3)
                blue = (low & 0x1F) << 3
                buffer[j] = red
                buffer[j+1] = green
                buffer[j+2] = blue
                j += 3
            picture.set(w, h, buffer)
        if self.id < 1500:
            # pre-June 2008 epucks
            picture.rotate(90)
        return picture

    def help(self):
        # use write, not send
        self.port.write('H\n')
        firstline = self.port.readline()
        if len(firstline) == 0:
            print 'Sorry, no help'
        else:
            print firstline.strip()
            while self.port.inWaiting() > 0:
                response = self.port.readline()
                print response.strip()
                time.sleep(0.01)

#     # closes the port connection to the robot
    def close(self):
        print 'Disconnecting from e-puck %d' % self.id
        self.port.close()

    # stops the robot and turns off its LEDs
    def standby(self):
        self.send('S')

    # sends a reset command to the robot
    def reset(self):
        self.send('R')

    ####
    # Movement
    ####

    # stops the robot
    def stop(self):
        self.motors(0, 0)

    # sets the individual motor speeds
    def motors(self, left, right):
        if left < -1: left = -1
        if left > 1: left = 1
        if right < -1: right = -1
        if right > 1: right = 1
        left = int(left * 1000)
        right = int(right * 1000)
        self.send('D,%d,%d' % (left, right))

    # sets the translation speed of the robot,
    # accounting for current rotation speed
    def translate(self, speed):
        cr = self.currentRotate
        self.move(speed, cr)

    # sets the rotation speed of the robot,
    # accounting for current translation speed
    def rotate(self, speed):
        ct = self.currentTranslate
        self.move(ct, speed)

    # combines the effects of translate and rotate, above
    def move(self, translation, rotation = 0.0):
        t = translation
        r = rotation
        at = abs(translation)
        ar = abs(rotation)
        if (at + ar) > 1.0:
            ratio = 1.0 / (at + ar)
            t = translation * ratio
            r = rotation * ratio
        self.currentTranslate = t
        self.currentRotate = r
        self.motors(t-r, t+r)

    #####
    # Sensors
    #####

    # a general selector function which gives access to all available sensors
    # all sensor values are return as a list of values scaled to 0 to +1
    # the values are ordered clockwise around the perimeter from the forward-most,
    # right-most sensor (i.e. the front right microphone is robot.sensors('sound')[0])
    def sensors(self, sensor):
        if sensor == 'light':
            return self.lightSensors()
        elif sensor == 'accelerometer':
            return accelSensors()
        elif sensor == 'sound':
            return self.soundSensors()
        elif sensor == 'distance':
            return self.distanceSensors()
        else:
            return "invalid sensor specification"

    ## front-right wrap-around to front-left
    def lightSensors(self):
        vals = [float(v) for v in self.send('O').strip().split(',')[1:]]
        return vals
#         s = s[1:]
#         for i in range(8):
#             s[i] = int(s[i])
#             s[i] = (4000 - s[i])
#             s[i] = s[i]/4000.0
#         return s

    ## looking down from top with camera at north...
    # front right sensor is #0
    # sensors proceed clockwise
    # sensor#  angle of offset clockwise from north (counterclockwise)
    # 0        20     (-340)
    # 1        50     (-310)
    # 2        90     (-270)
    # 3        150    (-210)
    # 4        210    (-150)
    # 5        270    (-90)
    # 6        310    (-50)
    # 7        340    (-20)
    def distanceSensors(self):
        vals = [float(v) for v in self.send('N').strip().split(',')[1:]]
        return vals

    def getIR(self):
        return self.distanceSensors()

    def getIRAngle(self, sensorNum):
        angles = [20, 50, 90, 150, 210, 270, 310, 340]
        return angles[sensorNum]

    # wheel encoders self.send('Q')
    # goes from 0 up to 32767, then wraps to negatives values back up to 0...

    ## returns values as x y z
    def accelSensors(self):
        s = self.send('A')[0].strip().split(',')
        s = s[1:]
        ar = []
        for i in range(3):
            ar.append(int(s[i]))
        return ar
            
    ## right, left, rear
    def soundSensors(self):
        s = self.send('U')[0].strip().split(',')
        s = s[1:]
        ar = []
        for i in range(3):
            ar.append(int(s[i]))
        return ar        
        

    #####
    # LEDs
    #####

    def frontLedOn(self):
        self.send('F, 1')
        
    def frontLedOff(self):
        self.send('F, 0')
        
    def frontLedToggle(self):
        self.send('F, 2')

    def bodyLedOn(self):
        self.send('B, 1')
        
    def bodyLedOff(self):
        self.send('B, 0')
        
    def bodyLedToggle(self):
        self.send('B, 2')

    def allOn(self):
        self.send('L,8,1')

    def allOff(self):
        self.send('L,8,0')

    def ledsOff(self, which=range(8)):
        for n in which:
            self.send('L,%d,0' % n)
            #time.sleep(0.01)

    def ledsOn(self, which=range(8)):
        for n in which:
            self.send('L,%d,1' % n)
            #time.sleep(0.01)

    def ledsToggle(self, which=range(8)):
        for n in which:
            self.send('L,%d,2' % n)
            #time.sleep(0.01)

    #####
    ## Amusing Behaviors
    #####

    # plays one of the preset E-Puck sounds: 5 options, chosen by setting num to the appropriate value.
    def playSound(self, num):
        s = self.send('T, %d' % num)
        return s
    
    # cycles each of the LEDs on and off continuously, for a delay specified by the input.
    def clockwiseLEDcycle(self, delay=0.1):
        for n in range(8):
            self.send('L,%d,1' % n)
            time.sleep(delay)
            #self.send('L,%d,0' % n)
        time.sleep(delay)
        self.allOff()

    # causes the E-Puck to approximate a very crude figure eight.
    def figureEight(self):
        self.currentRotation = 0.3
        delay = 4.0
        for i in range(10):
            self.currentRotation = self.currentRotation*-1
            self.move(0.3, self.currentRotation)
            time.sleep(delay)
            self.ledsToggle()

    # causes the e-puck to spiral in gradually expanding circles.                        
    def spiral(self):
        adjust = 0
        try:
            while(True):
                self.spiralStep(adjust)
                adjust += 0.01
        except:
            KeyboardInterrupt
            self.move(0,0)
            self.ledsOff()
            
    # makes the e-puck cry for a random length of time, ten times.
    def cry(self):
        for i in range(10):
            self.playSound(5)
            time.sleep(1)
            
    # helper function for "spiral, above"
    def spiralStep(self, adjust):
        self.move(1, 1.0-adjust)
        self.spiralLeds(0.1)

    # cycles the LEDs once, leaving each light on for a length of delay seconds
    def spiralLeds(self, delay):
        for n in range(8):
            self.send('L,%d,1' % n)
            time.sleep(delay)
            self.send('L,%d,0' % n)

    # provides a simple, moderately glitchy wall-avoiding function.
    def simpleAvoid(self):
        while True:
            sensArray = self.sensors('distance')
            rot = 0.0
            tra = 0.2
            if (sensArray[0] < 0.9 and sensArray[0] != -1) \
               or (sensArray[1] < 0.9 and sensArray[1] != -1):
                rot += 0.5
            if (sensArray[7] < 0.9 and sensArray[7] != -1) \
               or (sensArray[6] < 0.9 and sensArray[6] != -1):
                rot -= 0.5
            if -1 < sensArray[0] < 0.5 and -1 < sensArray[7] < 0.5:
                tra = 0.0
                rot = 1
            self.move(tra, rot)

    def rawDistances(self):
        raw = [int(x) for x in self.send('n')[0].strip().split(',')[1:]]
        if len(raw) == 0: return [0]
        return raw

    def calibrate(self):
        self.send('K')
        while True:
            result = self.port.readlines()
            if len(result) > 0: break
        print 'done'

    def sound(self, num):
        self.playSound(num)
        delay = 0.1
        if num == 5: delay = 1
        time.sleep(delay)
        self.playSound(0)

    def respondToSensor(self, sensor, distance):
        actions = { 0: (-1, -1),
                    1: (-1, -0.3),
                    2: (-0.8, 0.8),
                    3: (0.5, 1),
                    4: (1, 0.5),
                    5: (0.8, -0.8),
                    6: (-0.3, -1),
                    7: (-1, -1)
                    }
        if sensor in actions:
            (left, right) = actions[sensor]
            #factor = min(1.0, distance / 200.0)
            factor = 0.5
            left = factor * left
            right = factor * right
            return (left, right)
        else:
            return None

    def argmax(self, vals):
        assert len(vals) > 0, 'list is empty'
        m = max(vals)
        return vals.index(m)

    # possible LEDs: 0, 1, 2, 3, 5, 6, 7
    def whichLED(self, sensorIndex):
        leds = { 0: 0, 1: 1, 2: 2, 3: 3, 4: 5, 5: 6, 6: 7, 7: 0 }
        return leds[sensorIndex]

    def manual_flush(self):
        pass

    def hardStop(self):
        self.stop()


"""
To extract the color components from an RGB 565 image, treat each
pixel as a WORD type and use the following bit masks:

WORD red_mask = 0xF800;
WORD green_mask = 0x7E0;
WORD blue_mask = 0x1F;

Get the color components from a pixel as follows:

BYTE red_value = (pixel & red_mask) >> 11;
BYTE green_value = (pixel & green_mask) >> 5;
BYTE blue_value = (pixel & blue_mask);

Remember that the red and blue channels are 5 bits and the green
channel is 6 bits. To convert these values to 8-bit components (for
24-bit or 32-bit RGB), you must left-shift the appropriate number of
bits:

// Expand to 8-bit values.
BYTE red   = red_value << 3;
BYTE green = green_value << 2;
BYTE blue  = blue_value << 3;

Reverse this process to create an RGB 565 pixel.  Assuming the color
values have been truncated to the correct number of bits:

WORD pixel565 = (red_value << 11) | (green_value << 5) | blue_value;

"""
#----------------------------------------------------------------------------

# class RobotBrain():

#     def __init__(self, id):
#         self.robot = EPuck(id)
#         self.behaviors = []
#         # add behaviors, highest priorities first:
#         self.addBehavior(AvoidBehavior())
#         #self.addBehavior(WanderBehavior())
#         #self.addBehavior(RestBehavior())

#     def addBehavior(self, behavior):
#         # give the behavior access to the robot object
#         behavior.robot = self.robot
#         self.behaviors.append(behavior)

#     def step(self):
#         behavior = self.updateAll()
#         #print "%s is in control" % behavior
#         self.robot.move(behavior.speed, behavior.rotate)
#         time.sleep(0.5)

#     def updateAll(self):
#         # update all behaviors in order of priority
#         for behavior in self.behaviors:
#             behavior.flag = False
#             behavior.update()
#             # if the behavior fired, return it immediately
#             if behavior.flag:
#                 return behavior
#         # if none fired, return the last (lowest-priority) behavior:
#         return self.behaviors[-1]

# #-----------------------------------------------------------------------
# # base class for behaviors

# class SubsumptionBehavior:

#     def __init__(self):
#         self.speed = 0
#         self.rotate = 0
#         self.flag = False
#         # this will be set later when the behavior is created
#         self.robot = None

#     def __repr__(self):
#         return self.__class__.__name__

#     def requestMove(self, speed, rotate):
#         self.speed = speed
#         self.rotate = rotate
#         self.flag = True

# #-----------------------------------------------------------------------

# class AvoidBehavior(SubsumptionBehavior):
#     def update(self):
#         readings = self.robot.sensors('distance')
#         distance = max(readings)
#         if distance > 0:
#             n = self.argmax(readings)
#             led = self.whichLED(n)
#             self.send('L,%d,1' % led)
#             #(speed, rotate) = self.chooseAction(n, distance)
#             #self.requestMove(speed, rotate)
#         else:
#             print 'nothing nearby'

#     def whichLED(self, sensorIndex):
#         if sensorIndex == 7:
#             return 0
#         else:
#             return sensorIndex

#     def chooseAction(self, sonarNum, scaledDistance):
#         actions = { 0: (1,-1), 15: (1,-1),
#                     7: (1,+1), 8: (1,+1),
#                     1: (0.5,-1), 6: (0.5,+1),
#                     2: (0.1,-1), 5: (0.1,+1),
#                     3: (-1,+0.3), 4: (-1,-0.3),
#                     14: (1,-1), 9: (1,+1),
#                     13: (1,-0.5), 10: (1,+0.5),
#                     12: (1,-0.3), 11: (1,+0.3)
#                     }
#         print 'responding to sonar %d' % sonarNum
#         if sonarNum in actions:
#             (s, r) = actions[sonarNum]
#             factor = 1 - scaledDistance
#             return (s * factor, r * factor)
#         else:
#             return (0, 0)

#     def argmax(self, vals):
#         assert len(vals) > 0, 'list is empty'
#         m = max(vals)
#         return vals.index(m)

# class RestBehavior(SubsumptionBehavior):
#     def update(self):
#         self.requestMove(0, 0)

# class WanderBehavior(SubsumptionBehavior):
#     def update(self):
#         self.requestMove(0.2, random.uniform(-0.5, 0.5))

# #-----------------------------------------------------------------------

# def INIT(engine):
#     return SubsumptionBrain('Subsumption Brain', engine)
