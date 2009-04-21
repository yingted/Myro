import myro
from myro import Robot
import myro.globvars
import time, platform, serial, array

#-----------------------------------------------------------------------------
# this function maps epuck ID numbers to port names, and is system-specific

def portname(id):
    if platform.system() == 'Darwin':  # Mac OS X
        assert type(id) is int and id > 0, 'Bad epuck ID: %s' % (id,)
        # arrrrrrggghhh!!!!!
        if id == 1781:
            return '/dev/tty.1781-COM1-1'
        else:
            return '/dev/tty.e-puck_%04d-COM1-1' % id
    elif platform.system() == 'Linux':
        assert type(id) is int and id > 0, 'Bad epuck ID: %s' % (id,)
        # see /etc/bluetooth/rfcomm.conf
        rfcommPortNumber = {27: 0, 1197: 0, 1198: 1, 1190: 2,
                            1559: 4, 1602: 5, 1603: 6,
                            1604: 7, 1770: 8, 1781: 9}
        if id in rfcommPortNumber:
            return '/dev/rfcomm%d' % rfcommPortNumber[id]
        else:
            print 'Warning: unknown epuck ID -- using /dev/rfcomm0'
            return '/dev/rfcomm0'
    elif platform.system() == 'Windows':
        assert type(id) is str, 'Bad port name: %s' % (id,)  # example: "COM27"
        portname = id
        portnum = int(portname[3:])
        if portnum >= 10:
            portname = r'\\.\COM%d' % portnum
        return portname

#-----------------------------------------------------------------------------

class Picture(myro.graphics.Picture):
    def __init__(self, id):
        myro.graphics.Picture.__init__(self)
        self.id = id
    def show(self):
        myro.show(self, 'Epuck %d' % self.id)
        if myro.globvars.runtkthread == 0:
            myro.updateGraphics()
    def repaint(self):
        myro.show(self, 'Epuck %d' % self.id)
        if myro.globvars.runtkthread == 0:
            myro.updateGraphics()
    def getWidth(self):
        return self.width
    def getHeight(self):
        return self.height

class Pixel(myro.graphics.Pixel):
    def __init__(self, x, y, picture):
        myro.graphics.Pixel.__init__(self, x, y, picture)
    def getX():
        return self.x
    def getY():
        return self.y

class Epuck(myro.Robot):

    sensorGroups = {'left': 5, 'right': 2, 'center': (0, 7),
                    'front': (0, 7), 'front-left': 6, 'front-right': 1,
                    'back': (3, 4), 'back-left': 4, 'back-right': 3}

    # establishes a serial connection to the specified robot
    def __init__(self, id):
        myro.Robot.__init__(self)
        myro.globvars.robot = self
        self.robotinfo = {'robot': 'epuck', 'robot-version': '0.1'}
        self.portname = portname(id)
        self.port = serial.Serial(self.portname, 115200, timeout=1)
        self.id = id
        # initialize communication (use write, not send)
        self.port.write('\n')
        self._clearLines()
        self.setCameraMode('color', 40, 40, 8)
        self._lastTranslate = 0
        self._lastRotate = 0
        # flash LEDs
        self.onCycleLED(0.05)
        self.offLED('all')

    def reset(self):
        cmd = raw_input("Press blue reset button on robot, then press RETURN...")
        if cmd != '':
            print 'Aborted'
            return
        self._clearLines()
        self._lastTranslate = 0
        self._lastRotate = 0
        self.setCameraMode('color', 40, 40, 8)
        print 'Robot ready'

    def send(self, msg):
        assert msg[0] not in 'HKRVhkrv', "command '%s' not allowed with send" % msg[0]
        self.port.write('%s\n' % msg)
        response = self.port.readline()
        if response == '' or response[0].upper() != msg[0].upper():
            print "Bad response: '%s' -- check battery" % response.strip()
            self.reset()
        else:
            return response

    def version(self):
        self.port.write('V\n')
        self._printLines()

    def help(self):
        self.port.write('H\n')
        self._printLines()

    def _printLines(self):
        time.sleep(0.05)
        response = self.port.readline()
        while response != '':
            print response.strip()
            time.sleep(0.05)
            response = self.port.readline()

    # flushes communication channel
    def _clearLines(self):
        time.sleep(0.05)
        response = self.port.readline()
        while response != '':
            time.sleep(0.05)
            response = self.port.readline()

    def calibrateSensors(self):
        cmd = raw_input('Remove all objects in sensor range, then press RETURN...')
        if cmd != '':
            print 'Aborted'
            return
        print 'Calibrating sensors...'
        self.port.write('K\n')
        self.port.readline()
        time.sleep(2)
        while self.port.readline().strip() != 'k, Calibration finished':
            time.sleep(0.05)
        print 'Calibration finished'

    # closes the port connection to the robot
    def close(self):
        if self.port.isOpen():
            print 'Disconnecting from e-puck %d' % self.id
            self.port.close()

    def manual_flush(self):
        print '\nInterrupted...please wait'
        self._clearLines()

    def hardStop(self):
        self.send('S')
        self.send('D,0,0')  # reset internal motor speed info
        self.send('T,0')
        self._lastTranslate = 0
        self._lastRotate = 0

    # temporary. necessary to avoid crashes with clicking in
    # a Picture window. need to fix.
    def set_blob_yuv(self, *ignore1, **ignore2):
        return (0, 0, 0, 0, 0, 0)

    #----------------------------------------------------------------------
    # camera

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
        return [mode, width, height, zoom, bytes]

    def setCameraMode(self, mode=None, width=None, height=None, zoom=None):
        if mode == None:
            mode = self.cameraMode
        elif mode not in ('color', 'greyscale', 'grayscale', 'grey', 'gray'):
            raise Exception("Valid modes are 'color' or 'gray'")
        if width == None:
            width = self.cameraWidth
        elif width < 0:
            raise Exception("Bad image width")
        if height == None:
            height = self.cameraHeight
        elif height < 0:
            raise Exception("Bad image height")
        if zoom == None:
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
        if mode == None:
            mode = self.cameraMode
        elif mode != self.cameraMode:
            self.setCameraMode(mode)
        self.port.write(chr(183)+chr(0))
        # first 3 bytes: mode, width, height
        # note: if width or height of camera image exceeds 255, then the
        # header width/height values will be wrong.
        dataLength = 3 + self.cameraBytes
        time.sleep(0.2)
        imageData = self.port.read(dataLength)
        if len(imageData) != dataLength:
            print "Expected %d bytes from camera, got %d -- check battery" % \
                (dataLength, len(imageData))
            self.reset()
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
        picture = Picture(self.id)
        if self.cameraMode == 'gray':
            picture.set(w, h, data, 'gray')
        else:
            # color
            buffer = array.array('B', [0] * (w * h * 3))
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

    # To extract the color components from an RGB 565 image, treat each
    # pixel as a WORD type and use the following bit masks:
    # 
    # WORD red_mask = 0xF800;
    # WORD green_mask = 0x7E0;
    # WORD blue_mask = 0x1F;
    # 
    # Get the color components from a pixel as follows:
    # 
    # BYTE red_value = (pixel & red_mask) >> 11;
    # BYTE green_value = (pixel & green_mask) >> 5;
    # BYTE blue_value = (pixel & blue_mask);
    # 
    # Remember that the red and blue channels are 5 bits and the green
    # channel is 6 bits. To convert these values to 8-bit components (for
    # 24-bit or 32-bit RGB), you must left-shift the appropriate number of
    # bits:
    # 
    # // Expand to 8-bit values.
    # BYTE red   = red_value << 3;
    # BYTE green = green_value << 2;
    # BYTE blue  = blue_value << 3;
    # 
    # Reverse this process to create an RGB 565 pixel.  Assuming the color
    # values have been truncated to the correct number of bits:
    # 
    # WORD pixel565 = (red_value << 11) | (green_value << 5) | blue_value;

    #----------------------------------------------------------------------
    # movement

    def move(self, translate, rotate, seconds=None):
        assert -1 <= translate <= 1, 'move called with bad translate value: %g' % translate
        assert -1 <= rotate <= 1, 'move called with bad rotate value: %g' % rotate
        self._adjustSpeed(translate, rotate)
        if seconds is not None:
            time.sleep(seconds)
            self.stop()

    def translate(self, speed, seconds=None):
        assert -1 <= speed <= 1, 'translate called with bad value: %g' % speed
        self._adjustSpeed(speed, self._lastRotate)
        if seconds is not None:
            time.sleep(seconds)
            self.stop()

    def rotate(self, speed, seconds=None):
        assert -1 <= speed <= 1, 'rotate called with bad value: %g' % speed
        self._adjustSpeed(self._lastTranslate, speed)
        if seconds is not None:
            time.sleep(seconds)
            self.stop()

    def stop(self):
        self._adjustSpeed(0, 0)

    def forward(self, speed, seconds=None):
        assert 0 <= speed <= 1, 'forward called with bad value: %g' % speed
        self._adjustSpeed(speed, self._lastRotate)
        if seconds is not None:
            time.sleep(seconds)
            self.stop()

    def backward(self, speed, seconds=None):
        assert 0 <= speed <= 1, 'backward called with bad value: %g' % speed
        self._adjustSpeed(-speed, self._lastRotate)
        if seconds is not None:
            time.sleep(seconds)
            self.stop()

    def turnLeft(self, speed, seconds=None):
        assert 0 <= speed <= 1, 'turnLeft called with bad value: %g' % speed
        self._adjustSpeed(self._lastTranslate, speed)
        if seconds is not None:
            time.sleep(seconds)
            self.stop()

    def turnRight(self, speed, seconds=None):
        assert 0 <= speed <= 1, 'turnRight called with bad value: %g' % speed
        self._adjustSpeed(self._lastTranslate, -speed)
        if seconds is not None:
            time.sleep(seconds)
            self.stop()

    def motors(self, left, right):
        assert -1 <= left <= 1, 'motors called with bad left value: %g' % left
        assert -1 <= right <= 1, 'motors called with bad right value: %g' % right
        translate = (right + left) / 2.0
        rotate = (right - left) / 2.0
        self._adjustSpeed(translate, rotate)

    def getSpeed(self):
        return (self._lastTranslate, self._lastRotate)

    def getTranslate(self):
        return self._lastTranslate

    def getRotate(self):
        return self._lastRotate

    def getMotors(self):
        left, right = [int(x) for x in self.send('E').strip().split(',')[1:]]
        return [left/1000.0, right/1000.0]

    def _adjustSpeed(self, translate, rotate):
        self._lastTranslate = translate
        self._lastRotate = rotate
        left = int(max(-1, min(1, translate - rotate)) * 1000)
        right = int(max(-1, min(1, translate + rotate)) * 1000)
        self.send('D,%d,%d' % (left, right))

    #----------------------------------------------------------------------
    # external sensors

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
    def getIRAngle(self, position):
        assert type(position) == int and 0 <= position <= 7, \
            'Bad position value: %s' % position
        angles = [20, 50, 90, 150, 210, 270, 310, 340]
        return angles[position]

    def getLight(self, position=None):
        return self._readIR('O', position)

    def getProximity(self, position=None):
        return self._readIR('N', position)

    def getDistance(self, position=None):
        return self._readIR('N', position)

    def getIR(self, position=None):
        return self._readIR('N', position)

    def getAmbientLight(self):
        vals = self.getLight()
        return sum(vals) / len(vals)

    def getBright(self, position=None):
        pass

    def getObstacle(self, position=None):
        pass

    def _readIR(self, command, position):
        assert position == None \
            or type(position) == int and 0 <= position <= 7 \
            or type(position) == str and position in Epuck.sensorGroups, \
            'Bad position value: %s' % position
        vals = [max(0, min(4000, int(x))) for x in self.send(command).strip().split(',')[1:]]
        if position == None:
            return vals
        elif type(position) == int:
            return vals[position]
        else:
            group = Epuck.sensorGroups[position]
            if type(group) == int:
                return vals[group]
            else:
                # return average value for group
                groupVals = [vals[i] for i in group]
                return sum(groupVals) / len(groupVals)

    ## return accelerometer readings as [x, y, z]
    def getAccel(self):
        vals = [int(x) for x in self.send('A').strip().split(',')[1:]]
        return vals
            
    # values go from 0 to 65535, then wrap around to 0
    def getWheels(self):
        left, right = [int(x) for x in self.send('Q').strip().split(',')[1:]]
        return [left % 65536, right % 65536]

    def resetWheels(self):
        self.send('P,0,0')

    def getSelector(self):
        position = int(self.send('C').strip().split(',')[1])
        return position

    # a general selector function which gives access to all available sensors
    def getSensors(self, sensor):
        assert sensor in ('light', 'proximity', 'distance',
                          'accelerometer', 'sound', 'wheels'), \
            'Bad sensor specification: %s' % sensor
        if sensor == 'light':
            return self.getLight()
        elif sensor == 'proximity' or sensor == 'distance':
            return self.getProximity()
        elif sensor == 'accelerometer':
            return getAccel()
        elif sensor == 'sound':
            return self.getSound()
        elif sensor == 'wheels':
            return self.getWheels()

    ## return microphone readings as [front-right, front-left, rear]
    def getSound(self):
        vals = [int(x) for x in self.send('U').strip().split(',')[1:]]
        return vals
        
    def playSound(self, num):
        assert 0 <= num <= 5, 'Bad sound number: %s' % num
        self.send('T,%d' % num)
        if num > 0:
            waitTime = {1: 0.25, 2: 0.25, 3: 0.40, 4: 0.45, 5: 1.15}
            time.sleep(waitTime[num])
            self.send('T,0')
    
    def shush(self):
        self.send('T,0')

    def currentTime(self):
        return time.time()

    def getStall(self):
        assert False, 'getStall not implemented for epuck'

    def getBattery(self):
        assert False, 'getBattery not implemented for epuck'

    # returns the index number of the LED closest to the given IR sensor
    def getLEDnumber(self, sensorNum):
        LEDs = [0, 1, 2, 3, 5, 6, 7, 0]
        return LEDs[sensorNum]

    def onLED(self, LED_descriptor='all'):
        assert type(LED_descriptor) is str \
            and LED_descriptor in ('all', 'front', 'body') \
            or type(LED_descriptor) is int and 0 <= LED_descriptor <= 7, \
            'Bad LED descriptor: %s' % (LED_descriptor,)
        self._onLED(LED_descriptor)

    def _onLED(self, LED_descriptor):
        if LED_descriptor == 'all':
            self.send('L,8,1')
        elif LED_descriptor == 'front':
            self.send('F,1')
        elif LED_descriptor == 'body':
            self.send('B,1')
        else:
            self.send('L,%d,1' % LED_descriptor)

    def offLED(self, LED_descriptor='all'):
        assert type(LED_descriptor) is str \
            and LED_descriptor in ('all', 'front', 'body') \
            or type(LED_descriptor) is int and 0 <= LED_descriptor <= 7, \
            'Bad LED descriptor: %s' % (LED_descriptor,)
        self._offLED(LED_descriptor)

    def _offLED(self, LED_descriptor):
        if LED_descriptor == 'all':
            self.send('L,8,0')
        elif LED_descriptor == 'front':
            self.send('F,0')
        elif LED_descriptor == 'body':
            self.send('B,0')
        else:
            self.send('L,%d,0' % LED_descriptor)

    def toggleLED(self, LED_descriptor='all'):
        assert type(LED_descriptor) is str \
            and LED_descriptor in ('all', 'front', 'body') \
            or type(LED_descriptor) is int and 0 <= LED_descriptor <= 7, \
            'Bad LED descriptor: %s' % (LED_descriptor,)
        self._toggleLED(LED_descriptor)

    def _toggleLED(self, LED_descriptor):
        if LED_descriptor == 'all':
            # self.send('L,8,2') doesn't work
            for num in (0, 4, 3, 5, 1, 7, 2, 6):
                self.send('L,%d,2' % num)
        elif LED_descriptor == 'front':
            self.send('F,2')
        elif LED_descriptor == 'body':
            self.send('B,2')
        else:
            self.send('L,%d,2' % LED_descriptor)

    def flashLED(self, LED_descriptor='all', delay=0):
        assert type(LED_descriptor) is str \
            and LED_descriptor in ('all', 'front', 'body') \
            or type(LED_descriptor) is int and 0 <= LED_descriptor <= 7, \
            'Bad LED descriptor: %s' % (LED_descriptor,)
        self._flashLED(LED_descriptor, delay)

    def _flashLED(self, LED_descriptor, delay):
        if LED_descriptor == 'all':
            # don't use toggleLED('all') -- too slow
            self._onLED('all')
            time.sleep(delay)
            self._offLED('all')
        elif LED_descriptor == 'front':
            self._toggleLED('front')
            time.sleep(delay)
            self._toggleLED('front')
        elif LED_descriptor == 'body':
            self._toggleLED('body')
            time.sleep(delay)
            self._toggleLED('body')
        else:
            self._toggleLED(LED_descriptor)
            time.sleep(delay)
            self._toggleLED(LED_descriptor)

    def onCycleLED(self, delay=0):
        for num in range(8):
            self._onLED(num)
            time.sleep(delay)
        
    def offCycleLED(self, delay=0):
        for num in range(8):
            self._offLED(num)
            time.sleep(delay)

    def toggleCycleLED(self, delay=0):
        for num in range(8):
            self._toggleLED(num)
            time.sleep(delay)

    def flashCycleLED(self, delay=0):
        for num in range(8) + [0]:
            self._flashLED(num, delay)


#     def onSpotlightLED(self):
#         self.send('F,1')
        
#     def offSpotlightLED(self):
#         self.send('F,0')
        
#     def toggleSpotlightLED(self):
#         self.send('F,2')

#     def flashSpotlightLED(self, delay=0):
#         self.toggleSpotlightLED()
#         time.sleep(delay)
#         self.toggleSpotlightLED()

#     def onBodyLED(self):
#         self.send('B,1')
        
#     def offBodyLED(self):
#         self.send('B,0')
        
#     def toggleBodyLED(self):
#         self.send('B,2')

#     def flashBodyLED(self, delay=0):
#         self.toggleBodyLED()
#         time.sleep(delay)
#         self.toggleBodyLED()

#     def onLED(self, num):
#         assert 0 <= num <= 7, 'Bad LED number: %s' % num
#         self.send('L,%d,1' % num)

#     def offLED(self, num):
#         assert 0 <= num <= 7, 'Bad LED number: %s' % num
#         self.send('L,%d,0' % num)

#     def toggleLED(self, num):
#         assert 0 <= num <= 7, 'Bad LED number: %s' % num
#         self.send('L,%d,2' % num)

#     def flashLED(self, num, delay=0):
#         self.toggleLED(num)
#         time.sleep(delay)
#         self.toggleLED(num)

#     def onAllLEDs(self):
#         self.send('L,8,1')

#     def offAllLEDs(self):
#         self.send('L,8,0')

#     def toggleAllLEDs(self):
#         # self.send('L,8,2') doesn't work
#         for num in (0, 4, 3, 5, 1, 7, 2, 6):
#             self.toggleLED(num)

#     def flashAllLEDs(self, delay=0):
#         # don't use toggleAllLEDs (too slow)
#         self.onAllLEDs()
#         time.sleep(delay)
#         self.offAllLEDs()

#     # turns on the LEDs clockwise
#     def onCycleLEDs(self, delay=0):
#         for num in range(8):
#             self.onLED(num)
#             time.sleep(delay)

#     # turns off the LEDs clockwise
#     def offCycleLEDs(self, delay=0):
#         for num in range(8):
#             self.offLED(num)
#             time.sleep(delay)

#     # toggles the LEDs clockwise
#     def toggleCycleLEDs(self, delay=0):
#         for num in range(8):
#             self.toggleLED(num)
#             time.sleep(delay)

#     # flashes the LEDs clockwise
#     def flashCycleLEDs(self, delay=0):
#         for num in range(8):
#             self.flashLED(num, delay)

#-----------------------------------------------------------------------------
# procedural interface

def move(translate, rotate, seconds=None):
    myro.globvars.robot.move(translate, rotate, seconds)

def translate(speed, seconds=None):
    myro.globvars.robot.translate(speed, seconds)

def rotate(speed, seconds=None):
    myro.globvars.robot.rotate(speed, seconds)

def playSound(num):
    myro.globvars.robot.playSound(num)

def onLED(LED_descriptor='all'):
    myro.globvars.robot.onLED(LED_descriptor)

def offLED(LED_descriptor='all'):
    myro.globvars.robot.offLED(LED_descriptor)

def toggleLED(LED_descriptor='all'):
    myro.globvars.robot.toggleLED(LED_descriptor)

def flashLED(LED_descriptor='all', delay=0):
    myro.globvars.robot.flashLED(LED_descriptor, delay)

def onCycleLED(delay=0):
    myro.globvars.robot.onCycleLED(delay)

def offCycleLED(delay=0):
    myro.globvars.robot.offCycleLED(delay)

def toggleCycleLED(delay=0):
    myro.globvars.robot.toggleCycleLED(delay)

def flashCycleLED(delay=0):
    myro.globvars.robot.flashCycleLED(delay)
