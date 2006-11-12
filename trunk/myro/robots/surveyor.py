"""
Surveyor SVR-1 Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Doug"

import serial, time, string
from myro import Robot, ask
import myro.globals

def ascii(vec):
    retval = ""
    for v in vec:
        retval += "0123456789#"[int(v * 10)]
    return retval

def dec2bin(num, bits = 8):
    neg = 0
    if num < 0: # two's complement
        num = (num * -1) - 1
        neg = 1
    s='0' * bits
    t={'0':'000','1':'001','2':'010','3':'011',
       '4':'100','5':'101','6':'110','7':'111'}
    for c in oct(num)[1:]:
        s+=t[c]
    retval = s[-bits:]
    if neg: # two's complement
        retval = complement(retval)
        return '1' + retval[1:]
    return retval

def complement(binNum):
    retval = ''
    for bit in binNum:
        retval += str(int(not int(bit)))
    return retval

def bin2dec(binNum):
    retval = 0
    pos = len(binNum) - 1
    for bit in binNum:
        retval += int(bit) * (2 ** pos)
        pos -= 1
    return retval

def encode(num):
    if num >= 0: return chr(num)
    else:        return chr(bin2dec(dec2bin(num)))

def isTrue(value):
    """
    Returns True if value is something we consider to be "on".
    Otherwise, return False.
    """
    if type(value) == str:
        return (value.lower() == "on")
    elif value: return True
    return False

class Surveyor(Robot):
    def __init__(self, serialport = None, baudrate = 115200):
        Robot.__init__(self)
        self.debug = 0
        self._lastTranslate = 0
        self._lastRotate    = 0
        self._volume = 0
        if serialport == None:
            serialport = ask("Port", useCache = 1)
	# Deal with requirement that Windows "COM#" names where # >= 9 needs to
	# be in the format "\\.\COM#"
	if type(serialport) == str and serialport.startswith("com"):
	    portnum = int(serialport[3:])
	    if portnum >= 9:
	        serialport = r'\\.\COM%d' % (portnum + 1)
        self.serialPort = serialport
        self.baudRate = baudrate
        self.id = None
        self.open()
        self.restart()
        myro.globals.robot = self

    def open(self):
        if myro.globals.robot != None:
            self.ser = myro.globals.robot.ser
            self.ser.open()
        else:
            while 1:
                try:
                    self.ser = serial.Serial(self.serialPort, timeout = 2) # does this need to be 10?
                    break
                except:
                    print "Waiting on port..."
                    time.sleep(1)
            self.ser.baudrate = self.baudRate
        self.ser.flushOutput()
        self.ser.flushInput()
        self.stop() 
        self._send("F") # turn off failsafe
        self._send("m") # disable automovement, samples ground for scans
        self.setCameraResolution((160,128)) # this resolution allows scans

    def close(self):
        self.ser.close()

    def restart(self):
        self.stop()
        print "Hello, I'm Surveyor!"

    def get(self, sensor = "all", *position):
        sensor = sensor.lower()
        if sensor == "name":
            return self.name
        elif sensor == "version":
            return self.getVersion()
        elif sensor == "cameraresolution":
            return self.cameraResolution
        else:
            retvals = []
            if len(position) == 0:
                if sensor == "scan":
                    return self._send("S")
                elif sensor == "cameraimage":
                    return self._send("I")
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if sensor == "scan":
                data = self._send("S")
                for pos in position:
                    retvals.append(data[pos])
            else:
                raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def set(self, item, position, value = None):
        item = item.lower()
        if item == "name":
            name = position[:8].strip()
            self.name = name
            return "ok"
        elif item == "cameraresolution":
            self.setCameraResolution(position)
            return "ok"
        else:
            raise ("invalid set item name: '%s'" % item)

    def stop(self):
        self._lastTranslate = 0
        self._lastRotate = 0
        self._send("5") # stop; faster than move(0,0)

    def translate(self, amount):
        self._lastTranslate = amount
        self._adjustSpeed()

    def rotate(self, amount):
        self._lastRotate = amount
        self._adjustSpeed()

    def move(self, translate, rotate):
        self._lastTranslate = translate
        self._lastRotate = rotate
        self._adjustSpeed()

    def update(self):
        pass

    def getVersion(self):
        return self._send("V")

    def setCameraResolution(self, mode = (80, 64)): 
        if mode == (80, 64):
            self._send("a")
        elif mode == (160,128):
            self._send("b")
        elif mode == (320,240):
            self._send("c")
        else:
            raise AttributeError, ("invalid camera resolution:" + mode)
        self.cameraResolution = mode
        return "ok"

    def setSwarmMode(self, mode):
        if mode:
            self._send("r") # swarm mode
        else:
            self._send("m") # reset ground, non-autonomous movement
        self.swarmMode = mode

    def sampleGroundColor(self):
        self._send("m") # samples background for obstacle detection
        return "ok"

    def getScan(self):
        return self._send("S")

    def getCameraImage(self):
        return self._send("I")

####################### Private

    def _adjustSpeed(self):
        left  = min(max(self._lastTranslate - self._lastRotate, -1), 1)
        right  = min(max(self._lastTranslate + self._lastRotate, -1), 1)
        leftPower = left * 100.0
        rightPower = right * 100.0
        self._set_motors(leftPower, rightPower)

    def _send(self, message):
        self.ser.write(message + "\n")
        data = None
        if message[0] == 'S':
            header = self.ser.read(9)
            data   = self.ser.read(160)
            newline   = self.ser.read(1)
            retval = [0 for x in range(80)]
            for i in range(len(retval)):
                retval[i] = int(data[i * 2:i * 2 + 2])/63.0
            return retval
        elif message[0] in ['r', 'R']:
            header = self.ser.readline()
            self.id = header[3] + header[4]
        elif message[0] == 'I':
            header = self.ser.read(10)
            resolution = header[5] # 1, 3, 5
            length = (ord(header[6]) * 256 ** 0 +
                      ord(header[7]) * 256 ** 1 +
                      ord(header[8]) * 256 ** 2 +
                      ord(header[9]) * 256 ** 3)
            data   = self.ser.read(length)
            return data
        elif message[0] == 'V':
            return self.ser.readline()[12:].strip()
        else:
            ack = self.ser.read(2)
            if ack != "#" + message[0]:
                print "error:", message, ack
                return 0
            return 1

    def _set_motors(self, motor_left, motor_right):
        ml = int(min(max(motor_left, -100), 100))
        mr = int(min(max(motor_right, -100), 100))
        self._send("M" + encode(ml) + encode(mr) + chr(0))
