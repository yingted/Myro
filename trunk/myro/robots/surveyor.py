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

def hex2dec(hexNum):
    v, h = hexNum.split('x')
    values = {"0": 0, "1": 1, "2": 2, "3": 3, "4": 4,
              "5": 5, "6": 6, "7": 7, "8": 8, "9": 9,
              "A": 10, "B": 11, "C": 12, "D": 13,
              "E": 14, "F": 15}
    pos = len(h) - 1
    retval = 0
    for b in h:
        retval += values[b.upper()] * (16 ** pos)
        pos -= 1
    return retval

def encode(num):
    return chr(bin2dec(dec2bin(num)))

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
            self._write("F") # turn off failsafe
            self._write("m") # wander mode, non-autonomous movement

    def close(self):
        self.ser.close()

    def restart(self):
        self.stop()
        print "Hello, I'm Surveyor!"

    def get(self, sensor = "all", *position):
        sensor = sensor.lower()
        if sensor == "stall":
            return self._get(Scribbler.GET_STALL)
        elif sensor == "startsong":
            #TODO
            return "tada"
        elif sensor == "name":
            self._write_long([Scribbler.GET_NAME])
            c = self.ser.read(Scribbler.NAME_LENGTH)        
            self._check(Scribbler.GET_NAME)
            c = c.replace("\x00", "")
            return c
        elif sensor == "volume":
            return self._volume
        else:
            retvals = []
            if len(position) == 0:
                if sensor == "light":
                    return self._get(Scribbler.GET_LIGHT_ALL, 6, "word")
                elif sensor == "ir":
                    return self._get(Scribbler.GET_IR_ALL, 2)
                elif sensor == "line":
                    return self._get(Scribbler.GET_LINE_ALL, 2)
                elif sensor == "all":
                    retval = self._get(Scribbler.GET_ALL, 11) # returned as bytes
                    return {"light": [retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], retval[6] << 8 | retval[7]],
                            "ir": [retval[0], retval[1]], "line": [retval[8], retval[9]], "stall": retval[10]}
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            for pos in position:
                if sensor == "light":
                    if pos in [0, "left"]:
                        retvals.append(self._get(Scribbler.GET_LIGHT_LEFT, 2, "word"))
                    elif pos in [1, "middle", "center"]:
                        retvals.append(self._get(Scribbler.GET_LIGHT_CENTER, 2, "word"))
                    elif pos in [2, "right"]:
                        retvals.append(self._get(Scribbler.GET_LIGHT_RIGHT, 2, "word"))
                elif sensor == "ir":
                    if pos in [0, "left"]:
                        retvals.append(self._get(Scribbler.GET_OPEN_LEFT))
                    elif pos in [1, "right"]:
                        retvals.append(self._get(Scribbler.GET_OPEN_RIGHT))
                elif sensor == "line":
                    if pos in [0, "left"]:
                        retvals.append(self._get(Scribbler.GET_LINE_LEFT))
                    elif pos in [1, "right"]:
                        retvals.append(self._get(Scribbler.GET_LINE_RIGHT))
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def set(self, item, position, value = None):
        item = item.lower()
        if item == "led":
            if type(position) in [int, float]:
                if position == 0:
                    if isTrue(value): return self._set(Scribbler.SET_LED_LEFT_ON)
                    else:             return self._set(Scribbler.SET_LED_LEFT_OFF)
                elif position == 1:
                    if isTrue(value): return self._set(Scribbler.SET_LED_CENTER_ON)
                    else:             return self._set(Scribbler.SET_LED_CENTER_OFF)
                elif position == 2:
                    if isTrue(value): return self._set(Scribbler.SET_LED_RIGHT_ON)
                    else:             return self._set(Scribbler.SET_LED_CENTER_OFF)
                else:
                    raise AttributeError, "no such LED: '%s'" % position
            else:
                position = position.lower()
                if position == "center":
                    if isTrue(value): return self._set(Scribbler.SET_LED_CENTER_ON)
                    else:             return self._set(Scribbler.SET_LED_CENTER_OF)
                elif position == "left":
                    if isTrue(value): return self._set(Scribbler.SET_LED_LEFT_ON)
                    else:             return self._set(Scribbler.SET_LED_LEFT_OFF)
                elif position == "right":
                    if isTrue(value): return self._set(Scribbler.SET_LED_RIGHT_ON)
                    else:             return self._set(Scribbler.SET_LED_RIGHT_OFF)
                elif position == "all":
                    if isTrue(value): return self._set(Scribbler.SET_LED_ALL_ON)
                    else:             return self._set(Scribbler.SET_LED_ALL_OFF)
                else:
                    raise AttributeError, "no such LED: '%s'" % position
            return "ok"
        elif item == "name":
            name = position[:8].strip()
            name_raw = map(lambda x:  ord(x), name)
            self._set(Scribbler.SET_NAME, name_raw)
            return "ok"
        elif item == "volume":
            if isTrue(position):
                self._volume = 1
                return self._set(Scribbler.SET_LOUD)
            else:
                self._volume = 0
                return self._set(Scribbler.SET_QUIET)
            return "ok"
        elif item == "startsong":
            self.startsong = position
            return "ok"
        else:
            raise ("invalid set item name: '%s'" % item)

    def stop(self):
        return self.move(0, 0)

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

####################### Private

    def _adjustSpeed(self):
        left  = min(max(self._lastTranslate - self._lastRotate, -1), 1)
        right  = min(max(self._lastTranslate + self._lastRotate, -1), 1)
        leftPower = left * 100.0
        rightPower = right * 100.0
        self._set_motors(leftPower, rightPower)

    def _write(self, message):
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
        elif message[0] == 'I':
            header = self.ser.read(10)
            resolution = header[5] # 1, 3, 5
            length = (ord(header[6]) * 256 ** 0 +
                      ord(header[7]) * 256 ** 1 +
                      ord(header[8]) * 256 ** 2 +
                      ord(header[9]) * 256 ** 3)
            data   = self.ser.read(length)
            return data
        else:
            ack = self.ser.read(2)
            if ack != "#" + message[0]:
                print "check error:", message, ack
                return 0
            return 1

    def _set(self, value, subvalues = []):
        rawdata = [value]
        rawdata.extend(subvalues)
        if self.debug: print "new set():", rawdata
        self._write_long(rawdata)
        return self._check(value)

    def _get(self, value, bytes = 1, mode = "byte"):
        self._write_long([value])
        retval = self._read(bytes)
        self._check(value)
	if mode == "byte":
            return retval
	elif mode == "word":
	    newRetval = []
	    for p in range(0,len(retval),2):
	        newRetval.append(retval[p] << 8 | retval[p + 1])
	    return newRetval

    def _set_motors(self, motor_left, motor_right):
        ml = int(min(max(motor_left, -100), 100))
        mr = int(min(max(motor_right, -100), 100))
        self._write("M" + encode(ml) + encode(mr) + chr(0))
