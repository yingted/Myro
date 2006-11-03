"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Keith and Doug"

import serial, time, string
from myro import Robot, ask
import myro.globals

def isTrue(value):
    """
    Returns True if value is something we consider to be "on".
    Otherwise, return False.
    """
    if type(value) == str:
        return (value.lower() == "on")
    elif value: return True
    return False

class Scribbler(Robot):
    GET_INPUT=1
    GET_OPEN_LEFT=2
    GET_OPEN_RIGHT=3
    GET_STALL=4
    GET_LIGHT_LEFT=5
    GET_LIGHT_CENTER=6
    GET_LIGHT_RIGHT=7
    GET_LINE_RIGHT=8
    GET_LINE_LEFT=9
    GET_NAME=10
    
    SET_MOTORS_OFF=20
    SET_MOTORS=21
    SET_LED_LEFT_ON=22
    SET_LED_LEFT_OFF=23
    SET_LED_CENTER_ON=24
    SET_LED_CENTER_OFF=25
    SET_LED_RIGHT_ON=26
    SET_LED_RIGHT_OFF=27
    SET_SPEAKER=28
    SET_SPEAKER_2=29
    SET_NAME=30
    SET_LED_ALL_ON=31
    SET_LED_ALL_OFF=32
    GET_LIGHT_ALL=33
    GET_IR_ALL=34
    GET_LINE_ALL=35
    GET_ALL=36
    SET_LOUD=37
    SET_QUIET=38

    PACKET_LENGTH=9
    NAME_LENGTH=8
    
    def __init__(self, serialport = None, baudrate = 38400):
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

    def search(self):
        for x in range(20):
            port = "com" + str(x)
            if (self.debug or 1):
                print "trying on port", port, "for",self.serialPort
            try:
                self.ser = serial.Serial(port, timeout=.5)
                self.ser.baudrate = self.baudRate
                self.ser.flushOutput()
                self.ser.flushInput()
                time.sleep(.5)
                name = self.getName().lower()
                if (name == self.serialPort.strip().lower()):
                    print "Found", self.serialPort
                    self.ser.timeout=10
                    return
            except:
                pass
        print "Couldn't find the scribbler or device named", self.serialPort
    
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

    def close(self):
        self.ser.close()

    def restart(self):
        self.stop()
        self.set("led", "all", "off")
        self.beep(.10, 1600)
        self.beep(.10, 800)
        self.beep(.10, 1200)
	name = self.get("name")
        print "Hello, I'm %s!" % name

    def beep(self, duration, frequency, frequency2 = None):
        if frequency2 == None:
            self._set_speaker(int(frequency), int(duration * 1000))
        else:
            self._set_speaker_2(int(frequency), int(frequency2), int(duration * 1000))

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
        return self._set(Scribbler.SET_MOTORS_OFF)

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
        leftPower = (left + 1.0) * 100.0
        rightPower = (right + 1.0) * 100.0
        self._set_motors(leftPower, rightPower)

    def _read(self, bytes = 1):
        c = self.ser.read(bytes)
        if self.debug:
            if (c != ""):
                print "_read(): ", ord(c)
            else:
                print "_read(): "
        if bytes == 1:
            x = -1
            if (c != ""):
                x = ord(c)            
            return x
	else:
	    return map(ord, c)

    def _check(self, cmd):
        c = self._read()
        if (cmd != c) and self.debug: print "   failed check!", cmd, c
        
    def _write(self, num):
        self.ser.write(chr(int(num)))
        c = self._read()
        if (num != c):
            if self.debug: print "   failed write check!"
            self.ser.flushInput() # flush buffer

    def _write_long(self, rawdata):        
        t = map(lambda x: chr(int(x)), rawdata)
        for x in range(len(t), Scribbler.PACKET_LENGTH):
            t.append(chr(int(0)))
        data = string.join(t, '')
        if self.debug:
            print "write", len(data), "bytes: ",
            print map(lambda x:  ord(x), data)
        
        self.ser.write(data)      # write 7 packets
        c = self.ser.read(Scribbler.PACKET_LENGTH)      # read the echo packet
        if self.debug:
            print "read ", len(c), "bytes: ",
            print map(lambda x:  ord(x), c)

        # do a sanity check on the first byte of the echo
        if (data[0] != c[0]):
            if self.debug: print "   failed write check!", data, c
            self.ser.flushInput() # flush buffer

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
        return self._set(Scribbler.SET_MOTORS, [motor_right, motor_left])

    def _set_speaker(self, frequency, duration):
        return self._set(Scribbler.SET_SPEAKER, [duration >> 8,
                                                duration % 256,
                                                frequency >> 8,
                                                frequency % 256])

    def _set_speaker_2(self, freq1, freq2, duration):
        return self._set(Scribbler.SET_SPEAKER_2, [duration >> 8,
                                                  duration % 256,
                                                  freq1 >> 8,
                                                  freq1 % 256,
                                                  freq2 >> 8,
                                                  freq2 % 256])
    
