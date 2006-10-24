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
        self.lastTranslate = 0
        self.lastRotate    = 0
        if serialport == None:
            serialport = ask("Port")
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
                    self.ser = serial.Serial(self.serialPort, timeout = 10)
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
        self.set_motors_off()
        self.set_led_right_off()
        self.set_led_center_on()
        self.set_led_left_off()
        self.beep(.10, 1600)
        self.beep(.10, 800)
        self.beep(.10, 1200)
	name = self.getName()
        print "Hello, I'm %s!" % name

    def beep(self, duration, frequency, frequency2 = None):
        if frequency2 == None:
            self.set_speaker(int(frequency), int(duration * 1000))
        else:
            self.set_speaker_2(int(frequency), int(frequency2), int(duration * 1000))

    def getName(self):
        return self.get_name().strip(chr(0))

    def getAll(self):
	""" IrLeft, IrRight, LightLeft/2, LightCenter/2, LightRight/2, LineLeft, LineRight, Stall """
	retval = self.get_all() # returned as bytes
        return {"light": [retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], retval[6] << 8 | retval[7]],
		"ir": [retval[0], retval[1]], "line": [retval[8], retval[9]], "stall": retval[10]}

    def setName(self, str):
        return self.set_name(str[:8].strip())

    def translate(self, amount):
        self.lastTranslate = amount
        self.adjustSpeed()

    def rotate(self, amount):
        self.lastRotate = amount
        self.adjustSpeed()

    def move(self, translate, rotate):
        self.lastTranslate = translate
        self.lastRotate = rotate
        self.adjustSpeed()

    def setVolume(self, value):
        if isTrue(value): return self.set_loud()
	else:             return self.set_quiet()

    def setLED(self, position, value):
        if type(position) in [int, float]:
            if position == 0:
                if isTrue(value): return self.set_led_left_on()
                else:             return self.set_led_left_off()
            elif position == 1:
                if isTrue(value): return self.set_led_center_on()
                else:             return self.set_led_center_off()
            elif position == 2:
                if isTrue(value): return self.set_led_right_on()
                else:             return self.set_led_right_off()
            else:
                raise AttributeError, "no such LED: '%s'" % position
        else:
            position = position.lower()
            if position == "center":
                if isTrue(value): return self.set_led_center_on()
                else:             return self.set_led_center_off()
            elif position == "left":
                if isTrue(value): return self.set_led_left_on()
                else:             return self.set_led_left_off()
            elif position == "right":
                if isTrue(value): return self.set_led_right_on()
                else:             return self.set_led_right_off()
            elif position == "all":
                if isTrue(value): return self.set_led_all_on()
                else:             return self.set_led_all_off()
            else:
                raise AttributeError, "no such LED: '%s'" % position

    def getLight(self, position):
        if type(position) in [float, int]:
            if position == 0:
                return self.get_light_left()
            elif position == 1:
                return self.get_light_center()
            elif position == 2:
                return self.get_light_right()
            else:
                raise AttributeError, "no such light sensor: '%s'" % position
        else:
            position = position.lower()
            if position == "center":
                return self.get_light_center()
            elif position == "left":
                return self.get_light_left()
            elif position == "right":
                return self.get_light_right()
            elif position == "all":
                return self.get_light_all()
            else:
                raise AttributeError, "no such light sensor: '%s'" % position

    def getIR(self, position):
        if type(position) in [float, int]:
            if position == 0:
                return self.get_open_left()
            elif position == 1:
                return self.get_open_right()
            else:
                raise AttributeError, "no such IR sensor: '%s'" % position
	else:
            position = position.lower()
            if position == "left":
                return self.get_ir_left()
            elif position == "right":
                return self.get_ir_right()
            elif position == "all":
                return self.get_ir_all()
            else:
                raise AttributeError, "no such IR sensor: '%s'" % position

    def getLine(self, position):
        if type(position) in [float, int]:
            if position == 0:
                return self.get_line_left()
            elif position == 1:
                return self.get_line_right()
            else:
                raise AttributeError, "no such line sensor: '%s'" % position
	else:
            position = position.lower()
            if position == "left":
                return self.get_line_left()
            elif position == "right":
                return self.get_line_right()
            elif position == "all":
                return self.get_line_all()
            else:
                raise AttributeError, "no such line sensor: '%s'" % position

    def getStall(self):
        return self.get_stall()

    def update(self):
        # FIX: store all data in a structure, remove updates from above get's
        # and return cached version
        pass

####################### Private

    def adjustSpeed(self):
        left  = min(max(self.lastTranslate - self.lastRotate, -1), 1)
        right  = min(max(self.lastTranslate + self.lastRotate, -1), 1)
        leftPower = (left + 1.0) * 100.0
        rightPower = (right + 1.0) * 100.0
        self.set_motors(leftPower, rightPower)

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

    def set_name(self, name):
        name_raw = map(lambda x:  ord(x), name)
        return self._set(Scribbler.SET_NAME, name_raw)

    def set_motors(self, motor_left, motor_right):
        return self._set(Scribbler.SET_MOTORS, [motor_right, motor_left])

    
    def set_speaker(self, frequency, duration):
        return self._set(Scribbler.SET_SPEAKER, [duration >> 8,
                                                duration % 256,
                                                frequency >> 8,
                                                frequency % 256])

    def set_speaker_2(self, freq1, freq2, duration):
        return self._set(Scribbler.SET_SPEAKER_2, [duration >> 8,
                                                  duration % 256,
                                                  freq1 >> 8,
                                                  freq1 % 256,
                                                  freq2 >> 8,
                                                  freq2 % 256])
    
    def set_motors_off(self):
        return self._set(Scribbler.SET_MOTORS_OFF)
        
    def set_led_left_on(self):
        return self._set(Scribbler.SET_LED_LEFT_ON)
    
    def set_led_left_off(self):
        return self._set(Scribbler.SET_LED_LEFT_OFF)

    def set_led_center_on(self):
        return self._set(Scribbler.SET_LED_CENTER_ON)

    def set_led_center_off(self):
        return self._set(Scribbler.SET_LED_CENTER_OFF)

    def set_led_all_on(self):
        return self._set(Scribbler.SET_LED_ALL_ON)

    def set_led_all_off(self):
        return self._set(Scribbler.SET_LED_ALL_OFF)

    def set_led_right_on(self):
        return self._set(Scribbler.SET_LED_RIGHT_ON)

    def set_led_right_off(self):
        return self._set(Scribbler.SET_LED_RIGHT_OFF)
                
    def set_loud(self):
        return self._set(Scribbler.SET_LOUD)
                
    def set_quiet(self):
        return self._set(Scribbler.SET_QUIET)
                
    def get_open_left(self):
        return self._get(Scribbler.GET_OPEN_LEFT)

    def get_open_right(self):
        return self._get(Scribbler.GET_OPEN_RIGHT)

    def get_ir_all(self):
        return self._get(Scribbler.GET_IR_ALL, 2)

    def get_stall(self):
        return self._get(Scribbler.GET_STALL)

    def get_light_left(self):
        return self._get(Scribbler.GET_LIGHT_LEFT, 2, "word")

    def get_light_center(self):
        return self._get(Scribbler.GET_LIGHT_CENTER, 2, "word")

    def get_light_right(self):
        return self._get(Scribbler.GET_LIGHT_RIGHT, 2, "word")

    def get_light_all(self):
        return self._get(Scribbler.GET_LIGHT_ALL, 6, "word")

    def get_line_right(self):
        return self._get(Scribbler.GET_LINE_RIGHT)

    def get_line_left(self):
        return self._get(Scribbler.GET_LINE_LEFT)

    def get_line_all(self):
        return self._get(Scribbler.GET_LINE_ALL, 2)

    def get_all(self):
        return self._get(Scribbler.GET_ALL, 11)

    def get_name(self):
        self._write_long([Scribbler.GET_NAME])
        c = self.ser.read(Scribbler.NAME_LENGTH)        
        self._check(Scribbler.GET_NAME)
        return c
