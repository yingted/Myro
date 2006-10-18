"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Keith and Doug"

import serial, time, string
from myro import Robot

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

    PACKET_LENGTH=9
    NAME_LENGTH=8
    
    def __init__(self, serialport, baudrate = 38400):
        self.debug = 0
        self.lastTranslate = 0
        self.lastRotate    = 0
        self.serialPort = serialport
        self.baudRate = baudrate
        self.open()
        time.sleep(1)
        self.restart()

    def search_for_robot(self):
        for x in range(0,20):
            port = "com" + str(x)
            if (self.debug):
                print "trying on port", port, "for",self.serialPort
            try:
                self.ser = serial.Serial(port, timeout=.5)
                self.ser.baudrate = self.baudRate
                self.ser.flushOutput()
                self.ser.flushInput()

                name = self.getName().strip(chr(0)).lower()
                if (name == self.serialPort.strip().lower()):
                    print "Found", self.serialPort
                    self.ser.timeout=10
                    return
            except:
                pass

        print "Couldn't find the scribbler or device named", self.serialPort
    
    def open(self):
        try:
            self.ser = serial.Serial(self.serialPort, timeout = 10)
            self.ser.baudrate = self.baudRate
            self.ser.flushOutput()
            self.ser.flushInput()
        except:
            self.search_for_robot()
            
    def quit(self):
        self.stop()
        self.close()

    def close(self):
        self.ser.close()
        
    def restart(self):
        self.set_motors_off()
        self.set_led_right_on()
        self.set_led_center_on()
        self.set_led_left_on()
        self.beep(.16, 1600)
        self.beep(.10, 800)
        self.beep(.16, 1200)

    def beep(self, duration, frequency, frequency2 = None):
        if frequency2 == None:
            self.set_speaker(int(frequency), int(duration * 1000))
        else:
            self.set_speaker_2(int(frequency), int(frequency2), int(duration * 1000))

    def getName(self):
        return self.get_name()

    def setName(self, str):
        return self.set_name(str)

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

    def setLED(self, position, value):
        if type(position) in [int, float]:
            if position == 2:
                if isTrue(value): return self.set_led_center_on()
                else:             return self.set_led_center_off()
            elif position == 0:
                if isTrue(value): return self.set_led_left_on()
                else:             return self.set_led_left_off()
            elif position == 1:
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
            else:
                raise AttributeError, "no such LED: '%s'" % position

    def readLight(self, position):
        if type(position) in [float, int]:
            if position == 0:
                return self.get_light_left()
            elif position == 1:
                return self.get_light_right()
            elif position == 2:
                return self.get_light_center()
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
            else:
                raise AttributeError, "no such light sensor: '%s'" % position

    def readIR(self, position):
        if position == 0:
            return self.get_open_left()
        elif position == 1:
            return self.get_open_right()
        else:
            raise AttributeError, "no such IR sensor: '%s'" % position

    def readLine(self, position):
        if position == 0:
            return self.get_line_left()
        elif position == 1:
            return self.get_line_right()
        else:
            raise AttributeError, "no such line sensor: '%s'" % position

    def readStall(self):
        return self.get_stall()

    def update(self):
        # FIX: store all data in a structure, remove updates from above read's
        # and return cached version
        pass

####################### Private

    def adjustSpeed(self):
        left  = min(max(self.lastTranslate - self.lastRotate, -1), 1)
        right  = min(max(self.lastTranslate + self.lastRotate, -1), 1)
        leftPower = (left + 1.0) * 100.0
        rightPower = (right + 1.0) * 100.0
        self.set_motors(leftPower, rightPower)

    def _read(self):
        c = self.ser.read(1)
        if self.debug:
            if (c != ""):
                print "_read(): ", ord(c)
            else:
                print "_read(): "
        x = -1
        if (c != ""):
            x = ord(c)            
        return x

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

    def _get(self, value):
        self._write_long([value])
        retval = self._read()
        self._check(value)
        return retval

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

    def set_led_right_on(self):
        return self._set(Scribbler.SET_LED_RIGHT_ON)

    def set_led_right_off(self):
        return self._set(Scribbler.SET_LED_RIGHT_OFF)
                
    def get_open_left(self):
        return self._get(Scribbler.GET_OPEN_LEFT)

    def get_open_right(self):
        return self._get(Scribbler.GET_OPEN_RIGHT)

    def get_stall(self):
        return self._get(Scribbler.GET_STALL)

    def get_light_left(self):
        return self._get(Scribbler.GET_LIGHT_LEFT)

    def get_light_center(self):
        return self._get(Scribbler.GET_LIGHT_CENTER)

    def get_light_right(self):
        return self._get(Scribbler.GET_LIGHT_RIGHT)

    def get_line_right(self):
        return self._get(Scribbler.GET_LINE_RIGHT)

    def get_line_left(self):
        return self._get(Scribbler.GET_LINE_LEFT)

    def get_name(self):
        self._write_long([Scribbler.GET_NAME])
        c = self.ser.read(Scribbler.NAME_LENGTH)        
        self._check(Scribbler.GET_NAME)
        return c


