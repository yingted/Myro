"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Keith"

import serial, time
from myro import Robot

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
    
    SET_MOTORS_OFF=20
    SET_MOTORS=21
    SET_LED_LEFT_ON=22
    SET_LED_LEFT_OFF=23
    SET_LED_CENTER_ON=24
    SET_LED_CENTER_OFF=25
    SET_LED_RIGHT_ON=26
    SET_LED_RIGHT_OFF=27
    SET_SPEAKER=28

    def __init__(self, serialport, baudrate = 38400):
        self.debug = 0
        self.lastTranslate = 0
        self.lastRotate    = 0
        self.ser = serial.Serial(serialport, timeout=.5)
        self.ser.baudrate = baudrate
        self.ser.flushInput()
        self.ser.flushOutput()
        time.sleep(1)
        self.restart()

    def restart(self):
        self.set_motors_off()
        self.set_led_right_on()
        self.set_led_center_on()
        self.set_led_left_on()
        self.beep(1600, .16)
        self.beep(800, .1)
        self.beep(1200, .16)

    def beep(self, frequency, duration):
        self.set_speaker(int(frequency), int(duration * 1000))

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

    def quit(self):
        self.ser.close()

    def setLED(self, position, value):
        if position == "center":
            if value: return self.set_led_center_on()
            else:        return self.set_led_center_off()
        elif position == "left":
            if value: return self.set_led_left_on()
            else:        return self.set_led_left_off()
        elif position == "right":
            if value: return self.set_led_right_on()
            else:        return self.set_led_right_off()
        else:
            raise AttributeError, "no such LED: '%s'" % position

    def readLight(self, position):
        if position == 0:
            return self.get_light_left()
        elif position == 1:
            return self.get_light_right()
        elif position == 2:
            return self.get_light_center()
        else:
            raise AttributeError, "no such light sensor: '%s'" % position

    def readIR(self, position):
        if position == 0:
            return self.get_open_left()
        elif position == 1:
            return self.get_open_right()
        else:
            raise AttributeError, "no such IR sensor: '%s'" % position

    def update(self):
        # store all data in a structure?
        pass

####################### Private

    def adjustSpeed(self):
        left  = min(max(self.lastTranslate - self.lastRotate, -1), 1)
        right  = min(max(self.lastTranslate + self.lastRotate, -1), 1)
        leftPower = (left + 1.0) * 100.0
        rightPower = (right + 1.0) * 100.0
        self.set_motors(leftPower, rightPower)

    def read(self):
        c = self.ser.read(1)
        x = -1
        if (c != ""):
            x = ord(c)            
        return x

    def check(self, cmd):
        c = self.read()
        if (cmd != c) and self.debug: print "   failed check!"
        
    def write(self, num):
        self.ser.write(chr(int(num)))
        c = self.read()
        if (num != c):
            if self.debug: print "   failed write check!"
            self.ser.flushInput() # flush buffer

    def set(self, value, subvalues = []):
        if self.debug: print "set():", value, subvalues
        self.write(value)
        for v in subvalues:
            time.sleep(0.05)
            self.set(v)
        return self.check(value)

    def set_motors(self, motor_right, motor_left):
        return self.set(Scribbler.SET_MOTORS, [motor_right, motor_left])
    
    def set_speaker(self, frequency, duration):
        return self.set(Scribbler.SET_SPEAKER, [duration >> 8,
                                                duration % 256,
                                                frequency >> 8,
                                                frequency % 256])
    
    def set_motors_off(self):
        return self.set(Scribbler.SET_MOTORS_OFF)
        
    def set_led_left_on(self):
        return self.set(Scribbler.SET_LED_LEFT_ON)
    
    def set_led_left_off(self):
        return self.set(Scribbler.SET_LED_LEFT_OFF)

    def set_led_center_on(self):
        return self.set(Scribbler.SET_LED_CENTER_ON)

    def set_led_center_off(self):
        return self.set(Scribbler.SET_LED_CENTER_OFF)

    def set_led_right_on(self):
        return self.set(Scribbler.SET_LED_RIGHT_ON)

    def set_led_right_off(self):
        return self.set(Scribbler.SET_LED_RIGHT_OFF)
        
    def get(self, value):
        self.write(value)
        retval = self.read()
        self.check(value)
        return retval
        
    def get_open_left(self):
        return self.get(Scribbler.GET_OPEN_LEFT)

    def get_open_right(self):
        return self.get(Scribbler.GET_OPEN_RIGHT)

    def get_stall(self):
        return self.get(Scribbler.GET_STALL)

    def get_light_left(self):
        return self.get(Scribbler.GET_LIGHT_LEFT)

    def get_light_center(self):
        return self.get(Scribbler.GET_LIGHT_CENTER)

    def get_light_right(self):
        return self.get(Scribbler.GET_LIGHT_RIGHT)

    def get_line_right(self):
        return self.get(Scribbler.GET_LINE_RIGHT)

    def get_line_left(self):
        return self.get(Scribbler.GET_LINE_LEFT)

