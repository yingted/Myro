import serial, time

class Scribbler:
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
        self.debug = 1
        self.ser = serial.Serial(serialport, timeout=1)
        self.ser.baudrate = baudrate
        self.ser.flushInput()
        self.ser.flushOutput()
        time.sleep(1)
        self.init()

    def init(self):
        self.set_motors_off()
        self.set_led_right_on()
        self.set_led_center_on()
        self.set_led_left_on()
        self.set_speaker(1600, 160)
        self.set_speaker(800, 100)
        self.set_speaker(1200, 160)

    def translate(self, amount):
        power = (amount + 1.0) * 100.0 # scale between 0,200
        # need to keep track of these to blend
        self.set_motor(power, power)

    def rotate(self, amount):
        power = (amount + 1.0) * 100.0 # scale between 0,200
        # -1 to right
        # +1 to left

    def close(self):
        self.ser.close()

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
        #        time.sleep(0.005)

    def set(self, value, subvalues = []):
        if self.debug: print "set():", value, subvalues
        self.write(value)
        for v in subvalues:
            self.set(v)
        return self.check(value)

    def set_motor(self, motor_right, motor_left):
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

