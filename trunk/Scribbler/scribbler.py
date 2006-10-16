import serial
import time
import logging
import sys
import thread
import signal
from threading import *


if (sys.platform == "win32"):
    import msvcrt
    #serialport = "/dev/tty.eb500-A7SerialPort-1"
else:    
    import termios, TERMIOS
    import fcntl, os
    
BACKWARD=0
BACKWARDSLOW=50
STOP=100
FORWARDSLOW=150
FORWARD=200

logging.basicConfig(level=logging.ERROR)

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

    def __init__(self, serialport):
        self.ser = serial.Serial(serialport, timeout=30)
        #self.ser.baudrate = 9600
        self.ser.baudrate = 38400
        print self.ser
        self.ser.flushInput()
        self.ser.flushOutput()
        time.sleep(1)
        # Set the signal handler

    def init(self):
        self.set_motors_off()
        self.set_led_right_on()
        self.set_led_center_on()
        self.set_led_left_on()
        self.set_speaker(1600, 160)
        self.set_speaker(800, 100)
        self.set_speaker(1200, 160)

    def close(self):
        print "Closing serial"
        self.ser.close()

    def read_num(self):
        c = self.ser.read(1)
        x = -1
        if (c == ""):
            logging.error ("PACKET TIMED OUT")
        else:
            x = ord(c)            
        return x

    def check_cmd(self, cmd):
        c = self.read_num()
        if (cmd != c):
            logging.error("CMD: Bad echo for " +  str(cmd) + " " + str(c))
        
    def write_num(self,x):
        logging.debug("Sending: " + str(x))
        self.ser.write(chr(x))
        c = self.read_num()
        if (x != c):
            logging.error("Bad echo for " +  str(x) + " " + str(c))
            self.ser.flushInput() # flush buffer
#        time.sleep(0.005)
        
    def set_motors_off(self):
        logging.info("Motors off:")
        self.write_num(Scribbler.SET_MOTORS_OFF)
        self.check_cmd(Scribbler.SET_MOTORS_OFF)
        
    def set_motor(self,motor_right, motor_left):
        logging.info("Setting motor: " + str(motor_right) + " " + str(motor_left))
        self.write_num(Scribbler.SET_MOTORS)
        self.write_num(motor_right)
        self.check_cmd(motor_right)
        self.write_num(motor_left)
        self.check_cmd(motor_left)
        self.check_cmd(Scribbler.SET_MOTORS)

    def set_speaker(self,frequency, duration):
        logging.info("Setting speaker: " + str(frequency) + " " + str(duration))
        self.write_num(Scribbler.SET_SPEAKER)
        self.write_num(duration >> 8)
        self.check_cmd(duration >> 8)
        self.write_num(duration % 256)
        self.check_cmd(duration % 256)
        self.write_num(frequency >> 8)
        self.check_cmd(frequency >> 8)
        self.write_num(frequency % 256)
        self.check_cmd(frequency % 256)
#        time.sleep(duration/1000.0)
        self.check_cmd(Scribbler.SET_SPEAKER)
    
    def get_open_left(self):
        self.write_num(Scribbler.GET_OPEN_LEFT)
        x = self.read_num()
        logging.info("Open left: " + str(x))
        self.check_cmd(Scribbler.GET_OPEN_LEFT)
        return x

    def get_open_right(self):
        self.write_num(Scribbler.GET_OPEN_RIGHT)
        x = self.read_num()
        logging.info("Open right: " + str(x))
        self.check_cmd(Scribbler.GET_OPEN_RIGHT)
        return x

    def get_stall(self):
        self.write_num(Scribbler.GET_STALL)
        x = self.read_num()
        logging.info( "Stall: " + str(x))
        self.check_cmd(Scribbler.GET_STALL)
        return x

    def get_light_left(self):
        self.write_num(Scribbler.GET_LIGHT_LEFT)
        x = self.read_num()
        logging.info( "Light left: " + str(x))
        self.check_cmd(Scribbler.GET_LIGHT_LEFT)
        return x


    def get_light_center(self):
        self.write_num(Scribbler.GET_LIGHT_CENTER)
        x = self.read_num()
        logging.info( "Light center: " + str(x))
        self.check_cmd(Scribbler.GET_LIGHT_CENTER)
        return x

    def get_light_right(self):
        self.write_num(Scribbler.GET_LIGHT_RIGHT)
        x = self.read_num()
        logging.info("Light right: " + str(x))
        self.check_cmd(Scribbler.GET_LIGHT_RIGHT)
        return x

    def get_line_right(self):
        self.write_num(Scribbler.GET_LINE_RIGHT)
        x = self.read_num()
        logging.info("Line right: " + str(x))
        self.check_cmd(Scribbler.GET_LIGHT_RIGHT)
        return x

    def get_line_left(self):
        self.write_num(Scribbler.GET_LINE_LEFT)
        x = self.read_num()
        logging.info("Line left: " + str(x))
        self.check_cmd(Scribbler.GET_LIGHT_LEFT)
        return x

    def set_led_left_on(self):
        logging.info("LED left on")
        self.write_num(Scribbler.SET_LED_LEFT_ON)
        self.check_cmd(Scribbler.SET_LED_LEFT_ON)
    
    def set_led_left_off(self):
        logging.info("LED left off")
        self.write_num(Scribbler.SET_LED_LEFT_OFF)
        self.check_cmd(Scribbler.SET_LED_LEFT_OFF)

    def set_led_center_on(self):
        logging.info("LED center on")
        self.write_num(Scribbler.SET_LED_CENTER_ON)
        self.check_cmd(Scribbler.SET_LED_CENTER_ON)

    def set_led_center_off(self):
        logging.info("LED center off")
        self.write_num(Scribbler.SET_LED_CENTER_OFF)
        self.check_cmd(Scribbler.SET_LED_CENTER_OFF)

    def set_led_right_on(self):
        logging.info("LED right on")
        self.write_num(Scribbler.SET_LED_RIGHT_ON)
        self.check_cmd(Scribbler.SET_LED_RIGHT_ON)

    def set_led_right_off(self):
        logging.info("LED right off")
        self.write_num(Scribbler.SET_LED_RIGHT_OFF)
        self.check_cmd(Scribbler.SET_LED_RIGHT_OFF)
        
