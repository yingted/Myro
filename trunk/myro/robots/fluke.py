"""
NOTICE: This file is a copy of scribbler.py.
IT IS NOT USED BY MYRO2!

This file contains experimental modifications for using the Fluke on it's
own, without a scribbler. If you are trying to modify Myro2 (the Python
verson of Myro) you should be working on the scribbler.py file instead!

Myro code for the Fluke board.
(c) 2007, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Keith O'Hara and Doug Blank"

import time, string
try:
    import serial
except:
    print "WARNING: pyserial not loaded: fluke won't work!"
from myro import ask
from myro.graphics import _askQuestion, Picture
import myro.globvars
import cStringIO
import threading
import array

class BufferedRead:
    def __init__(self, serial, size, start = 1):
        self.serial = serial
        self.size = size
        if start:
            self.data = self.serial.read(size)
        else:
            self.data = ""
    def __getitem__(self, position):
        """ Return an element of the string """
        while position >= len(self.data):
            #self.data += self.serial.read(self.size - len(self.data))
            self.data += self.serial.read(self.size - len(self.data))
            #print "      length so far = ", len(self.data), " waiting for total = ", self.size
        return self.data[position]
    def __len__(self):
        """ Lie. Tell them it is this long. """
        return self.size

def isTrue(value):
    """
    Returns True if value is something we consider to be "on".
    Otherwise, return False.
    """
    if type(value) == str:
        return (value.lower() == "on")
    elif value: return True
    return False

class Fluke:
    SOFT_RESET=33
    GET_ALL=65 
    GET_ALL_BINARY=66  
    GET_LIGHT_LEFT=67  
    GET_LIGHT_CENTER=68  
    GET_LIGHT_RIGHT=69  
    GET_LIGHT_ALL=70  
    GET_IR_LEFT=71  
    GET_IR_RIGHT=72  
    GET_IR_ALL=73  
    GET_LINE_LEFT=74  
    GET_LINE_RIGHT=75  
    GET_LINE_ALL=76  
    GET_STATE=77  
    GET_NAME1=78
    GET_NAME2=64
    GET_STALL=79  
    GET_INFO=80  
    GET_DATA=81  

    GET_PASS1=50
    GET_PASS2=51

    GET_RLE=82  # a segmented and run-length encoded image
    GET_IMAGE=83  # the entire 256 x 192 image in YUYV format
    GET_WINDOW=84  # the windowed image (followed by which window)
    GET_DONGLE_L_IR=85  # number of returned pulses when left emitter is turned on
    GET_DONGLE_C_IR=86  # number of returned pulses when center emitter is turned on
    GET_DONGLE_R_IR=87  # number of returned pulses when right emitter is turned on
    GET_WINDOW_LIGHT=88    # average intensity in the user defined region
    GET_BATTERY=89  # battery voltage
    GET_SERIAL_MEM=90  # with the address returns the value in serial memory
    GET_SCRIB_PROGRAM=91  # with offset, returns the fluke program buffer
    GET_CAM_PARAM=92 # with address, returns the camera parameter at that address

    GET_BLOB=95

    SET_PASS1=55
    SET_PASS2=56
    SET_SINGLE_DATA=96
    SET_DATA=97
    SET_ECHO_MODE=98
    SET_LED_LEFT_ON=99 
    SET_LED_LEFT_OFF=100
    SET_LED_CENTER_ON=101
    SET_LED_CENTER_OFF=102
    SET_LED_RIGHT_ON=103
    SET_LED_RIGHT_OFF=104
    SET_LED_ALL_ON=105
    SET_LED_ALL_OFF=106
    SET_LED_ALL=107 
    SET_MOTORS_OFF=108
    SET_MOTORS=109 
    SET_NAME1=110 
    SET_NAME2=119           # set name2 byte
    SET_LOUD=111
    SET_QUIET=112
    SET_SPEAKER=113
    SET_SPEAKER_2=114

    SET_DONGLE_LED_ON=116   # turn binary dongle led on
    SET_DONGLE_LED_OFF=117  # turn binary dongle led off
    SET_RLE=118             # set rle parameters 
    SET_DONGLE_IR=120       # set dongle IR power
    SET_SERIAL_MEM=121      # set serial memory byte
    SET_SCRIB_PROGRAM=122   # set fluke program memory byte
    SET_START_PROGRAM=123   # initiate fluke programming process
    SET_RESET_SCRIBBLER=124 # hard reset scribbler
    SET_SERIAL_ERASE=125    # erase serial memory
    SET_DIMMER_LED=126      # set dimmer led
    SET_WINDOW=127          # set user defined window
    SET_FORWARDNESS=128     # set direction of fluke
    SET_WHITE_BALANCE=129   # turn on white balance on camera 
    SET_NO_WHITE_BALANCE=130 # diable white balance on camera (default)
    SET_CAM_PARAM=131       # with address and value, sets the camera parameter at that address

    GET_JPEG_GRAY_HEADER=135
    GET_JPEG_GRAY_SCAN=136
    GET_JPEG_COLOR_HEADER=137
    GET_JPEG_COLOR_SCAN=138
    DUMP_IMAGE=139

    SET_PASS_N_BYTES=139
    GET_PASS_N_BYTES=140
    GET_PASS_BYTES_UNTIL=141

    GET_VERSION=142

    PACKET_LENGTH     =  9
    
    def __init__(self, serialport = None, baudrate = 38400):
        self.lock = threading.Lock()

        #### Camera Addresses ####
        self.CAM_PID=0x0A
        self.CAM_PID_DEFAULT=0x76
    
        self.CAM_VER=0x0B
        self.CAM_VER_DEFAULT=0x48
    
        self.CAM_BRT=0x06
        self.CAM_BRT_DEFAULT=0x80
    
        self.CAM_EXP=0x10
        self.CAM_EXP_DEFAULT=0x41
    
        self.CAM_COMA=0x12
        self.CAM_COMA_DEFAULT=0x14
        self.CAM_COMA_WHITE_BALANCE_ON= (self.CAM_COMA_DEFAULT |  (1 << 2))
        self.CAM_COMA_WHITE_BALANCE_OFF=(self.CAM_COMA_DEFAULT & ~(1 << 2))
    
        self.CAM_COMB=0x13
        self.CAM_COMB_DEFAULT=0xA3
        self.CAM_COMB_GAIN_CONTROL_ON= (self.CAM_COMB_DEFAULT |  (1 << 1))
        self.CAM_COMB_GAIN_CONTROL_OFF=(self.CAM_COMB_DEFAULT & ~(1 << 1))
        self.CAM_COMB_EXPOSURE_CONTROL_ON= (self.CAM_COMB_DEFAULT |  (1 << 0))
        self.CAM_COMB_EXPOSURE_CONTROL_OFF=(self.CAM_COMB_DEFAULT & ~(1 << 0))

        self.ser = None
        self.requestStop = 0
        self.debug = 0
        self._lastTranslate = 0
        self._lastRotate    = 0
        self._volume = 0
        if serialport == None:
            serialport = ask("Port", useCache = 1)
        # Deal with requirement that Windows "COM#" names where # >= 9 needs to
        # be in the format "\\.\COM#"
        hasPort = True
        if type(serialport) == str and serialport.lower().startswith("com"):
            portnum = int(serialport[3:])
        elif isinstance(serialport, int): #allow integer input
            portnum = serialport
        else: hasPort = False
        if hasPort:
            if portnum >= 10:
                serialport = r'\\.\COM%d' % (portnum)
        self.serialPort = serialport
        self.baudRate = baudrate
        self.open()
        
        myro.globvars.robot = self
        self.dongle = None
        self.robotinfo = {}
        info = self.getVersion()
        if "fluke" in info.keys():
            self.dongle = info["fluke"]
            print "You are using fluke firmware", info["fluke"]
        elif "dongle" in info.keys():
            self.dongle = info["dongle"]
            print "You are using fluke firmware", info["dongle"]
        if self.dongle != None:
            # Turning on White Balance, Gain Control, and Exposure Control
            self.set_cam_param(self.CAM_COMA, self.CAM_COMA_WHITE_BALANCE_ON)
            self.set_cam_param(self.CAM_COMB, self.CAM_COMB_GAIN_CONTROL_ON | self.CAM_COMB_EXPOSURE_CONTROL_ON)
            # Config grayscale on window 0, 1, 2
            conf_gray_window(self.ser, 0, 2,   0, 128, 191, 1, 1)
            conf_gray_window(self.ser, 1, 64,  0, 190, 191, 1, 1)
            conf_gray_window(self.ser, 2, 128, 0, 254, 191, 1, 1)
            set_ir_power(self.ser, 135)
            self.conf_rle(delay = 90, smooth_thresh = 4,
                          y_low=0, y_high=255,
                          u_low=51, u_high=136,
                          v_low=190, v_high=255)

        self.robotinfo = {}

    def open(self):
        try:
            if self.serialPort == myro.globvars.robot.ser.portstr:
                myro.globvars.robot.ser.close()
                print "Closing serial port..."
                time.sleep(1)
        except KeyboardInterrupt:
            raise
        except:
            pass
        while 1:
            try:
                self.ser = serial.Serial(self.serialPort, timeout = 10) 
                break
            except KeyboardInterrupt:
                raise
            except serial.SerialException:
                print "   Serial element not found. If this continues, remove/replace serial device..."
                try:
                    self.ser.close()
                except KeyboardInterrupt:
                    raise
                except:
                    pass
                try:
                    del self.ser
                except KeyboardInterrupt:
                    raise
                except:
                    pass
                time.sleep(1)
            except:
                print "Waiting on port...", self.serialPort
                try:
                    self.ser.close()
                except KeyboardInterrupt:
                    raise
                except:
                    pass
                try:
                    del self.ser
                except KeyboardInterrupt:
                    raise
                except:
                    pass
                time.sleep(1)
        self.ser.baudrate = self.baudRate
        #self.restart()

    def close(self):
        self.ser.close()

    def manual_flush(self):
        old = self.ser.timeout
        self.ser.setTimeout(.5)
        l = 'a'
        count = 0;
        while (len(l) != 0 and count < 50000):
            l = self.ser.read(1)
            count += len(l)
        self.ser.setTimeout(old)

    def restart(self):
        self.manual_flush()
        time.sleep(.25)               # give it some time
        while 1:
            self.ser.flushInput()         # flush "IPREScribby"...
            self.ser.flushOutput()
            time.sleep(1.2)       # give it time to see if another IPRE show up
            if self.ser.inWaiting() == 0: # if none, then we are out of here!
                break
            print "Waking fluke from sleep..."
            time.sleep(.25)               # give it some time
        self.ser.flushInput()
        self.ser.flushOutput()

    def get(self, sensor = "all", *position):
        sensor = sensor.lower()
        if sensor == "battery":
            return self.getBattery()
        elif sensor == "blob":
            return self.getBlob()
        else:
            if len(position) == 0:
                if sensor == "obstacle":
                    return [self.getObstacle("left"), self.getObstacle("center"), self.getObstacle("right")]
                elif sensor == "bright":
                    return [self.getBright("left"), self.getBright("middle"), self.getBright("right") ]
                elif sensor == "all":
                    retval = self._get(Fluke.GET_ALL, 11) # returned as bytes
                    self._lastSensors = retval # single bit sensors
                    return {"obstacle": [self.getObstacle("left"), self.getObstacle("center"), self.getObstacle("right")],
                            "bright": [self.getBright("left"), self.getBright("middle"), self.getBright("right")],
                            "blob": self.getBlob(),
                            "battery": self.getBattery(),
                            }
                else:                
                    raise ("invalid sensor name: '%s'" % sensor)
            retvals = []
            for pos in position:
                if sensor == "obstacle":
                    return self.getObstacle(pos)
                elif sensor == "bright":
                    return self.getBright(pos)
                elif sensor == "picture":
                    return self.takePicture(pos)
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 0:
                return None
            elif len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def getVersion(self):
        self.ser.write(chr(Fluke.GET_VERSION))
        retval = self.ser.readline()
        retDict = {}
        for pair in retval.split(","):
            if ":" in pair:            
                it, value = pair.split(":")
                retDict[it.lower().strip()] = value.strip()        
        return retDict

    ########################################################## Dongle Commands


    def conf_rle_range(self, picture, x1, y1, x2, y2):
        xs = [x1, x2]
        ys = [y1, y2]
        xs.sort()
        ys.sort()
        # min, max
        us = [255, 0]
        vs = [255, 0]
        #yps = [255, 0]
        for i in range(xs[0], xs[1] + 1, 1):
            for j in range(ys[0], ys[1] + 1, 1):
                r,g,b = picture.getPixel(i, j).getRGB()
                y,u,v = rgb2yuv(r, g, b)

                #if y < yps[0]:
                #    yps[0] = y
                #elif v > yps[1]:
                #    yps[1] = y

                if v < vs[0]:
                    vs[0] = v
                elif v > vs[1]:
                    vs[1] = v
                    
                if u < us[0]:
                    us[0] = u
                elif u > us[1]:                    
                    us[1] = u
                    
        self.conf_rle(delay = 90, smooth_thresh = 4,
                      #y_low=yps[0], y_high=yps[1],
                      u_low=us[0], u_high=us[1],
                      v_low=vs[0], v_high=vs[1])


    def configureBlob(self,
                      y_low=0,  y_high=255,
                      u_low=0,  u_high=255,
                      v_low=0,  v_high=255,
                      smooth_thresh=4):        
        self.conf_rle(y_low=y_low, y_high=y_high,
                      u_low=u_low, u_high=u_high,
                      v_low=v_low, v_high=v_high,
                      smooth_thresh=smooth_thresh)

# conf_rle   - Sets parameters for the Run Length Encoded blob image
#		returned by takePicture("blob")
#
# Y,U,V high/low parameters configure a bounding box (in YUV color space)
# of pixels that are "on" or white. All other pixels are off or black.
#
# Note - The delay parameter should always be 90. it's an internal tuning 
# parameter, and Keith says Bad Things(TM) happen if it's not 90.
#
# The smooth_thresh parameter tells how many pixels must be black/white
# before the Run Length Encoding (of blob images) recognizes the change
# and sends a run of the opposite bit. This makes the blob images look 
# slightly streaky, but greatly increases the efficiency of the RLE compression
# by eliminating minor noise.

    def conf_rle(self, delay = 90, smooth_thresh = 4,
                 y_low=0, y_high=254,
                 u_low=51, u_high=136,
                 v_low=190, v_high=254):
        if self.debug:
            print "configuring RLE", delay, smooth_thresh, y_low, y_high, u_low, u_high, v_low, v_high
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.SET_RLE))
            self.ser.write(chr(delay))
            self.ser.write(chr(smooth_thresh))
            self.ser.write(chr(y_low)) 
            self.ser.write(chr(y_high))
            self.ser.write(chr(u_low)) 
            self.ser.write(chr(u_high))
            self.ser.write(chr(v_low)) 
            self.ser.write(chr(v_high))
        finally:
            self.lock.release()

    def read_uint32(self):
        buf = self.ser.read(4)
        return ord(buf[0]) + ord(buf[1]) * 256 + ord(buf[2]) * 65536 + ord(buf[3]) * 16777216
    
    def read_jpeg_scan(self):
        bytes = ''
        last_byte = 0
        while True:
            byte = self.ser.read(1)
            bytes += byte
            
            if last_byte == chr(0xff) and byte == chr(0xd9):
                # End-of-image marker
                break

            last_byte = byte

        bm0 = self.read_uint32();   # Start
        bm1 = self.read_uint32();   # Read
        bm2 = self.read_uint32();   # Compress

        if self.debug:
            print "got image"
            freq = 60e6
            print '%.3f %.3f' % (((bm1 - bm0) / freq), ((bm2 - bm1) / freq))
        
        return bytes
    
    def read_jpeg_header(self):
        buf = self.ser.read(2)
        len = ord(buf[0]) + ord(buf[1]) * 256
        return self.ser.read(len)
    
    color_header = None
    def grab_jpeg_color(self, reliable):
        try:
            self.lock.acquire()
            if self.color_header == None:
                self.ser.write(chr(self.GET_JPEG_COLOR_HEADER))
                self.color_header = self.read_jpeg_header()
            
            self.ser.write(chr(self.GET_JPEG_COLOR_SCAN))
            self.ser.write(chr(reliable))
            jpeg = self.color_header + self.read_jpeg_scan()
        finally:
            self.lock.release()
        return jpeg
    
    gray_header = None
    def grab_jpeg_gray(self, reliable):
        try:
            self.lock.acquire()
            if self.gray_header == None:
                self.ser.write(chr(self.GET_JPEG_GRAY_HEADER))
                self.gray_header = self.read_jpeg_header()
            
            self.ser.write(chr(self.GET_JPEG_GRAY_SCAN))
            self.ser.write(chr(reliable))
            jpeg = self.gray_header + self.read_jpeg_scan()
        finally:
            self.lock.release()
        return jpeg

    # for older versions of fluke without jpeg
    # use this lookup table to decide what picture to grab
    image_codes = {"jpeg": "color",
                   "jpeg-fast": "color",
                   "grayjpeg": "gray",
                   "grayjpeg-fast": "gray"}
    
    def takePicture(self, mode="jpeg"):
        width = 256
        height = 192
        p = Picture()

        version = map(int, self.dongle.split("."))
    
        if version < [2, 7, 8]:
            mode = image_codes[mode]
            
        if mode == "color":
            a = self._grab_array()
            p.set(width, height, a)
        elif mode == "jpeg":
            jpeg = self.grab_jpeg_color(1)
            stream = cStringIO.StringIO(jpeg)  
            p.set(width, height, stream, "jpeg")
        elif mode == "jpeg-fast":
            jpeg = self.grab_jpeg_color(0)
            stream = cStringIO.StringIO(jpeg)  
            p.set(width, height, stream, "jpeg")
        elif mode == "grayjpeg":
            jpeg = self.grab_jpeg_gray(1)
            stream = cStringIO.StringIO(jpeg)  
            p.set(width, height, stream, "jpeg")
        elif mode == "grayjpeg-fast":
            jpeg = self.grab_jpeg_gray(0)
            stream = cStringIO.StringIO(jpeg)  
            p.set(width, height, stream, "jpeg")
        elif mode in ["gray", "grey"]:
            conf_window(self.ser, 0, 1, 0, 255, 191, 2, 2)
            a = self._grab_gray_array()
            conf_gray_window(self.ser, 0, 2, 0,    128, 191, 1, 1)
            p.set(width, height, a, "gray")
        elif mode == "blob":
            a = self._grab_blob_array()
            p.set(width, height, a, "blob")
        return p

    def _grab_blob_array(self):
        width = 256
        height = 192    
        blobs = array.array('B', [0] * (height * width)) # zeros(((height + 1), (width + 1)), dtype=uint8)
        line = ''
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.GET_RLE))
            size=ord(self.ser.read(1))
            size = (size << 8) | ord(self.ser.read(1))
            if self.debug:
                print "Grabbing RLE image size =", size
            line =''
            while (len(line) < size):
                line+=self.ser.read(size-len(line))
        finally:
            self.lock.release()
            
        px = 0
        counter = 0
        val = 128
        inside = True
        for i in range(height):
            for j in range(0, width, 4):
                if (counter < 1 and px < len(line)):
                    counter = ord(line[px])    	
                    px += 1
                    counter = (counter << 8) | ord(line[px])    	
                    px += 1
                    if (inside):
                        val = 0
                        inside = False
                    else:
                        val = 255
                        inside = True
                for z in range(0,4):
                    blobs[i * width + j+z] = val
                counter -= 1
        return blobs

    def _grab_gray_array(self):
        width = 128
        height = 96
        size= width*height
        #print "grabbing image size = ", size
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.GET_WINDOW))
            self.ser.write(chr(0))
            line = ''
            while (len(line) < size):
                line += self.ser.read(size-len(line))
                #print "length so far = ", len(line), " waiting for total = ", size
        finally:
            self.lock.release()
            
        line = quadrupleSize(line, width)
        return line

    def _grab_array(self):
        width = 256
        height = 192
        buffer = array.array('B', [0] * (height * width * 3))
        try:
            self.lock.acquire()
            oldtimeout = self.ser.timeout
            self.ser.setTimeout(.01)
            self.ser.write(chr(Fluke.GET_IMAGE))
            size= width*height
            line = BufferedRead(self.ser, size, start = 0)
            #create the image from the YUV layer
            for i in range(height):
                for j in range(width):   
                    if j >= 3:
                        # go to the left for other values
                        #vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3
                        vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3
                    else:
                        # go to the right for other values
                        vy = 1; vu = 2; y1v = 3; y1u = 1; uy = 1; uv = 2; y2u = 3; y2v = 1
                    
                                       #   0123 0123 0123
                    if ((j % 4) == 0): #3 #2   VYUY VYUY VYUY
                        V = ord(line[i * width + j])
                        Y = ord(line[i * width + j + vy])
                        U = ord(line[i * width + j + vu])
                    elif ((j % 4) == 1): #0 #3
                        Y = ord(line[i * width + j])
                        V = ord(line[i * width + j + y1v])
                        U = ord(line[i * width + j + y1u])
                    elif ((j % 4) == 2): #1 #0
                        U = ord(line[i * width + j])
                        Y = ord(line[i * width + j + uy])
                        V = ord(line[i * width + j + uv])
                    elif ((j % 4) == 3): #2 #1
                        Y = ord(line[i * width + j])
                        U = ord(line[i * width + j + y2u])
                        V = ord(line[i * width + j + y2v])
                    U = U - 128
                    V = V - 128
                    Y = Y
                    buffer[(i * width + j) * 3 + 0] = max(min(Y + 1.13983 * V, 255), 0)
                    buffer[(i * width + j) * 3 + 1] = max(min(Y - 0.39466*U-0.58060*V, 255), 0)
                    buffer[(i * width + j) * 3 + 2] = max(min(Y + 2.03211*U, 255), 0)
            self.ser.setTimeout(oldtimeout)
        finally:
            self.lock.release()
            
        return buffer

    def _grab_array_bilinear_horizontal(self):
        width = 256
        height = 192
        buffer = array.array('B', [0] * (height * width * 3))
        try:
            self.lock.acquire()
            oldtimeout = self.ser.timeout
            self.ser.setTimeout(.01)
            self.ser.write(chr(Fluke.GET_IMAGE))
            size= width*height
            line = BufferedRead(self.ser, size, start = 0)
            #create the image from the YUV layer
            for i in range(height):
                for j in range(width):
                    vy = 1; vu = 2; y1v = 3; y1u = 1; uy = 1; uv = 2; y2u = 3; y2v = 1
                    vy2 = -1; vu2 = -2; y1v2 = -1; y1u2 = -3; uy2 = -1; uv2 = -2; y2u2 = -1; y2v2 = -3

                    if j >= 3:
                        # go to the left for other values
                        #vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3
                        vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3
                        if j < (width - 4):
                            vy2 = +3; vu2 = +2; y1v2 = +3; y1u2 = +1; uy2 = +3; uv2 = +2; y2u2 = +3; y2v2 = +1


                                #   0123 0123 0123
                    if ((j % 4) == 0): #3 #2   VYUY VYUY VYUY
                        V = ord(line[i * width + j])
                        Y = 0.5*ord(line[i * width + j + vy]) + 0.5*ord(line[(i) * width + j + vy2])
                        U = 0.5*ord(line[i * width + j + vu]) + 0.5*ord(line[(i) * width + j + vu2])
                    elif ((j % 4) == 1): #0 #3
                        Y = ord(line[i * width + j])
                        V = 0.5*ord(line[i * width + j + y1v]) + 0.5*ord(line[(i) * width + j + y1v2])
                        U = 0.5*ord(line[i * width + j + y1u]) + 0.5*ord(line[(i) * width + j + y1u2]) 
                    elif ((j % 4) == 2): #1 #0
                        U = ord(line[i * width + j])
                        Y = 0.5*ord(line[i * width + j + uy]) + 0.5*ord(line[(i) * width + j + uy2])
                        V = 0.5*ord(line[i * width + j + uv]) + 0.5*ord(line[(i) * width + j + uv2])
                    elif ((j % 4) == 3): #2 #1
                        Y = ord(line[i * width + j])
                        U = 0.5*ord(line[i * width + j + y2u]) + 0.5*ord(line[(i) * width + j + y2u2])
                        V = 0.5*ord(line[i * width + j + y2v]) + 0.5*ord(line[(i) * width + j + y2v2])
                    U = U - 128
                    V = V - 128
                    Y = Y
                    buffer[(i * width + j) * 3 + 0] = max(min(Y + 1.13983 * V, 255), 0)
                    buffer[(i * width + j) * 3 + 1] = max(min(Y - 0.39466*U-0.58060*V, 255), 0)
                    buffer[(i * width + j) * 3 + 2] = max(min(Y + 2.03211*U, 255), 0)
            self.ser.setTimeout(oldtimeout)
        finally:
            self.lock.release()
        return buffer

    def _grab_array_bilinear_vert(self):
        width = 256
        height = 192
        buffer = array.array('B', [0] * (height * width * 3))
        try:
            self.lock.acquire()
            oldtimeout = self.ser.timeout
            self.ser.setTimeout(.01)
            self.ser.write(chr(Fluke.GET_IMAGE))
            size= width*height
            line = BufferedRead(self.ser, size, start = 0)
            #create the image from the YUV layer
            for i in range(height):
                if i < 1:
                    n = 0
                else:
                    n = 1

                for j in range(width):   
                    if j >= 3:
                        # go to the left for other values
                        #vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3
                        vy = -1; vu = -2; y1v = -1; y1u = +1; uy = -1; uv = -2; y2u = -1;
                        if j < (width - 2):
                            y2v = +1
                        else:
                            y2v = -3
                    else:
                        # go to the right for other values
                        vy = 1; vu = 2; y1v = 3; y1u = 1; uy = 1; uv = 2; y2u = 3; y2v = 1

                            #   0123 0123 0123
                    if ((j % 4) == 0): #3 #2   VYUY VYUY VYUY
                        V = ord(line[i * width + j])
                        Y = 0.5*ord(line[i * width + j + vy]) + 0.5*ord(line[(i-n) * width + j + vy])
                        U = 0.5*ord(line[i * width + j + vu]) + 0.5*ord(line[(i-n) * width + j + vu])
                    elif ((j % 4) == 1): #0 #3
                        Y = ord(line[i * width + j])
                        V = 0.5*ord(line[i * width + j + y1v]) + 0.5*ord(line[(i-n) * width + j + y1v])
                        U = 0.5*ord(line[i * width + j + y1u]) + 0.5*ord(line[(i-n) * width + j + y1u]) 
                    elif ((j % 4) == 2): #1 #0
                        U = ord(line[i * width + j])
                        Y = 0.5*ord(line[i * width + j + uy]) + 0.5*ord(line[(i-n) * width + j + uy])
                        V = 0.5*ord(line[i * width + j + uv]) + 0.5*ord(line[(i-n) * width + j + uv])
                    elif ((j % 4) == 3): #2 #1
                        Y = ord(line[i * width + j])
                        U = 0.5*ord(line[i * width + j + y2u]) + 0.5*ord(line[(i-n) * width + j + y2u])
                        V = 0.5*ord(line[i * width + j + y2v]) + 0.5*ord(line[(i-n) * width + j + y2v])
                    U = U - 128
                    V = V - 128
                    Y = Y
                    buffer[(i * width + j) * 3 + 0] = max(min(Y + 1.13983 * V, 255), 0)
                    buffer[(i * width + j) * 3 + 1] = max(min(Y - 0.39466*U-0.58060*V, 255), 0)
                    buffer[(i * width + j) * 3 + 2] = max(min(Y + 2.03211*U, 255), 0)
            self.ser.setTimeout(oldtimeout)
        finally:
            self.lock.release()
        return buffer

    def getBattery(self):
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.GET_BATTERY))
            retval = read_2byte(self.ser) / 20.9813
        finally:
            self.lock.release()
        return retval
    
    def setBrightPower(self, power):
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.SET_DONGLE_IR))
            self.ser.write(chr(power))
        finally:
            self.lock.release()

    def setLEDFront(self, value):
        value = int(min(max(value, 0), 1))
        try:
            self.lock.acquire()
            if isTrue(value):
                self.ser.write(chr(Fluke.SET_DONGLE_LED_ON))
            else:
                self.ser.write(chr(Fluke.SET_DONGLE_LED_OFF))
        finally:
            self.lock.release()

    def setLEDBack(self, value):
        if value > 1:
            value = 1
        elif value <= 0:
            value = 0
        else:
            value = int(float(value) * (255 - 170) + 170) # scale
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.SET_DIMMER_LED))
            self.ser.write(chr(value))
        finally:
            self.lock.release()

    def getObstacle(self, value=None):
        if value == None:
            return self.get("obstacle")
        try:            
            self.lock.acquire()
            if value in ["left", 0]:
                self.ser.write(chr(Fluke.GET_DONGLE_L_IR))
            elif value in ["middle", "center", 1]:
                self.ser.write(chr(Fluke.GET_DONGLE_C_IR))
            elif value in ["right", 2]:
                self.ser.write(chr(Fluke.GET_DONGLE_R_IR))
            retval = read_2byte(self.ser)
        finally:
            self.lock.release()
        return retval       

    def getBright(self, window=None):
        # left, middle, right

        # assumes this configuartion of the windows
        # conf_gray_window(self.ser, 0, 0, 0,    84, 191, 1, 1)
        # conf_gray_window(self.ser, 1, 84,  0, 170, 191, 1, 1)
        # conf_gray_window(self.ser, 2, 170, 0, 254, 191, 1, 1)

        if window == None or window == "all":
            return self.get("bright")
        if type(window) == str:
            if window in ["left"]:
                window = 0
            elif window in ["middle", "center"]:                
                window = 1
            elif window in ["right"]:
                window = 2
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.GET_WINDOW_LIGHT))
            self.ser.write(chr(window))
            retval = read_3byte(self.ser) #/ (63.0 * 192.0 * 255.0)
        finally:
            self.lock.release()
        return retval 

    def getBlob(self):
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.GET_BLOB))
            #self.ser.write(chr(window))
            numpixs = read_2byte(self.ser)
            xloc = ord(self.ser.read(1))
            yloc = ord(self.ser.read(1))
        finally:
            self.lock.release()
        return (numpixs, xloc, yloc)

    def setForwardness(self, direction):
        if direction in ["fluke-forward", 1]:
            direction = 1
        elif direction in ["fluke-forward", 0]:
            direction = 0
        else:
            raise AttributeError("unknown direction: '%s': should be 'fluke-forward' or 'fluke-forward'" % direction)
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.SET_FORWARDNESS))
            self.ser.write(chr(direction))
        finally:
            self.lock.release()

    def setIRPower(self, power):
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.SET_DONGLE_IR))
            self.ser.write(chr(power))
        finally:
            self.lock.release()

    def setWhiteBalance(self, value):
        try:
            self.lock.acquire()
            if isTrue(value):
                self.ser.write(chr(Fluke.SET_WHITE_BALANCE))
            else:
                self.ser.write(chr(Fluke.SET_NO_WHITE_BALANCE))
        finally:
            self.lock.release()
    
    def reboot(self):
        try:
            self.lock.acquire()
            self.ser.write(chr(Fluke.SET_RESET_SCRIBBLER))
        finally:
            self.lock.release()

    def set_cam_param(self, addr, byte):
        try:
            self.lock.acquire()
            self.ser.write(chr(self.SET_CAM_PARAM))
            self.ser.write(chr(addr))
            self.ser.write(chr(byte))
            time.sleep(.15) # camera needs time to reconfigure
        finally:
            self.lock.release()
    
    def get_cam_param(self, addr):
        try:
            self.lock.acquire()
            self.ser.write(chr(self.GET_CAM_PARAM))
            self.ser.write(chr(addr))
            v = ord(self.ser.read(1)) 
        finally:
            self.lock.release()
        return v

    def darkenCamera(self, level=0):
        if self.debug:
            print "Turning off White Balance, Gain Control, and Exposure Control", level

            
        self.set_cam_param(self.CAM_COMA, self.CAM_COMA_WHITE_BALANCE_OFF)
        self.set_cam_param(self.CAM_COMB,
                           (self.CAM_COMB_GAIN_CONTROL_OFF & self.CAM_COMB_EXPOSURE_CONTROL_OFF))
        self.set_cam_param(0, level)
        self.set_cam_param(1, 0)
        self.set_cam_param(2, 0)
        self.set_cam_param(6, 0)    
        self.set_cam_param(0x10, 0)

    def autoCamera(self):

        if self.debug:
            print "Turning on White Balance, Gain Control, and Exposure Control"

        self.set_cam_param(0, 0)
        self.set_cam_param(1, 0x80)
        self.set_cam_param(2, 0x80)
        self.set_cam_param(6, 0x80)
        self.set_cam_param(0x10, 0x41)
        self.set_cam_param(self.CAM_COMA, self.CAM_COMA_DEFAULT)
        self.set_cam_param(self.CAM_COMB, self.CAM_COMB_DEFAULT)

        
    ########################################################## End Dongle Commands

    def setData(self, position, value):
        data = self._get(Fluke.GET_DATA, 8)
        data[position] = value
        return self._set(*([Fluke.SET_DATA] + data))

    def setSingleData(self,position,value):
        data = [position,value]
        return self._set(  *([Fluke.SET_SINGLE_DATA] + data)  )

    def setEchoMode(self, value):
        if isTrue(value): self._set(Fluke.SET_ECHO_MODE, 1)
        else:             self._set(Fluke.SET_ECHO_MODE, 0)
        time.sleep(.25)
        self.ser.flushInput()
        self.ser.flushOutput()
        return

    def set(self, item, position, value = None):
        item = item.lower()
        if item == "led":
            if type(position) in [int, float]:
                if position == 0:
                    if isTrue(value): return self._set(Fluke.SET_LED_LEFT_ON)
                    else:             return self._set(Fluke.SET_LED_LEFT_OFF)
                elif position == 1:
                    if isTrue(value): return self._set(Fluke.SET_LED_CENTER_ON)
                    else:             return self._set(Fluke.SET_LED_CENTER_OFF)
                elif position == 2:
                    if isTrue(value): return self._set(Fluke.SET_LED_RIGHT_ON)
                    else:             return self._set(Fluke.SET_LED_RIGHT_OFF)
                else:
                    raise AttributeError("no such LED: '%s'" % position)
            else:
                position = position.lower()
                if position == "center":
                    if isTrue(value): return self._set(Fluke.SET_LED_CENTER_ON)
                    else:             return self._set(Fluke.SET_LED_CENTER_OFF)
                elif position == "left":
                    if isTrue(value): return self._set(Fluke.SET_LED_LEFT_ON)
                    else:             return self._set(Fluke.SET_LED_LEFT_OFF)
                elif position == "right":
                    if isTrue(value): return self._set(Fluke.SET_LED_RIGHT_ON)
                    else:             return self._set(Fluke.SET_LED_RIGHT_OFF)
                elif position == "front":
                    return self.setLEDFront(value)
                elif position == "back":
                    return self.setLEDBack(value)
                elif position == "all":
                    if isTrue(value): return self._set(Fluke.SET_LED_ALL_ON)
                    else:             return self._set(Fluke.SET_LED_ALL_OFF)
                else:
                    raise AttributeError("no such LED: '%s'" % position)
        elif item == "whitebalance":
            self.setWhiteBalance(position)
        elif item == "irpower":
            self.setIRPower(position)
        elif item == "echomode":
            return self.setEchoMode(position)
        else:
            raise ("invalid set item name: '%s'" % item)
   
    def stop(self):
        pass

####################### Private

    def _read(self, bytes = 1):
        
        if self.debug:
            print "Trying to read", bytes, "bytes", "timeout =", self.ser.timeout

        c = self.ser.read(bytes)
        
        if self.debug:
            print "Initially read", len(c), "bytes:",
            print map(lambda x:"0x%x" % ord(x), c)
            
        # .nah. bug fix
        while (bytes > 1 and len(c) < bytes):      
            c = c + self.ser.read(bytes-len(c))
            if self.debug:
                print map(lambda x:"0x%x" % ord(x), c)

        # .nah. end bug fix
        if self.debug:
            print "_read (%d)" % len(c)
            print map(lambda x:"0x%x" % ord(x), c)

        if self.dongle == None:
            time.sleep(0.01) # HACK! THIS SEEMS TO NEED TO BE HERE!
        if bytes == 1:
            x = -1
            if (c != ""):
                x = ord(c)            
            elif self.debug:
                print "timeout!"
                return x
        else:
            return map(ord, c)

def cap(c):
    if (c > 255): 
        return 255
    if (c < 0):
        return 0

    return c

def conf_window(ser, window, X_LOW, Y_LOW, X_HIGH, Y_HIGH, X_STEP, Y_STEP):
    ser.write(chr(Fluke.SET_WINDOW))
    ser.write(chr(window)) 
    ser.write(chr(X_LOW)) 
    ser.write(chr(Y_LOW)) 
    ser.write(chr(X_HIGH))
    ser.write(chr(Y_HIGH))
    ser.write(chr(X_STEP))
    ser.write(chr(Y_STEP))

def conf_gray_window(ser, window, lx, ly, ux, uy, xstep, ystep):
    # Y's are on odd pixels
    if (lx % 2)== 0:
        lx += 1
    if (xstep % 2) == 1:
        xstep += 1
    conf_window(ser, window, lx, ly, ux, uy, xstep, ystep)

def conf_gray_image(ser):
    # skip every other pixel
    conf_window(ser, 0, 1, 0, 255, 191, 2, 2)
    
def grab_rle_on(ser):
    """
    Returns a list of pixels that match.
    """
    print "RLE"
    width = 256
    height = 192    
    blobs = zeros(((height + 1), (width + 1)), dtype=uint8)
    on_pxs = []
    line = ''
    ser.write(chr(Fluke.GET_RLE))
    size=ord(ser.read(1))
    size = (size << 8) | ord(ser.read(1))
    if self.debug:
        print "Grabbing RLE image size =", size
    line =''
    while (len(line) < size):
        line+=ser.read(size-len(line))
    px = 0
    counter = 0
    val = 128
    inside = True
    for i in range(0, height, 1):
        for j in range(0, width, 4):            
            if (counter < 1 and px < len(line)):
                counter = ord(line[px])            
                px += 1
                counter = (counter << 8) | ord(line[px])            
                px += 1

                if (inside):
                    val = 0
                    inside = False
                else:
                    val = 255
                    inside = True

            for z in range(0,4):
                blobs[i][j+z] = val
                if (inside):
                    on_pxs += [[j+z, i]]
            counter -= 1
    return on_pxs

def read_2byte(ser):
    hbyte = ord(ser.read(1))
    lbyte = ord(ser.read(1))
    lbyte = (hbyte << 8) | lbyte
    return lbyte

def read_3byte(ser):
    hbyte = ord(ser.read(1))
    mbyte = ord(ser.read(1))
    lbyte = ord(ser.read(1))
    lbyte = (hbyte << 16)| (mbyte << 8) | lbyte
    return lbyte

def write_2byte(ser, value):
    ser.write(chr((value >> 8) & 0xFF))
    ser.write(chr(value & 0xFF))

def read_mem(ser, page, offset):
    ser.write(chr(Fluke.GET_SERIAL_MEM))
    write_2byte(ser, page)
    write_2byte(ser, offset)
    return ord(ser.read(1))

def write_mem(ser, page, offset, byte):
    ser.write(chr(Fluke.SET_SERIAL_MEM))
    write_2byte(ser, page)
    write_2byte(ser, offset)
    ser.write(chr(byte))

def erase_mem(ser, page):
    ser.write(chr(Fluke.SET_SERIAL_ERASE))
    write_2byte(ser, page)

# Also copied into system.py:
def set_fluke_memory(ser, offset, byte):
    ser.write(chr(Fluke.SET_SCRIB_PROGRAM))
    write_2byte(ser, offset)
    ser.write(chr(byte))
    
def set_fluke_start_program(ser, size):
    ser.write(chr(Fluke.SET_START_PROGRAM))
    write_2byte(ser, size)
            
def get_window_avg(ser, window):
    ser.write(chr(Fluke.GET_WINDOW_LIGHT))
    ser.write(chr(window))
    return read_2byte(ser)

def quadrupleSize(line, width):
    retval = [" "] * len(line) * 4
    col = 0
    row = 0
    for c in line:
        retval[row       * 2 * width + col]   = c
        retval[row       * 2 * width + col+1] = c
        retval[(row + 1) * 2 * width + col]   = c
        retval[(row + 1) * 2 * width + col+1] = c
        col += 2
        if col == width * 2:
            col = 0
            row += 2
    return "".join(retval)

def set_ir_power(ser, power):
    ser.write(chr(Fluke.SET_DONGLE_IR))
    ser.write(chr(power))

def yuv2rgb(Y, U, V):
    R = int(Y + (1.4075 * (V - 128)))
    G = int(Y - (0.3455 * (U - 128)) - (0.7169 * (V - 128)))
    B = int(Y + (1.7790 * (U - 128)))
    return [max(min(v,255),0) for v in (R, G, B)]

def rgb2yuv(R, G, B):
    Y = int(0.299 * R + 0.587 * G + 0.114 * B)
    U = int(-0.14713 * R - 0.28886 * G + 0.436 * B + 128)
    V = int( 0.615 * R - 0.51499* G - 0.10001 * B + 128)
    return [max(min(v,255),0) for v in (Y, U, V)]


