"""
Myro code for the Scribbler robot from Parallax
(c) 2007, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Keith O'Hara and Doug Blank"

import serial, time, string
from threading import Lock
from myro import Robot, ask, askQuestion, _update_gui
import myro.globvars

def _commport(s):
    if type(s) == int: return 1
    if type(s) == str:
        s = s.replace('\\', "")
        s = s.replace('.', "")
        if s.lower().startswith("com") and s[3:].isdigit():
            return 1
        if s.startswith("/dev/"):
            return 1
    return 0

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
    SOFT_RESET        = 33  

    GET_ALL           = 65  
    GET_ALL_BINARY    = 66  
    GET_LIGHT_LEFT    = 67  
    GET_LIGHT_CENTER  = 68  
    GET_LIGHT_RIGHT   = 69  
    GET_LIGHT_ALL     = 70  
    GET_IR_LEFT       = 71  
    GET_IR_RIGHT      = 72  
    GET_IR_ALL        = 73  
    GET_LINE_LEFT     = 74  
    GET_LINE_RIGHT    = 75  
    GET_LINE_ALL      = 76  
    GET_STATE         = 77  
    GET_NAME1         = 78  
    GET_NAME2         = 87  
    GET_STALL         = 79  
    GET_INFO          = 80  
    GET_DATA          = 81

    #Camera Dongle
    GET_RLE=82  # a segmented and run-length encoded image
    GET_IMAGE=83  # the entire 256 x 192 image in YUYV format
    GET_WINDOW=84  # the windowed image (followed by which window)
    GET_DONGLE_L_IR=85  # number of returned pulses when left emitter is turned on
    GET_DONGLE_C_IR=86  # number of returned pulses when center emitter is turned on
    GET_DONGLE_R_IR=87  # number of returned pulses when right emitter is turned on
    GET_WINDOW_LIGHT=88    # average intensity in the user defined region
    GET_BATTERY=89  # battery voltage
    GET_SERIAL_MEM=90  # with the address returns the value in serial memory
    GET_SCRIB_PROGRAM=91  # with offset, returns the scribbler program buffer
    GET_CAM_PARAM=92 # with address, returns the camera parameter at that address


    SET_SINGLE_DATA   = 96
    SET_DATA          = 97  
    SET_ECHO_MODE     = 98  
    SET_LED_LEFT_ON   = 99
    SET_LED_LEFT_OFF  = 100
    SET_LED_CENTER_ON = 101
    SET_LED_CENTER_OFF= 102
    SET_LED_RIGHT_ON  = 103
    SET_LED_RIGHT_OFF = 104
    SET_LED_ALL_ON    = 105
    SET_LED_ALL_OFF   = 106
    SET_LED_ALL       = 107 
    SET_MOTORS_OFF    = 108 
    SET_MOTORS        = 109 
    SET_NAME1         = 110 
    SET_NAME2         = 119 
    SET_LOUD          = 112
    SET_QUIET         = 113
    SET_SPEAKER       = 114 
    SET_SPEAKER_2     = 115

    #Camera Dongle
    SET_DONGLE_LED_ON=116   # turn binary dongle led on
    SET_DONGLE_LED_OFF=117  # turn binary dongle led off
    SET_RLE=118             # set rle parameters 
    SET_NAME2=119           # set name2 byte
    SET_DONGLE_IR=120       # set dongle IR power
    SET_SERIAL_MEM=121      # set serial memory byte
    SET_SCRIB_PROGRAM=122   # set scribbler program memory byte
    SET_START_PROGRAM=123   # initiate scribbler programming process
    SET_RESET_SCRIBBLER=124 # hard reset scribbler
    SET_SERIAL_ERASE=125    # erase serial memory
    SET_DIMMER_LED=126      # set dimmer led
    SET_WINDOW=127          # set user defined window
    SET_FORWARDNESS=128     # set direction of scribbler
    SET_WHITE_BALANCE=129   # turn on white balance on camera 
    SET_NO_WHITE_BALANCE=130 # diable white balance on camera (default)
    SET_CAM_PARAM=131       # with address and value, sets the camera parameter at that address

    PACKET_LENGTH     =  9
    
    def __init__(self, serialport = None, baudrate = 38400):
        Robot.__init__(self)
        self.requestStop = 0
        self.addService("audio", "type", "onboard")
        self.addService("sensor.stall", "type", "digital")
        self.addService("sensor.ir", "type", "digital")
        self.addService("sensor.light", "type", "analog")
        self.addService("sensor.line", "type", "digital")
        self.addService("movement", "type", "2d")
        self.debug = 0
        self._lastTranslate = 0
        self._lastRotate    = 0
        self._volume = 0
        self.lock = Lock()
        if serialport == None:
            serialport = ask("Port", useCache = 1)
        # Deal with requirement that Windows "COM#" names where # >= 9 needs to
        # be in the format "\\.\COM#"
        if type(serialport) == str and serialport.lower().startswith("com"):
            portnum = int(serialport[3:])
            if portnum >= 10:
                serialport = r'\\.\COM%d' % (portnum)
        self.serialPort = serialport
        self.baudRate = baudrate
        self.open()
        myro.globvars.robot = self
        self._fudge = range(4)
        self._oldFudge = range(4)
        self.loadFudge()

    def search(self):
        answer = askQuestion(title="Search for " + self.serialPort,
                             question="Press the red resest button on the robot\nPress OK when ready to search",
                             answers = ["OK", "Cancel"])
        if answer != "OK":
            raise KeyboardInterrupt
        for x in range(1, 21):
            if x >= 10:
                port = r'\\.\COM%d' % x
            else:
                port = "COM" + str(x)
            prettyPort = "COM" + str(x)
            print "Searching on port %s for robot named '%s'..." % (prettyPort, self.serialPort)
            try:
                self.ser = serial.Serial(port, timeout=1)
            except KeyboardInterrupt:
                raise
            except serial.SerialException:
                continue
            self.ser.baudrate = self.baudRate
            # assume that it has been running for at least a second!
            time.sleep(1)
            lines = self.ser.readlines()
            lines = ''.join(lines)
            if ("IPRE" in lines):
                position = lines.index("IPRE")
                name = lines[position+4:position + 9 + 4]
                name = name.replace("\x00", "")
                name = name.strip()
                s = port.replace('\\', "")
                s = s.replace('.', "")
                print "   Found robot named", name, "on port", s, "!"
                if name == self.serialPort:
                    self.serialPort = port
                    self.ser.timeout = 2.0
                    s = self.serialPort.replace('\\', "")
                    s = s.replace('.', "")
                    askQuestion("You can use \"%s\" from now on, like this:\n   initialize(\"%s\")" %
                                (s, s), answers=["Ok"])
                    return
                else:
                    self.ser.close()
        raise ValueError, ("Couldn't find the scribbler named " + self.serialPort)
    
    def open(self):
        try:
            myro.globvars.robot.ser.close()
        except KeyboardInterrupt:
            raise
        except:
            pass
        if not _commport(self.serialPort):
            self.search()
        else:
            while 1:
                try:
                    self.ser = serial.Serial(self.serialPort, timeout = 2) 
                    break
                except KeyboardInterrupt:
                    raise
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
        self.restart()

    def close(self):
        self.ser.close()

    def restart(self):
        print "Waking robot from sleep..."
        self.setEchoMode(0) # send command to get out of broadcast; turn off echo
        time.sleep(.25)               # give it some time
        while 1:
            self.ser.flushInput()         # flush "IPREScribby"...
            self.ser.flushOutput()
            time.sleep(1.2)       # give it time to see if another IPRE show up
            if self.ser.inWaiting() == 0: # if none, then we are out of here!
                break
            print "Waking robot from sleep..."
            self.setEchoMode(0) # send command to get out of broadcast; turn off echo
            time.sleep(.25)               # give it some time
        self.ser.flushInput()
        self.ser.flushOutput()
        self.stop()
        self.set("led", "all", "off")
        self.beep(.03, 784)
        self.beep(.03, 880)
        self.beep(.03, 698)
        self.beep(.03, 349)
        self.beep(.03, 523)
        name = self.get("name")
        print "Hello, I'm %s!" % name

    def beep(self, duration, frequency, frequency2 = None):
        if frequency2 == None:
            self._set_speaker(int(frequency), int(duration * 1000))
        else:
            self._set_speaker_2(int(frequency), int(frequency2), int(duration * 1000))

    def get(self, sensor = "all", *position):
        sensor = sensor.lower()
        if sensor == "config":
            return {"ir": 2, "line": 2, "stall": 1, "light": 3}
        elif sensor == "stall":
            retval = self._get(Scribbler.GET_ALL, 11) # returned as bytes
            self._lastSensors = retval # single bit sensors
            return retval[10]
        elif sensor == "startsong":
            #TODO: need to get this from flash memory
            return "tada"
        elif sensor == "version":
            #TODO: just return this version for now; get from flash
            return __REVISION__.split()[1]
        elif sensor == "data":
            return self.getData(*position)
        elif sensor == "info":
            return self.getInfo(*position)
        elif sensor == "name":
            c = self._get(Scribbler.GET_NAME1, 8)
            c += self._get(Scribbler.GET_NAME2, 8)
            c = string.join([chr(x) for x in c if "0" <= chr(x) <= "z"], '').strip()
            return c
        elif sensor == "volume":
            return self._volume
        else:
            if len(position) == 0:
                if sensor == "light":
                    return self._get(Scribbler.GET_LIGHT_ALL, 6, "word")
                elif sensor == "ir":
                    return self._get(Scribbler.GET_IR_ALL, 2)
                elif sensor == "line":
                    return self._get(Scribbler.GET_LINE_ALL, 2)
                elif sensor == "all":
                    retval = self._get(Scribbler.GET_ALL, 11) # returned as bytes
                    self._lastSensors = retval # single bit sensors
                    return {"light": [retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], retval[6] << 8 | retval[7]],
                            "ir": [retval[0], retval[1]], "line": [retval[8], retval[9]], "stall": retval[10]}
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            retvals = []
            for pos in position:
                if sensor == "light":
                    values = self._get(Scribbler.GET_LIGHT_ALL, 6, "word")
                    if pos in [0, "left"]:
                        retvals.append(values[0])
                    elif pos in [1, "middle", "center"]:
                        retvals.append(values[1])
                    elif pos in [2, "right"]:
                        retvals.append(values[2])
                elif sensor == "ir":
                    values = self._get(Scribbler.GET_IR_ALL, 2)
                    if pos in [0, "left"]:
                        retvals.append(values[0])
                    elif pos in [1, "right"]:
                        retvals.append(values[1])
                elif sensor == "line":
                    values = self._get(Scribbler.GET_LINE_ALL, 2)
                    if pos in [0, "left"]:
                        retvals.append(values[0])
                    elif pos in [1, "right"]:
                        retvals.append(values[1])
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 0:
                return None
            elif len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def getData(self, *position):
        if len(position) == 0: 
            return self._get(Scribbler.GET_DATA, 8)
        else:   
            retval = []               
            for p in position:
                retval.append(self._get(Scribbler.GET_DATA, 8)[p])
            if len(retval) == 1:
                return retval[0]
            else:
                return retval

    def getInfo(self, *item):
        retval = self._get(Scribbler.GET_INFO, mode="line")
        retDict = {}
        for pair in retval.split(","):
            it, value = pair.split(":")
            retDict[it.lower().strip()] = value.strip()
        if len(item) == 0:  
            return retDict
        else:               
            retval = []
            for it in item:
                retval.append(retDict[it.lower().strip()])
            if len(retval) == 1:
                return retval[0]
            else:
                return retval

    def setData(self, position, value):
        data = self._get(Scribbler.GET_DATA, 8)
        data[position] = value
        return self._set(*([Scribbler.SET_DATA] + data))

    def setSingleData(self,position,value):
        data = [position,value]
        return self._set(  *([Scribbler.SET_SINGLE_DATA] + data)  )

    def setEchoMode(self, value):
        if isTrue(value): return self._set(Scribbler.SET_ECHO_MODE, 1)
        else:             return self._set(Scribbler.SET_ECHO_MODE, 0)

    def set(self, item, position, value = None):
        _update_gui()
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
                    else:             return self._set(Scribbler.SET_LED_RIGHT_OFF)
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
        elif item == "name":
            position = position + (" " * 16)
            name1 = position[:8].strip()
            name1_raw = map(lambda x:  ord(x), name1)
            name2 = position[8:16].strip()
            name2_raw = map(lambda x:  ord(x), name2)
            self._set(*([Scribbler.SET_NAME1] + name1_raw))
            self._set(*([Scribbler.SET_NAME2] + name2_raw))
        elif item == "volume":
            if isTrue(position):
                self._volume = 1
                return self._set(Scribbler.SET_LOUD)
            else:
                self._volume = 0
                return self._set(Scribbler.SET_QUIET)
        elif item == "startsong":
            self.startsong = position
        elif item == "echomode":
            return self.setEchoMode(position)
        elif item == "data":
            return self.setData(position, value)
        else:
            raise ("invalid set item name: '%s'" % item)

   
    # Sets the fudge values (in memory, and on the flash memory on the robot)
    def setFudge(self,f1,f2,f3,f4):
        self._fudge[0] = f1
        self._fudge[1] = f2
        self._fudge[2] = f3
        self._fudge[3] = f4

    	# Save the fudge data (in integer 0..255 form) to the flash memory
	#f1-f4 are float values 0..2, convert to byte values
	# But to make things quick, only save the ones that have changed!
	# 0..255 and save.

        if self._oldFudge[0] != self._fudge[0] :
                self.setSingleData(0,  int(self._fudge[0] * 127.0) )
                self._oldFudge[0] = self._fudge[0] 

        if self._oldFudge[1] != self._fudge[1] :
                self.setSingleData(1,  int(self._fudge[1] * 127.0) )
                self._oldFudge[1] = self._fudge[1]


        if self._oldFudge[2] != self._fudge[2]:
                self.setSingleData(2,  int(self._fudge[2] * 127.0) )
                self._oldFudge[2] = self._fudge[2]
        
        if self._oldFudge[3] != self._fudge[3] :
                self.setSingleData(3,  int(self._fudge[3] * 127.0) )
                self._oldFudge[3] = self._fudge[3]
                
    
   #Called when robot is initialized, after serial connection is established.
   # Checks to see if the robot has fudge factors saved in it's data area
   # 0,1,2,3, and uses them. If the robot has zeros, it replaces them with 127
   # which is the equivalent of no fudge. Each factor goes from 0..255, where
   # a 127 is straight ahead (no fudging)
    def loadFudge(self):
        for i in range(4):
            self._fudge[i] = self.get("data",i)
            if self._fudge[i] == 0:
                    self._fudge[i] = 127
            self._fudge[i] = self._fudge[i] / 127.0 # convert back to floating point!

    #Gets the fudge values (from memory, so we don't get penalized by a slow
    # serial link)
    def getFudge(self):
        return(self._fudge[0],self._fudge[1],self._fudge[2],self._fudge[3])

    def stop(self):
        self._lastTranslate = 0
        self._lastRotate = 0
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

    def getLastSensors(self):
        retval = self._lastSensors
        return {"light": [retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], retval[6] << 8 | retval[7]],
                "ir": [retval[0], retval[1]], "line": [retval[8], retval[9]], "stall": retval[10]}

    def update(self):
        pass

####################### Private

    def _adjustSpeed(self):
        left  = min(max(self._lastTranslate - self._lastRotate, -1), 1)
        right  = min(max(self._lastTranslate + self._lastRotate, -1), 1)



	# JWS additions for "calibration" of motors.
	# Use fudge values 1-4 to change the actual power given to each
	# motor based upon the forward speed.
	#
	# This code is here for documentation purposes only.
	# 
	# The algorithm shown here is now implemented on the basic stamp
	# on the scribbler directly.

	#fudge the left motor when going forward!
	#if (self._fudge[0] > 1.0 and left > 0.5 ):
	   #left = left - (self._fudge[0] - 1.0)
	#if (self._fudge[1] > 1.0 and 0.5 >= left > 0.0):
	   #left = left - (self._fudge[1] - 1.0)
	#fudge the right motor when going forward!
	#if (self._fudge[0] < 1.0 and right > 0.5):
	  # right = right - (1.0 - self._fudge[0])
	#if (self._fudge[1] < 1.0 and 0.5 >= right > 0.0):
	   #right = right - (1.0 - self._fudge[1])

	#Backwards travel is just like forwards travel, but reversed!
	#fudge the right motor when going backwards.
	#if (self._fudge[2] > 1.0 and 0.0 > right >= -0.5):
	#	right = right + (self._fudge[2] - 1.0)
	#if (self._fudge[3] > 1.0 and -0.5 > right ):
	#	right = right + (self._fudge[3] - 1.0)

	#fudge the left motor when going backwards.
	#if (self._fudge[2] < 1.0 and 0.0 > left >= -0.5):
	#	left = left + (1.0 - self._fudge[2]) 
	#if (self._fudge[3] < 1.0 and -0.5 > left):
	#	left = left + (1.0 - self._fudge[3])

  

	#print "actual power: (",left,",",right,")"

	#end JWS additions for "calibration of motors.
        leftPower = (left + 1.0) * 100.0
        rightPower = (right + 1.0) * 100.0
        self._set(Scribbler.SET_MOTORS, rightPower, leftPower)

    def _read(self, bytes = 1):
        c = self.ser.read(bytes)
        # .nah. bug fix
        while (bytes > 1 and len(c) < bytes):      
            c = c + self.ser.read(bytes-len(c))
        # .nah. end bug fix
        if self.debug: print "_read:", c, len(c)
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

    def _write(self, rawdata):
        _update_gui()
        t = map(lambda x: chr(int(x)), rawdata)
        data = string.join(t, '') + (chr(0) * (Scribbler.PACKET_LENGTH - len(t)))[:9]
        if self.debug: print "_write:", data, len(data)
        time.sleep(0.01) # HACK! THIS SEEMS TO NEED TO BE HERE!
        self.ser.write(data)      # write packets

    def _set(self, *values):
        self.lock.acquire()
        self._write(values)
        self._read(Scribbler.PACKET_LENGTH) # read echo
        self._lastSensors = self._read(11) # single bit sensors
        self.ser.flushInput()
        if self.requestStop:
            self.requestStop = 0
            self.stop()
            raise KeyboardInterrupt
        self.lock.release()

    def _get(self, value, bytes = 1, mode = "byte"):
        self.lock.acquire()
        self._write([value])
        self._read(Scribbler.PACKET_LENGTH) # read the echo
        if mode == "byte":
            retval = self._read(bytes)
        elif mode == "word":
            retvalBytes = self._read(bytes)
            retval = []
            for p in range(0,len(retvalBytes),2):
                retval.append(retvalBytes[p] << 8 | retvalBytes[p + 1])
        elif mode == "line": # until hit \n newline
            retval = self.ser.readline()
        self.ser.flushInput()
        self.lock.release()
        return retval

    def _set_speaker(self, frequency, duration):
        return self._set(Scribbler.SET_SPEAKER, 
             duration >> 8,
                         duration % 256,
                         frequency >> 8,
                         frequency % 256)

    def _set_speaker_2(self, freq1, freq2, duration):
        return self._set(Scribbler.SET_SPEAKER_2, 
                         duration >> 8,
                         duration % 256,
                         freq1 >> 8,
                         freq1 % 256,
                         freq2 >> 8,
                         freq2 % 256)
    
