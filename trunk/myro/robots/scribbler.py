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
    GET_STATE          =  1
    GET_OPEN_LEFT      =  2  
    GET_OPEN_RIGHT     =  3
    GET_STALL          =  4  
    GET_LIGHT_LEFT     =  5  
    GET_LIGHT_CENTER   =  6
    GET_LIGHT_RIGHT    =  7
    GET_LINE_RIGHT     =  8  
    GET_LINE_LEFT      =  9
    GET_NAME           = 10 
    GET_LIGHT_ALL      = 33 
    GET_IR_ALL         = 34 
    GET_LINE_ALL       = 35 
    GET_ALL            = 36 
    GET_ALL_BINARY     = 39 
    GET_INFO           = 42 
    GET_DATA           = 44 
    SET_MOTORS_OFF     = 20 
    SET_MOTORS         = 21 
    SET_LED_LEFT_ON    = 22
    SET_LED_LEFT_OFF   = 23
    SET_LED_CENTER_ON  = 24
    SET_LED_CENTER_OFF = 25
    SET_LED_RIGHT_ON   = 26
    SET_LED_RIGHT_OFF  = 27
    SET_SPEAKER        = 28 
    SET_SPEAKER_2      = 29 
    SET_NAME           = 30 
    SET_LED_ALL_ON     = 31
    SET_LED_ALL_OFF    = 32
    SET_LOUD           = 37
    SET_QUIET          = 38
    SET_LED_ALL        = 40 
    SET_DATA           = 43 
    SET_ECHO_MODE      = 45 
    SOFT_RESET         = 41 
    PACKET_LENGTH      =  9
    NAME_LENGTH        =  8
    
    def __init__(self, serialport = None, baudrate = 38400):
        Robot.__init__(self)
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
        myro.globals.robot = self

    def search(self):
        for x in range(20):
            port = "com" + str(x)
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
        try:
            myro.globals.robot.ser.close()
        except:
            pass
        while 1:
            try:
                self.ser = serial.Serial(self.serialPort, timeout = 2) 
                break
            except:
                print "Waiting on port..."
                try:
                    self.ser.close()
                except:
                    pass
                try:
                    del self.ser
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
        self.ser.flushInput()         # flush "IPREScribby"...
        time.sleep(1.25)              # give it time to see if another IPRE show up
        while 1:
            self.setEchoMode(0) # send command to get out of broadcast; turn off echo
            time.sleep(.25)               # give it some time
            self.ser.flushInput()         # flush "IPREScribby"...
            time.sleep(1.25)              # give it time to see if another IPRE show up
            if self.ser.inWaiting() == 0: # if none, then we are out of here!
                break
            print "Waking robot from sleep..."
        self.ser.flushInput()
        self.ser.flushOutput()
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
            c = self._get(Scribbler.GET_NAME, Scribbler.NAME_LENGTH)
            c = string.join([chr(x) for x in c], '').strip()
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

    def setEchoMode(self, value):
        if isTrue(value): return self._set(Scribbler.SET_ECHO_MODE, 1)
        else:             return self._set(Scribbler.SET_ECHO_MODE, 0)

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
        elif item == "name":
            name = position[:8].strip()
            name_raw = map(lambda x:  ord(x), name)
            self._set(*([Scribbler.SET_NAME] + name_raw))
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
        self._set(Scribbler.SET_MOTORS, rightPower, leftPower)

    def _read(self, bytes = 1):
        c = self.ser.read(bytes)
        if self.debug: print "_read:", c, len(c)
        time.sleep(0.001) # HACK! THIS SEEMS TO NEED TO BE HERE!
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
        t = map(lambda x: chr(int(x)), rawdata)
        data = string.join(t, '') + (chr(0) * (Scribbler.PACKET_LENGTH - len(t)))[:9]
        if self.debug: print "_write:", data, len(data)
        self.ser.write(data)      # write packets

    def _set(self, *values):
        self._write(values)
        self._read(Scribbler.PACKET_LENGTH) # read echo
        self._read(11) # single bit sensors

    def _get(self, value, bytes = 1, mode = "byte"):
        self._write([value])
        self._read(Scribbler.PACKET_LENGTH) # read the echo
        if mode == "byte":
            retval = self._read(bytes)
            return retval
        elif mode == "word":
            retval = self._read(bytes)
            newRetval = []
            for p in range(0,len(retval),2):
                newRetval.append(retval[p] << 8 | retval[p + 1])
            return newRetval
        elif mode == "line": # until hit \n newline
            retval = self.ser.readline()
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
    
