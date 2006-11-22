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
import StringIO
import Tkinter
try:
    import ImageTk
    from PIL import Image
except:
    ImageTk = None
    Image = None

def ascii(vec):
    retval = ""
    for v in vec:
        retval += "0123456789#"[int(v * 10)]
    return retval

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

def encode(num):
    if num >= 0: return chr(num)
    else:        return chr(bin2dec(dec2bin(num)))

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
        Robot.__init__(self)
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
            if portnum >= 10:
                serialport = r'\\.\COM%d' % (portnum)
        self.serialPort = serialport
        self.baudRate = baudrate
        self.canvas = None
        self.window = None
        self.id = None
        self.name = "SRV-1"
        self.open()
        myro.globals.robot = self

    def watch(self):
        self.window = Tkinter.Toplevel(myro.globals.gui)
        self.window.wm_title("SRV-1 View (%dx%d)" % self.resolution)
        self.canvas = Tkinter.Canvas(self.window, width = 160, height = 128)
        self.canvas.pack(fill="both", expand="y")

    def open(self):
        try:
            myro.globals.robot.ser.close()
        except:
            pass
        while 1:
            try:
                self.ser = serial.Serial(self.serialPort, timeout = 2) 
                break
            except KeyboardInterrupt:
                raise
            except:
                print "Waiting on port..."
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
        self.ser.flushOutput()
        self.ser.flushInput()
        self.stop() 
        self._send("F") # turn off failsafe
        self._send("m") # disable automovement, samples ground for scans
        self.setResolution((160,128)) # this resolution allows scans
        self.restart()

    def close(self):
        self.ser.close()

    def restart(self):
        print "Hello, I'm %s!" % self.name

    def get(self, sensor = "all", *position):
        sensor = sensor.lower()
        if sensor == "name":
            return self.name
        elif sensor == "version":
            return self._send("V")
        elif sensor == "resolution":
            return self.resolution
        else:
            retvals = []
            if len(position) == 0:
                if sensor == "scan":
                    retval = self._send("S")
                    if self.canvas != None:
                        self.canvas.delete("scan")
                        c = 0
                        for s in retval:
                            self.canvas.create_line(c, self.resolution[1],
                                                    c, self.resolution[1] - s * self.resolution[1],
                                                    fill="yellow", tag="scan")
                            c += 2
                        #print "", # hack to force IDLE to update
                    return retval
                elif sensor == "image":
                    return self.getImage()
                elif sensor == "ir":
                    return self._send("B")
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if sensor == "scan":
                data = self._send("S")
                for pos in position:
                    retvals.append(data[pos])
            elif sensor == "ir":
                data = self._send("B")
                for pos in position:
                    retvals.append(data[pos])
            else:
                raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def set(self, item, position, value = None):
        item = item.lower()
        if item == "name":
            name = position.strip()
            self.name = name
            return "ok"
        elif item == "resolution":
            self.setResolution(position)
            return "ok"
        else:
            raise ("invalid set item name: '%s'" % item)

    def stop(self):
        self._lastTranslate = 0
        self._lastRotate = 0
        self._send("5") # stop; faster than move(0,0)

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

    def setResolution(self, mode = (160, 128)): 
        if mode == (80, 64):
            self._send("a") # no scans available in this mode
        elif mode == (160,128):
            self._send("b")
        elif mode == (320,240):
            self._send("c") # no scans available in this mode
        else:
            raise AttributeError, ("invalid camera resolution:" + str(mode))
        self.resolution = mode
        return "ok"

    def setSwarmMode(self, mode):
        if mode:
            self._send("r") # swarm mode
        else:
            self._send("m") # reset ground, non-autonomous movement
        self.swarmMode = mode

    def sampleGroundColor(self):
        self._send("m") # samples background for obstacle detection
        return "ok"

    def getScan(self, *position): # because this isn't in Robot class
        return self.get("scan", *position)

    def getImage(self): # because this isn't in Robot class
        i = self._send("I")
        if self.canvas != None and ImageTk != None and Image != None:
            try:
                fileThing = StringIO.StringIO(i)
                self.im = Image.open(fileThing)
                self.image = ImageTk.PhotoImage(self.im)
                self.canvas.create_image(80, 64, image = self.image)
                #print "", # hack to get IDLE to update
            except KeyboardInterrupt:
                raise
            except:
                pass
        return i

####################### Private

    def _adjustSpeed(self):
        left  = min(max(self._lastTranslate - self._lastRotate, -1), 1)
        right  = min(max(self._lastTranslate + self._lastRotate, -1), 1)
        leftPower = left * 100.0
        rightPower = right * 100.0
        self._set_motors(leftPower, rightPower)

    def _send(self, message):
        self.ser.write(message + "\n")
        data = None
        if message[0] == 'S':
            try:
                header = self.ser.read(9)
                data   = self.ser.read(160)
                newline   = self.ser.read(1)
                retval = [0 for x in range(80)]
                for i in range(len(retval)):
                    retval[i] = int(data[i * 2:i * 2 + 2])/63.0
            except KeyboardInterrupt:
                raise
            except:
                retval = None
            return retval
        elif message[0] in ['r', 'R']:
            header = self.ser.readline()
            self.id = header[3] + header[4]
        elif message[0] == 'B':
            try:
                header = self.ser.read(13)
                data = self.ser.readline()
                retval = {"front": int(data[0:8].strip(), 16)/255.0,
                          "left": int(data[8:16].strip(), 16)/255.0,
                          "back": int(data[16:24].strip(), 16)/255.0,
                          "right": int(data[24:32].strip(), 16)/255.0}
            except KeyboardInterrupt:
                raise
            except:
                retval = None
            return retval
        elif message[0] == 'I':
            try:
                header = self.ser.read(10)
                resolution = header[5] # 1, 3, 5
                length = (ord(header[6]) * 256 ** 0 +
                          ord(header[7]) * 256 ** 1 +
                          ord(header[8]) * 256 ** 2 +
                          ord(header[9]) * 256 ** 3)
                data   = self.ser.read(length)
                if len(data) != length:
                    raise ValueError, "invalid image data"
            except KeyboardInterrupt:
                raise
            except:
                print "camera image error"
                data = None
            return data
        elif message[0] == 'V':
            return self.ser.readline()[12:].strip()
        else:
            ack = self.ser.read(2)
            if ack != "#" + message[0]:
                print "error reading data:", message, ack
                return 0
            return 1

    def _set_motors(self, motor_left, motor_right):
        ml = int(min(max(motor_left, -100), 100))
        mr = int(min(max(motor_right, -100), 100))
        self._send("M" + encode(ml) + encode(mr) + chr(0))
