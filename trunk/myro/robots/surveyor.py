"""
Surveyor SVR-1 Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Doug"

import time, string
try:
    import serial
except:
    print "WARNING: pyserial not loaded: surveyor won't work!"
from myro import Robot, ask
import myro.globvars
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

def hex2dec(hex, base = 16):
    """ Like hex(), but to string """
    hex = hex.upper()
    retval = 0
    chars = "0123456789ABCDEF"
    for count in range(len(hex)):
        value = base ** count
        c = hex[-(count + 1)]
        if c in chars:
            retval += chars.index(c) * value
    return retval

def dec2hex(num, positions = 2, base = 16):
    """ Like chr(), but as string """
    retval = ""
    chars = "0123456789ABCDEF"
    for count in range(positions - 1, -1, -1):
        value = base ** count
        digit = chars[int(num / value)] # int division
        retval += digit
        num -= int(num/value) * value
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

class CameraWindow(Tkinter.Toplevel):
    def __init__(self, robot):
        self.robot = robot
        self.delay = .1
        Tkinter.Toplevel.__init__(self, myro.globvars.gui)
        self.wm_title("SRV-1 View (%dx%d)" % self.robot.resolution)
        self.canvas = Tkinter.Canvas(self, width = 160, height = 128)
        self.canvas.pack(fill="both", expand="y")
        self.canvas.bind("<B1-Motion>", func=lambda event=self:self.dispatch_event(event, "motion", 0))
        self.canvas.bind("<Button-1>",  func=lambda event=self:self.dispatch_event(event, "down", 0))
        self.canvas.bind("<ButtonRelease-1>", func=lambda event=self:self.dispatch_event(event, "up", 0))
        self.canvas.bind("<B2-Motion>", func=lambda event=self:self.dispatch_event(event, "motion", 1))
        self.canvas.bind("<Button-2>",  func=lambda event=self:self.dispatch_event(event, "down", 1))
        self.canvas.bind("<ButtonRelease-2>", func=lambda event=self:self.dispatch_event(event, "up", 1))
        self.canvas.bind("<B3-Motion>", func=lambda event=self:self.dispatch_event(event, "motion", 2))
        self.canvas.bind("<Button-3>",  func=lambda event=self:self.dispatch_event(event, "down", 2))
        self.canvas.bind("<ButtonRelease-3>", func=lambda event=self:self.dispatch_event(event, "up", 2))
        self.protocol('WM_DELETE_WINDOW',self.destroy)
        self.click_start = (0,0)
        self.blob = []
        self.update()

    def updateScan(self, scan):
        self.canvas.delete("scan")
        for c in range(len(scan) - 1):
            s1 = scan[c]
            s2 = scan[c + 1]
            pos = c * 2
            self.canvas.create_line(pos    ,
                                    self.robot.resolution[1] - s1 * self.robot.resolution[1],
                                    pos + 2,
                                    self.robot.resolution[1] - s2 * self.robot.resolution[1],
                                    fill="yellow", tag="scan")
            self.canvas.create_line(pos    ,
                                    self.robot.resolution[1] - s1 * self.robot.resolution[1] - 1,
                                    pos + 2,
                                    self.robot.resolution[1] - s2 * self.robot.resolution[1] - 1,
                                    fill="black", tag="scan")

    def destroy(self):
        self.running = 0
        Tkinter.Toplevel.destroy(self)

    def dispatch_event(self, event, action, bin):
        if action == "down":
            self.click_start = event.x, event.y
            x1, y1 = self.click_start
            x2, y2 = self.click_start
            self.canvas.create_rectangle(x1+1, y1+1, x2+1, y2+1,
                                         outline="yellow", tag="box")
            self.canvas.create_rectangle(x1, y1, x2, y2,
                                         outline="blue", tag="box")
        elif action == "motion":
            self.canvas.delete("box")
            x1, y1 = self.click_start
            x2, y2 = event.x, event.y
            self.canvas.create_rectangle(x1+1, y1+1, x2+1, y2+1,
                                         outline="yellow", tag="box")
            self.canvas.create_rectangle(x1, y1, x2, y2,
                                         outline="blue", tag="box")
        elif action == "up":
            self.canvas.delete("box")
            x1, y1 = self.click_start
            x2, y2 = event.x, event.y
            # divide by 2 because displayed image is 160x128
            # and color sampling is in 80x64
            x1 = dec2hex(x1/2)
            x2 = dec2hex(x2/2)
            y1 = dec2hex((self.robot.resolution[1] - y1)/2)
            y2 = dec2hex((self.robot.resolution[1] - y2)/2)
            message = "vg%s%s%s%s%s" % (bin, x1, x2, y2, y1)
            # print message
            self.robot._send(message)
            # returns YUV as y1, y2, u1, u2, v1, v2
            print "Stored tracking colors in location %d" % bin
            if bin not in self.blob:
                self.blob.append( bin )
        else:
            print "unknown mouse action:", action

    def minorloop(self, delay = None): # in milliseconds
        """
        As opposed to mainloop. This is a simple loop that works
        in IDLE.
        """
        if delay != None:
            self.delay = delay
        self.running = 1
        while self.robot and self.running:
            self.update()
            time.sleep(self.delay)

    def update(self, image = None):
        if image == None:
            image = self.robot.getImage() # returns jpeg string
        fileThing = StringIO.StringIO(image)
        try:
            self.im = Image.open(fileThing)
        except IOError:
            return
        self.image = ImageTk.PhotoImage(self.im)
        self.canvas.delete("all")
        self.canvas.create_image(80, 64, image = self.image)
        colors = ["red", "green", "blue"]
        for bin in self.blob:
            color = colors[bin]
            try:
                x1, y1, x2, y2, count = self.robot._send("vb%d" % bin)
            except:
                x1, y1, x2, y2, count = 0,0,0,0,0
            x1 = x1 * 2
            x2 = x2 * 2
            y1 = self.robot.resolution[1] - y1 * 2
            y2 = self.robot.resolution[1] - y2 * 2
            self.canvas.create_rectangle(x1, y1, x2, y2,
                                         outline=color, tag="box")
        Tkinter.Toplevel.update(self)

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
        if type(serialport) == str and serialport.lower().startswith("com"):
            portnum = int(serialport[3:])
            if portnum >= 10:
                serialport = r'\\.\COM%d' % (portnum)
        self.serialPort = serialport
        self.baudRate = baudrate
        self.window = None
        self.id = None
        self.name = "SRV-1"
        self.open()
        myro.globvars.robot = self

    def watch(self, continuous=1):
        self.window = CameraWindow(self)
        if continuous:
            self.window.minorloop()

    def open(self):
        try:
            myro.globvars.robot.ser.close()
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
        if sensor == "config":
            return {"ir": 4}
        if sensor == "name":
            return self.name
        elif sensor == "version":
            return self._send("V")
        elif sensor == "resolution":
            return self.resolution
        elif sensor == "all":
            retval = {}
            for s in ["ir"]:
                retval[s] = self.get(s)
            return retval
        else:
            retvals = []
            if len(position) == 0:
                if sensor == "scan":
                    retval = self._send("S")
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
                    if pos in [0, "front"]:
                        retvals.append(data[0])
                    elif pos in [1, "left"]:
                        retvals.append(data[1])
                    elif pos in [2, "back"]:
                        retvals.append(data[2])
                    elif pos in [3, "right"]:
                        retvals.append(data[3])
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
        if self.window != None:
            self.window.update()

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

    def getBlob(self, bin): # because this isn't in Robot class
        """ Get the blob that matches bin color """
        return self._send("vb%d" % bin)

    def getScan(self, *position): # because this isn't in Robot class
        return self.get("scan", *position)

    def getImage(self, update=0): # because this isn't in Robot class
        i = self._send("I")
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
                    retval[i] = int(data[i * 2:i * 2 + 2].strip(), 16)/64.0
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
                retval = [int(data[0:8].strip(), 16)/255.0,
                          int(data[8:16].strip(), 16)/255.0,
                          int(data[16:24].strip(), 16)/255.0,
                          int(data[24:32].strip(), 16)/255.0 ]
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
        elif message[0] == 'V': # version
            return self.ser.readline()[12:].strip()
        # Vision commands:
        elif message[0:2] == 'vg': # vision grab area
            readline = self.ser.readline()
            if not readline.startswith("##vg"):
                print "error in vg command"
                return None
            readline = readline[4:] # data
            return readline
        elif message[0:2] == 'vr': # vision read area
            readline = self.ser.readline()
            if not readline.startswith("##vr"):
                print "error in vr command"
                return None
            readline = readline[4:] # data
            return readline
        elif message[0:2] == 'vc': # vision set area
            readline = self.ser.readline()
            if not readline.startswith("##vc"):
                print "error in vc command"
                return None
            readline = readline[4:] # data
            return readline
        elif message[0:2] == 'vs': # vision scan
            readline = self.ser.readline()
            if not readline.startswith("##vs"):
                print "error in vs command"
                return None
            readline = readline[4:] # data
            return readline
        elif message[0:2] == 'vb': # vision blob
            readline = self.ser.readline()
            if not readline.startswith("##vb"):
                print "error in vb command"
                return None
            readline = readline[5:] # data
            x1, x2, y1, y2, count = (readline[0:2], readline[2:4],
                                     readline[4:6], readline[6:8],
                                     readline[8:12])
            x1 = hex2dec(x1) 
            x2 = hex2dec(x2) 
            y1 = hex2dec(y1) 
            y2 = hex2dec(y2) 
            count = hex2dec(count) 
            return x1, y1, x2, y2, count
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

def watch():
    if myro.globvars.robot:
        return myro.globvars.robot.watch()
    else:
        raise AttributeError, "need to initialize robot"
