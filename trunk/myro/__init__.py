"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__BUILD__    = "$Build: 7 $"
__VERSION__  = "0.5." + __BUILD__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@cs.brynmawr.edu>"

import sys, atexit, time, os, random
import myro.globals
from myro.media import *
from myro.speech import *
try:
    import Tkinter
    import tkFileDialog
    import tkColorChooser
except:
    Tkinter = None
try:
    import tkSnack
    tkSnack.initializeSnack(myro.globals.gui)
except:
    tkSnack = None
if Tkinter != None:
    from myro.widgets import AskDialog
    try:
        myro.globals.gui = Tkinter.Tk()
        myro.globals.gui.withdraw()
    except:
        Tkinter = None

def wait(seconds):
    """
    Wrapper for time.sleep() so that we may later overload.
    """
    return time.sleep(seconds)

def currentTime():
    """
    Returns current time in seconds since 
    """
    return time.time()

def flipCoin():
    """
    Randomly returns "heads" or "tails".
    """
    return ("heads", "tails")[random.randrange(2)]

def randomNumber():
    """
    Returns a number between 0 (inclusive) and 1 (exclusive).
    """
    return random.random()

def pickAFolder():
    folder = tkFileDialog.askdirectory()
    if folder == '':
        folder = myro.globals.mediaFolder
    return folder
	
def pickAFile():
    path = tkFileDialog.askopenfilename(parent=myro.globals.gui)
    return path

def pickAColor():
    color = tkColorChooser.askcolor()
    newColor = Color(color[0][0], color[0][1], color[0][2])
    return newColor

def ask(item, useCache = 0):
    retval = _ask(item, useCache = useCache)
    if len(retval.keys()) == 2: # ok, and item
        return retval[item]
    else: return retval

def _ask(data, title = "Information Request", forceAsk = 0, forceConsole = 0, useCache = 1):
    """ Given a dictionary return dictionary with answers. """
    if type(data) in [str]:
        data = {data:""}
    if type(data) in [list, tuple]:
        newData = {}
        for item in data:
            newData[item] = ""
        data = newData
    if useCache:
        # get data, if in cache:
        needToAsk = 0
        for question in data.keys():
            if question in myro.globals.askData.keys():
                data[question] = myro.globals.askData[question]
            else:
                needToAsk = 1
    else:
        needToAsk = 1
    # if I got it all, and don't need to ask, return
    # else, ask it all:
    if needToAsk or forceAsk: 
        if Tkinter == None or forceConsole:
            askConsole(data, title)
        else:
            data = askGUI(data, title)
            if data["ok"] == 0:
                raise KeyboardInterrupt
        # cache data in globals:
        for text in data.keys():
            myro.globals.askData[text] = data[text]
    return data

def askGUI(qlist, title = "Information Request"):
   d = AskDialog(myro.globals.gui, title, qlist)
   d.top.bind("<Return>", lambda event: d.OkPressed())
   ok = d.Show()
   if ok:
      retval = {"ok": 1}
      for name in qlist.keys():
          retval[name] = d.textbox[name].get()
      d.DialogCleanup()
      return retval
   else:
      d.DialogCleanup()
      return {"ok" : 0}

def askConsole(data, title = "Information Request"):
    print "+-----------------------------------------------------------------+"
    print "|" + title.center(65) + "|"
    print "+-----------------------------------------------------------------+"
    print "| Please enter the following information. Default values are in   |"
    print "| brackets. To accept default values, just press enter. I'll just |"
    print "| ask this question once this session.                            |"
    print "------------------------------------------------------------------"
    for key in data.keys():
        retval = raw_input("   " + key + (" [%s]" % data[key])+ ": ")
        retval.strip() # remove any spaces on either side
        if retval != "":
            data[key] = retval
    return data

class Robot(object):
    _app = None
    _joy = None
    def __init__(self):
        """
        Base robot class.
        """
        self.services = {}
        if tkSnack:
            self.addService("computer.audio", "type", "tksnack")
        if Tkinter:
            self.addService("computer.graphics", "type", "tkinter")
        if myro.globals.tts:
            self.addService("computer.text-to-speech", "type", str(myro.globals.tts))

    def addService(self, name, attribute, value):
        if name not in self.services.keys():
            self.services[name] = {}
        if attribute not in self.services[name]:
            self.services[name][attribute] = []
        self.services[name][attribute].append(value)
    
    def translate(self, amount):
        raise AttributeError, "this method needs to be written"

    def rotate(self, amount):
        raise AttributeError, "this method needs to be written"

    def move(self, translate, rotate):
        raise AttributeError, "this method needs to be written"

    def beep(self, duration, frequency1, frequency2 = None):
        if (tkSnack):
            snd1 = tkSnack.Sound()
            filt1 = tkSnack.Filter('generator', frequency1, 30000,
                                   0.0, 'sine', int(11500*duration))
            if frequency2 != None:
                snd2 = tkSnack.Sound()
                filt2 = tkSnack.Filter('generator', frequency2, 30000,
                                       0.0, 'sine', int(11500*duration))
                map2 = tkSnack.Filter('map', 1.0)
                snd2.stop()
                # blocking is choppy; sleep below
                snd2.play(filter=filt2, blocking=0) 
            snd1.stop()
            # blocking is choppy; sleep below
            map1 = tkSnack.Filter('map', 1.0)
            snd1.play(filter=filt1, blocking=0)
            start = time.time()
            while time.time() - start < duration:
                myro.globals.gui.update()
                time.sleep(.001)
        elif Tkinter:
	    print "beep!"
            myro.globals.gui.bell()            
            time.sleep(duration)
	else:
	    print "beep!", chr(7)
            time.sleep(duration)
        time.sleep(.1) # simulated delay, like real robot

    def update(self):
        raise AttributeError, "this method needs to be written"

### The rest of these methods are just rearrangements of the above

    def getLight(self, *position):
        return self.get("light", position)

    def getIR(self, *position):
        return self.get("ir", position)

    def getLine(self, *position):
        return self.get("line", position)

    def getStall(self):
        return self.get("stall")

    def getName(self):
        return self.get("name")

    def getAll(self):
        return self.get("all")

    def setLED(self, position, value):
        return self.set("led", position, value)
        
    def setName(self, name):
        return self.set("name", name)

    def setVolume(self, value):
        return self.set("volume", value)

    def setStartSong(self, songName):
        return self.set("startsong", songName)

    def joystick(self):
        from myro.joystick import Joystick
	try:
	    import idlelib
	except:
	    idlelib = None
        if self._joy == None:
            self._joy = Joystick(parent = self._app, robot = self)
        else:
            self._joy.deiconify()
	if idlelib != None and "PyShell" not in dir(idlelib): # "subprocess"
            self._joy._running = 1
	    self._joy.mainloop()

    def turn(self, direction, value = .8):
        if type(direction) in [float, int]:
            return self.rotate(direction)
        else:
            direction = direction.lower()
            if direction == "left":
                return self.turnLeft(value)
            elif direction == "right":
                return self.turnRight(value)
            elif direction in ["straight", "center"]:
                return self.rotate(0)

    def forward(self, amount):
        return self.translate(amount)

    def backward(self, amount):
        return self.translate(-amount)

    def turnLeft(self, amount):
        return self.rotate(amount)
    
    def turnRight(self, amount):
        return self.rotate(-amount)

    def stop(self):
        return self.move(0, 0)

    def motors(self, left, right):
        trans = (right + left) / 2.0
        rotate = (right - left) / 2.0
        return self.move(trans, rotate)

    def restart(self):
        pass
    def close(self):
        pass
    def open(self):
	pass
    def playSong(self, song, wholeNoteDuration = .545):
        """ Plays a song (list of note names, durations) """
        # 1 whole note should be .545 seconds for normal
        for tuple in song:
            self.playNote(tuple)

    def playNote(self, tuple, wholeNoteDuration = .545):
        if len(tuple) == 2:
            (freq, dur) = tuple
            self.beep(dur * wholeNoteDuration, freq)
        elif len(tuple) == 3:
            (freq1, freq2, dur) = tuple
            self.beep(dur * wholeNoteDuration, freq1, freq2)

from myro.robot.scribbler import Scribbler
from myro.robot.surveyor import Surveyor

class SimScribbler(Robot):
    def __init__(self, id = None):
        Robot.__init__(self)
        import myro.simulator
        globalspath, filename = os.path.split(myro.globals.__file__)
        myro.globals.myropath, directory = os.path.split(globalspath)
        self._simulator = myro.simulator.INIT(
            os.path.join(myro.globals.myropath, "worlds", "MyroWorld"))
        for port in self._simulator.ports:
            print "Simulator starting listener on port", port, "..."
            thread = myro.simulator.Thread(self._simulator, port)
            thread.start()
        # start the client(s):
        from myro.robot.symbolic import TCPRobot
        self._clients = []
        for port in self._simulator.ports:
            self._clients.append(TCPRobot("localhost", port))
        myro.globals.robot = self
        myro.globals.simulator = self._simulator
        # FIX: hack to get _cleanup called before Tk exitfunc, which hangs
        atexit.register(_cleanup)
        self.volume = 1
        self.name = "Scribby"
        self.startsong = "tada"
    def translate(self, amount):
        return self._clients[0].translate(amount)
    def rotate(self, amount):
        return self._clients[0].rotate(amount)
    def move(self, translate, rotate):
        return self._clients[0].move(translate, rotate)
    def update(self):
        return self._clients[0].update()
    def get(self, sensor = "all", *positions):
        self._clients[0].update()
        sensor = sensor.lower()
        if sensor == "stall":
            return self._clients[0].stall
        elif sensor == "startsong":
            return self.startsong
        elif sensor == "name":
            return self.name
        elif sensor == "volume":
            return self.volume
        else:
            retvals = []
            if len(positions) == 0:
                if sensor == "light":
                    return self.get("light", 0, 1, 2)
                elif sensor == "ir":
                    return self.get("ir", 0, 1)
                elif sensor == "line":
                    return self.get("line", 0, 1)
                elif sensor == "all":
                    return {"light": self.get("light"),
                            "ir": self.get("ir"),
                            "line": self.get("line"),
                            "stall": self.get("stall")}
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            for position in positions:
                position = int(position)
                if sensor == "light":
                    retvals.append(self._clients[0].light[0].value[position])
                elif sensor == "ir":
                    retvals.append(self._clients[0].ir[0].value[position])
                elif sensor == "line":
                    retvals.append(self._clients[0].line[0].value[position])
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def set(self, item, position, value = None):
        item = item.lower()
        if item == "led":
            return "ok"
        elif item == "name":
            self.name = position
            return "ok"
        elif item == "volume":
            self.volume = position
            return "ok"
        elif item == "startsong":
            self.startsong = position
            return "ok"
        else:
            raise ("invalid set item name: '%s'" % item)

class Computer(Robot):
    def __init__(self):
        Robot.__init__(self)
        if tkSnack:
            self.addService("audio", "type", "tksnack")
    def move(self, translate, rotate):
        print "move(%f, %f)" % (translate, rotate)

computer = Computer()

# functions:
def _cleanup():
    if myro.globals.robot != None:
        myro.globals.robot.stop() # hangs?
	time.sleep(.5)
        myro.globals.robot.close()
    if myro.globals.simulator != None:
       myro.globals.simulator.destroy()

# Get ready for user prompt; set up environment:
if not myro.globals.setup:
    myro.globals.setup = 1
    atexit.register(_cleanup)
    # Ok, now we're ready!
    print >> sys.stderr, "Myro, (c) 2006 Institute for Personal Robots in Education"
    print >> sys.stderr, "[See http://www.roboteducation.org/ for more information]"
    print >> sys.stderr, "Version %s, Revision %s, ready!" % (__VERSION__, __REVISION__.split()[1])

## Functional interface:

def initialize(id = None):
    global robot
    myro.globals.robot = Scribbler(id)
    robot = myro.globals.robot
def simulator(id = None):
    global robot
    myro.globals.robot = SimScribbler(id)
    robot = myro.globals.robot
def translate(amount):
    return myro.globals.robot.translate(amount)
def rotate(amount):
    return myro.globals.robot.rotate(amount)
def move(translate, rotate):
    return myro.globals.robot.move(rotate, translate)
def forward(amount):
    return myro.globals.robot.forward(amount)
def backward(amount):
    return myro.globals.robot.backward(amount)
def turn(direction, amount = .8):
    return myro.globals.robot.turn(direction, amount)
def turnLeft(amount):
    return myro.globals.robot.turnLeft(amount)
def turnRight(amount):
    return myro.globals.robot.turnRight(amount)
def stop():
    return myro.globals.robot.stop()
def openConnection():
    return myro.globals.robot.open()
def closeConnection():
    return myro.globals.robot.close()
def get(sensor = "all", *pos):
    return myro.globals.robot.get(sensor, *pos)
def getLight(*pos):
    return myro.globals.robot.get("light", pos)
def getIR(*pos):
    return myro.globals.robot.get("ir", pos)
def getLine(*pos):
    return myro.globals.robot.get("line", pos)
def getStall():
    return myro.globals.robot.get("stall")
def getAll():
    return myro.globals.robot.get("all")
def getName():
    return myro.globals.robot.get("name")
def getStartSong():
    return myro.globals.robot.get("startsong")
def getVolume():
    return myro.globals.robot.get("volume")
def update():
    return myro.globals.robot.update()
def beep(self, duration, frequency1, frequency2 = None):
    return myro.globals.robot.beep(duration, frequency1, frequency2)
def set(item, position, value = None):
    return myro.globals.robot.set(item, position, value)
def setLED(position, value):
    return myro.globals.robot.set("led", position, value)
def setName(name):
    return myro.globals.robot.set("name", name)
def setVolume(value):
    return myro.globals.robot.set("volume", value)
def setStartSong(songName):
    return myro.globals.robot.set("startsong", songName)
def motors(left, right):
    return myro.globals.robot.motors(left, right)
def restart():
    return myro.globals.robot.restart()
def joystick():
    return myro.globals.robot.joystick()
def playSong(song, wholeNoteDuration = .545):
    return myro.globals.robot.playSong(song, wholeNoteDuration)
def playNote(tuple, wholeNoteDuration = .545):
    return myro.globals.robot.playNote(tuple, wholeNoteDuration)
# --------------------------------------------------------
# Error handler:
# --------------------------------------------------------
import traceback
def _myroExceptionHandler(type, value, tb):
    if Tkinter == None:
        lines = traceback.format_exception(type, value, tb)
        print "Myro Traceback: -------------------------------------------"
        for line in lines:
            print line.rstrip()
    else: # Tkinter
        # make a window
        #win = HelpWindow()
        lines = traceback.format_exception(type, value, tb)
        print "Myro Traceback: -------------------------------------------"
        for line in lines:
            print line.rstrip()
sys.excepthook = _myroExceptionHandler
# --------------------------------------------------------
# Control-C signal handler:
# --------------------------------------------------------
import signal
def _interruptHandler(signum, frame):
    if myro.globals.robot != None:
        print "Stopping robot..."
        myro.globals.robot.stop()
    raise KeyboardInterrupt
signal.signal(signal.SIGINT, _interruptHandler)
# --------------------------------------------------------
