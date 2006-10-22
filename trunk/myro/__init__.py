"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__BUILD__    = "$Build: 2 $"
__VERSION__  = "0.4." + __BUILD__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@cs.brynmawr.edu>"

import sys, atexit, time, os, random
import myro.globals
from myro.media import *
try:
    import Tkinter
except:
    Tkinter = None
try:
    import tkSnack
except:
    tkSnack = None
if Tkinter != None:
    from myro.widgets import AskDialog
    myro.globals.gui = Tkinter.Tk()
    myro.globals.gui.withdraw()

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
	
def pickAColor():
    color = tkColorChooser.askcolor()
    newColor = Color(color[0][0], color[0][1], color[0][2])
    return newColor

def askForPort(forceAsk = 0, forceConsole = 0, useCache = 1):
    return ask({"Port": ""}, "Communication Port", forceAsk, forceConsole,
               useCache)["Port"]

def ask(data, title = None, forceAsk = 0, forceConsole = 0, useCache = 1):
    """ Given a dictionary return dictionary with answers. """
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

def pickAFile():
    path = tkFileDialog.askopenfilename(parent=myro.globals.gui)
    return path

class Robot(object):
    app = None
    joy = None
    
    def __init__(self):
        """
        Base robot class.
        """
    
    def translate(self, amount):
        raise AttributeError, "this method needs to be written"

    def rotate(self, amount):
        raise AttributeError, "this method needs to be written"

    def move(self, translate, rotate):
        raise AttributeError, "this method needs to be written"

    def beep(self, duration, frequency, frequency2 = None):
        raise AttributeError, "this method needs to be written"

    def getLight(self, position):
        raise AttributeError, "this method needs to be written"

    def getIR(self, position):
        raise AttributeError, "this method needs to be written"

    def getLine(self, position):
        raise AttributeError, "this method needs to be written"

    def getStall(self):
        raise AttributeError, "this method needs to be written"

    def getAll(self):
        raise AttributeError, "this method needs to be written"

    def setLED(self, position, value):
        raise AttributeError, "this method needs to be written"

    def setName(self, name):
        raise AttributeError, "this method needs to be written"

    def setVolume(self, value):
        raise AttributeError, "this method needs to be written"

### The rest of these methods are just rearrangements of the above

    def joystick(self):
        from myro.joystick import Joystick
	try:
	    import idlelib
	except:
	    idlelib = None
        if self.joy == None:
            self.joy = Joystick(parent = self.app, robot = self)
        else:
            self.joy.deiconify()
	#if idlelib != None:
	if "PyShell" not in dir(idlelib): # "subprocess"
            self.joy._running = 1
	    self.joy.mainloop()
        #try:
        #    self.app.mainloop()
        #except:
        #    pass

    def get(self, sensor, *positions):
        sensor = sensor.lower()
        if sensor == "stall":
            return self.getStall()
        else:
            retvals = []
            if len(positions) == 0:
                if sensor == "light":
                    return self.getLight("all")
                elif sensor == "ir":
                    return self.getIR("all")
                elif sensor == "line":
                    return self.getLine("all")
                elif sensor == "all":
                    return self.getAll()
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            for position in positions:
                if sensor == "light":
                    retvals.append(self.getLight(position))
                elif sensor == "ir":
                    retvals.append(self.getIR(position))
                elif sensor == "line":
                    retvals.append(self.getLine(position))
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def set(self, item, position, value = None):
        item = item.lower()
        if item == "led":
            return self.setLED(position, value)
        elif item == "name":
            return self.setName(position)
        elif item == "volume":
            return self.setVolume(position)
        else:
            raise ("invalid set item name: '%s'" % item)

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
    def update(self):
        pass

    def playSong(self, song, wholeNoteDuration = .545):
        """ Plays a song (list of note names, durations) """
        # 1 whole note should be .545 seconds for normal
        for tuple in song:
            self.playNote(tuple)

    def playNote(self, tuple):
        if len(tuple) == 2:
            (freq, dur) = tuple
            self.beep(dur * wholeNoteDuration, freq)
        elif len(tuple) == 3:
            (freq1, freq2, dur) = tuple
            self.beep(dur * wholeNoteDuration, freq1, freq2)

from myro.robot.scribbler import Scribbler

class SimScribbler(Robot):
    def __init__(self, id = None):
        import myro.simulator
        globalspath, filename = os.path.split(myro.globals.__file__)
        myro.globals.myropath, directory = os.path.split(globalspath)
        self._simulator = myro.simulator.INIT(
            os.path.join(myro.globals.myropath, "worlds", "MyroWorld"))
        if (tkSnack):
            tkSnack.initializeSnack(myro.globals.gui)
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
    def translate(self, amount):
        return self._clients[0].translate(amount)
    def rotate(self, amount):
        return self._clients[0].rotate(amount)
    def move(self, translate, rotate):
        return self._clients[0].move(translate, rotate)
    def getLight(self, pos):
        self._clients[0].update()
        return self._clients[0].light[0].value[pos]
    def getIR(self, pos):
        self._clients[0].update()
        return self._clients[0].ir[0].value[pos]
    def getLine(self, pos):
        self._clients[0].update()
        return self._clients[0].line[0].value[pos]
    def getStall(self):
        self._clients[0].update()
        return self._clients[0].stall
    def update(self):
        return self._clients[0].update()
    def beep(self, duration, frequency, frequency2 = None):
        if (tkSnack):
            snd = tkSnack.Sound()
            filt = tkSnack.Filter('generator', frequency, 30000,
                                  0.0, 'sine', int(11500*duration))
            snd.stop()
            snd.play(filter=filt, blocking=1)
	else:
	    print chr(7)
    def setLED(self, position, value):
        pass
    
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
    print >> sys.stderr, "Version %s, Revision %s, ready!" % (__VERSION__, __REVISION__.split()[1])

## Non-object interface:

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
    return myro.globals.robot.left(amount)
def turnRight(amount):
    return myro.globals.robot.right(amount)
def stop():
    return myro.globals.robot.stop()
def openConnection():
    return myro.globals.robot.open()
def closeConnection():
    return myro.globals.robot.close()
def get(sensor, *pos):
    return myro.globals.robot.get(sensor, *pos)
def getLight(pos):
    return myro.globals.robot.getLight(pos)
def getIR(pos):
    return myro.globals.robot.getIR(pos)
def getLine(pos):
    return myro.globals.robot.getLine(pos)
def getStall():
    return myro.globals.robot.getStall()
def getAll():
    return myro.globals.robot.getAll()
def getName():
    return myro.globals.robot.getName()
def update():
    return myro.globals.robot.update()
def beep(self, duration, frequency, frequency2 = None):
    return myro.globals.robot.beep(duration, frequency, frequency2)
def set(item, position, value = None):
    return myro.globals.robot.set(item, position, value)
def setLED(position, value):
    return myro.globals.robot.setLED(position, value)
def setName(name):
    return myro.globals.robot.setName(name)
def setVolume(value):
    return myro.globals.robot.setVolume(value)
def motors(left, right):
    return myro.globals.robot.motors(left, right)
def restart():
    return myro.globals.robot.restart()
def joystick():
    return myro.globals.robot.joystick()

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
