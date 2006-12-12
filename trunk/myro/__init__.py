"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://www.roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__BUILD__    = "$Build: 0 $"
__VERSION__  = "0.8." + __BUILD__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@cs.brynmawr.edu>"

import sys, atexit, time, random, pickle
import myro.globals
from myro.media import *
from myro.speech import *
from myro.chat import *

try:
    import Tkinter
    import tkFileDialog
    import tkColorChooser
    import Dialog
except:
    Tkinter = None
if Tkinter != None:
    #from myro.graphics import *
    from myro.widgets import AskDialog as _AskDialog
    try:
        myro.globals.gui = Tkinter.Tk()
        myro.globals.gui.withdraw()
    except:
        Tkinter = None
try:
    import tkSnack
    tkSnack.initializeSnack(myro.globals.gui)
except:
    tkSnack = None

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

def askQuestion(question, answers = ["Yes", "No"], title = "Myro Question",
                default = 0, bitmap=Dialog.DIALOG_ICON):
    """ Displays a question and returns answer. """
    d = Dialog.Dialog(title=title, default=default, bitmap=bitmap,
                      text=question, strings=answers)
    return answers[int(d.num)]

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
            _askConsole(data, title)
        else:
            data = _askGUI(data, title)
            if data["ok"] == 0:
                raise KeyboardInterrupt
        # cache data in globals:
        for text in data.keys():
            myro.globals.askData[text] = data[text]
    return data

def _askGUI(qlist, title = "Information Request"):
   d = _AskDialog(myro.globals.gui, title, qlist)
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

def _askConsole(data, title = "Information Request"):
    print "+-----------------------------------------------------------------+"
    print "|" + title.center(65) + "|"
    print "+-----------------------------------------------------------------+"
    print "| Please enter the following information. Default values are in   |"
    print "| brackets. To accept default values, just press enter.           |"
    print "------------------------------------------------------------------"
    for key in data.keys():
        retval = raw_input("   " + key + (" [%s]" % data[key])+ ": ")
        retval.strip() # remove any spaces on either side
        if retval != "":
            data[key] = retval
    return data


class BackgroundThread(threading.Thread):
    """
    A thread class for running things in the background.
    """
    def __init__(self, function, pause = 0.01):
        """
        Constructor, setting initial variables
        """
        self.function = function
        self._stopevent = threading.Event()
        self._sleepperiod = pause
        threading.Thread.__init__(self, name="MyroThread")
        
    def run(self):
        """
        overload of threading.thread.run()
        main control loop
        """
        while not self._stopevent.isSet():
            self.function()
            #self._stopevent.wait(self._sleepperiod)

    def join(self,timeout=None):
        """
        Stop the thread
        """
        self._stopevent.set()
        threading.Thread.join(self, timeout)

class Robot(object):
    _app = None
    _joy = None
    def __init__(self):
        """
        Base robot class.
        """
        self.services = {}
        if tkSnack != None:
            self.addService("computer.audio", "type", "tksnack")
        if Tkinter != None:
            self.addService("computer.graphics", "type", "tkinter")
        if myro.globals.tts != None:
            self.addService("computer.text-to-speech", "type", str(myro.globals.tts))

    def initializeRemoteControl(self, password):
        self.chat = Chat(self.name, password)

    def processRemoteControlLoop(self, threaded = 1):
        if threaded:
            self.thread = BackgroundThread(self.processRemoteControl, 1) # seconds
            self.thread.start()
        else:
            while 1:
                self.processRemoteControl()

    def processRemoteControl(self):
        messages = self.chat.receive()
        #print "process", messages
        for _from, message in messages:
            if message.startswith("robot."):
                # For user IM messages
                #print ">>> self." + message[6:]
                retval = eval("self." + message[6:])
                name, domain = _from.split("@")
                #print "sending:", pickle.dumps(retval)
                self.chat.send(name.lower(), pickle.dumps(retval))

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
        if tkSnack != None:
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
        elif Tkinter != None:
            myro.globals.gui.bell()            
            time.sleep(duration)
	else:
	    print "beep!", chr(7)
            time.sleep(duration)
        time.sleep(.1) # simulated delay, like real robot

    def update(self):
        raise AttributeError, "this method needs to be written"

### The rest of these methods are just rearrangements of the above

    def getVersion(self):
        return self.get("version")

    def getLight(self, *position):
        return self.get("light", *position)

    def getIR(self, *position):
        return self.get("ir", *position)

    def getLine(self, *position):
        return self.get("line", *position)

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

    def joyStick(self):
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
from myro.robot.simulator import SimScribbler

class Computer(Robot):
    """ An interface to computer devices. """
    def __init__(self):
        """ Constructs a computer object. """
        Robot.__init__(self)
        if tkSnack:
            self.addService("audio", "type", "tksnack")
    def move(self, translate, rotate):
        """ Moves the robot translate, rotate velocities. """
        print "move(%f, %f)" % (translate, rotate)
    def speak(self, message, async = 1):
        """ Speaks a text message. """
        if myro.globals.tts != None:
            myro.globals.tts.speak(message, async)
        else:
            print "Text-to-speech is not loaded"
    def stopSpeaking(self):
        if myro.globals.tts != None:
            myro.globals.tts.stop()
        else:
            print "Text-to-speech is not loaded"
    def setVoice(self, name):
        if myro.globals.tts != None:
            myro.globals.tts.setVoice(name)
        else:
            print "Text-to-speech is not loaded"
    def getVoice(self):
        if myro.globals.tts != None:
            return myro.globals.tts.getVoice()
        else:
            print "Text-to-speech is not loaded"
    def getVoices(self):
        if myro.globals.tts != None:
            return myro.globals.tts.getVoices()
        else:
            print "Text-to-speech is not loaded"
    def playSpeech(self, filename):
        if myro.globals.tts != None:
            myro.globals.tts.playSpeech(filename)
        else:
            print "Text-to-speech is not loaded"
    def saveSpeech(self, message, filename):
        if myro.globals.tts != None:
            myro.globals.tts.saveSpeech(message, filename)
        else:
            print "Text-to-speech is not loaded"
            
computer = Computer()
robot    = None

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
    robot = Scribbler(id)
def simulator(id = None):
    global robot
    robot = SimScribbler(id)
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
def getVersion():
    return myro.globals.robot.get("version")
def getLight(*pos):
    return myro.globals.robot.get("light", *pos)
def getIR(*pos):
    return myro.globals.robot.get("ir", *pos)
def getLine(*pos):
    return myro.globals.robot.get("line", *pos)
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
def beep(duration, frequency1, frequency2 = None):
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
def joyStick():
    return myro.globals.robot.joyStick()
def playSong(song, wholeNoteDuration = .545):
    return myro.globals.robot.playSong(song, wholeNoteDuration)
def playNote(tup, wholeNoteDuration = .545):
    return myro.globals.robot.playNote(tup, wholeNoteDuration)
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
