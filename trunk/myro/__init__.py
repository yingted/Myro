"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://www.roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__BUILD__    = "$Build: 2 $"
__VERSION__  = "1.0." + __BUILD__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@cs.brynmawr.edu>"

from idlelib import PyShell
import sys, atexit, time, random, pickle, threading, os
import myro.globvars
from myro.media import *
from myro.speech import *
from myro.chat import *
from myro.system import *

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
        myro.globvars.gui = Tkinter.Tk()
        myro.globvars.gui.withdraw()
    except:
        Tkinter = None
try:
    import tkSnack
    tkSnack.initializeSnack(myro.globvars.gui)
except:
    tkSnack = None

def _update_gui():
    if "flist" in dir(PyShell):
        PyShell.flist.pyshell.write("")
        #PyShell.flist.pyshell.update()

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

def pickOne(*args):
    """
    Randomly pick one of a list, or one between [0, arg).
    """
    if len(args) == 1:
        return random.randrange(args[0])
    else:
        return args[random.randrange(len(args))]

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
    """ Returns a folder path/name """
    folder = tkFileDialog.askdirectory()
    if folder == '':
        folder = myro.globvars.mediaFolder
    return folder
	
def pickAFile():
    """ Returns a filename """
    path = tkFileDialog.askopenfilename(parent=myro.globvars.gui)
    return path

def pickAColor():
    """ Returns an RGB color tuple """
    color = tkColorChooser.askcolor()
    newColor = Color(color[0][0], color[0][1], color[0][2])
    return newColor

def ask(item, useCache = 0):
    retval = _ask(item, useCache = useCache)
    if len(retval.keys()) == 1:
        return retval[item]
    if len(retval.keys()) == 2 and "ok" in retval.keys():
        return retval[item]
    else:
        return retval

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
            if question in myro.globvars.askData.keys():
                data[question] = myro.globvars.askData[question]
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
            myro.globvars.askData[text] = data[text]
    return data

def _askGUI(qlist, title = "Information Request"):
   d = _AskDialog(myro.globvars.gui, title, qlist)
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
        if myro.globvars.tts != None:
            self.addService("computer.text-to-speech", "type", str(myro.globvars.tts))

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
                myro.globvars.gui.update()
                time.sleep(.001)
        elif Tkinter != None:
            myro.globvars.gui.bell()            
            time.sleep(duration)
	else:
	    print "beep!", chr(7)
            time.sleep(duration)
        time.sleep(.1) # simulated delay, like real robot

    def getLastSensors(self):
        """ Returns last sensor readings as dictionary """
        return {}

    def update(self):
        """ Update the robot """
        raise AttributeError, "this method needs to be written"

### The rest of these methods are just rearrangements of the above

    def getVersion(self):
        """ Returns robot version information. """
        return self.get("version")

    def getLight(self, *position):
        """ Return the light readings. """
        return self.get("light", *position)

    def getIR(self, *position):
        """ Returns the infrared readings. """
        return self.get("ir", *position)

    def getLine(self, *position):
        """ Returns the line sensor readings. """
        return self.get("line", *position)

    def getStall(self):
        """ Returns the stall reading. """
        return self.get("stall")

    def getInfo(self, *item):
        """ Returns the info. """
        return self.get("info", *item)

    def getName(self):
        """ Returns the robot's name. """
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

    def joyStick(self, showSensors = 0):
        from myro.joystick import Joystick
        self._joy = Joystick(parent = self._app, robot = self, showSensors = showSensors)
        self._joy.minorloop()
        # this will not work, must be in same thread:
        #thread.start_new_thread(self._joy.minorloop, ())

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
            self.playNote(tuple, wholeNoteDuration)

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
        self.lock = threading.Lock()
        if tkSnack:
            self.addService("audio", "type", "tksnack")
    def move(self, translate, rotate):
        """ Moves the robot translate, rotate velocities. """
        print "move(%f, %f)" % (translate, rotate)
    def speak(self, message, async = 1):
        """ Speaks a text message. """
        if myro.globvars.tts != None:
            myro.globvars.tts.speak(message, async)
        else:
            print "Text-to-speech is not loaded"
    def get(self, what):
        return {}
    def stopSpeaking(self):
        if myro.globvars.tts != None:
            myro.globvars.tts.stop()
        else:
            print "Text-to-speech is not loaded"
    def setVoice(self, name):
        if myro.globvars.tts != None:
            myro.globvars.tts.setVoice(name)
        else:
            print "Text-to-speech is not loaded"
    def getVoice(self):
        if myro.globvars.tts != None:
            return str(myro.globvars.tts.getVoice())
        else:
            print "Text-to-speech is not loaded"
    def getVoices(self):
        if myro.globvars.tts != None:
            return map(str, myro.globvars.tts.getVoices())
        else:
            print "Text-to-speech is not loaded"
    def playSpeech(self, filename):
        if myro.globvars.tts != None:
            myro.globvars.tts.playSpeech(filename)
        else:
            print "Text-to-speech is not loaded"
    def saveSpeech(self, message, filename):
        if myro.globvars.tts != None:
            myro.globvars.tts.saveSpeech(message, filename)
        else:
            print "Text-to-speech is not loaded"
            
computer = Computer()

# functions:
def _cleanup():
    if myro.globvars.robot != None:
        myro.globvars.robot.stop() # hangs?
	time.sleep(.5)
        myro.globvars.robot.close()

# Get ready for user prompt; set up environment:
if not myro.globvars.setup:
    myro.globvars.setup = 1
    atexit.register(_cleanup)
    # Ok, now we're ready!
    print >> sys.stderr, "Myro, (c) 2006 Institute for Personal Robots in Education"
    print >> sys.stderr, "[See http://www.roboteducation.org/ for more information]"
    print >> sys.stderr, "Version %s, Revision %s, ready!" % (__VERSION__, __REVISION__.split()[1])

## Functional interface:

def requestStop():
    if myro.globvars.robot:
        myro.globvars.robot.requestStop = 1
    else:
        raise AttributeError, "need to initialize robot"
def initialize(id = None):
    myro.globvars.robot = Scribbler(id)
def simulator(id = None):
    _startSimulator()
    time.sleep(2)
    myro.globvars.robot = SimScribbler(id)
def translate(amount):
    if myro.globvars.robot:
        return myro.globvars.robot.translate(amount)
    else:
        raise AttributeError, "need to initialize robot"
def rotate(amount):
    if myro.globvars.robot:
        return myro.globvars.robot.rotate(amount)
    else:
        raise AttributeError, "need to initialize robot"
def move(translate, rotate):
    if myro.globvars.robot:
        return myro.globvars.robot.move(translate, rotate)
    else:
        raise AttributeError, "need to initialize robot"
def forward(amount):
    if myro.globvars.robot:
        return myro.globvars.robot.forward(amount)
    else:
        raise AttributeError, "need to initialize robot"
def backward(amount):
    if myro.globvars.robot:
        return myro.globvars.robot.backward(amount)
    else:
        raise AttributeError, "need to initialize robot"
def turn(direction, amount = .8):
    if myro.globvars.robot:
        return myro.globvars.robot.turn(direction, amount)
    else:
        raise AttributeError, "need to initialize robot"
def turnLeft(amount):
    if myro.globvars.robot:
        return myro.globvars.robot.turnLeft(amount)
    else:
        raise AttributeError, "need to initialize robot"
def turnRight(amount):
    if myro.globvars.robot:
        return myro.globvars.robot.turnRight(amount)
    else:
        raise AttributeError, "need to initialize robot"
def stop():
    if myro.globvars.robot:
        return myro.globvars.robot.stop()
    else:
        raise AttributeError, "need to initialize robot"
def openConnection():
    if myro.globvars.robot:
        return myro.globvars.robot.open()
    else:
        raise AttributeError, "need to initialize robot"
def closeConnection():
    if myro.globvars.robot:
        return myro.globvars.robot.close()
    else:
        raise AttributeError, "need to initialize robot"
def get(sensor = "all", *pos):
    if myro.globvars.robot:
        return myro.globvars.robot.get(sensor, *pos)
    else:
        raise AttributeError, "need to initialize robot"
def getVersion():
    if myro.globvars.robot:
        return myro.globvars.robot.get("version")
    else:
        raise AttributeError, "need to initialize robot"
def getLight(*pos):
    if myro.globvars.robot:
        return myro.globvars.robot.get("light", *pos)
    else:
        raise AttributeError, "need to initialize robot"
def getIR(*pos):
    if myro.globvars.robot:
        return myro.globvars.robot.get("ir", *pos)
    else:
        raise AttributeError, "need to initialize robot"
def getLine(*pos):
    if myro.globvars.robot:
        return myro.globvars.robot.get("line", *pos)
    else:
        raise AttributeError, "need to initialize robot"
def getStall():
    if myro.globvars.robot:
        return myro.globvars.robot.get("stall")
    else:
        raise AttributeError, "need to initialize robot"
def getInfo(*item):
    if myro.globvars.robot:
        return myro.globvars.robot.getInfo(*item)
    else:
        raise AttributeError, "need to initialize robot"
def getAll():
    if myro.globvars.robot:
        return myro.globvars.robot.get("all")
    else:
        raise AttributeError, "need to initialize robot"
def getName():
    if myro.globvars.robot:
        return myro.globvars.robot.get("name")
    else:
        raise AttributeError, "need to initialize robot"
def getStartSong():
    if myro.globvars.robot:
        return myro.globvars.robot.get("startsong")
    else:
        raise AttributeError, "need to initialize robot"
def getVolume():
    if myro.globvars.robot:
        return myro.globvars.robot.get("volume")
    else:
        raise AttributeError, "need to initialize robot"
def update():
    if myro.globvars.robot:
        return myro.globvars.robot.update()
    else:
        raise AttributeError, "need to initialize robot"
def beep(duration, frequency1, frequency2 = None):
    if myro.globvars.robot:
        return myro.globvars.robot.beep(duration, frequency1, frequency2)
    else:
        raise AttributeError, "need to initialize robot"
def set(item, position, value = None):
    if myro.globvars.robot:
        return myro.globvars.robot.set(item, position, value)
    else:
        raise AttributeError, "need to initialize robot"
def setLED(position, value):
    if myro.globvars.robot:
        return myro.globvars.robot.set("led", position, value)
    else:
        raise AttributeError, "need to initialize robot"
def setName(name):
    if myro.globvars.robot:
        return myro.globvars.robot.set("name", name)
    else:
        raise AttributeError, "need to initialize robot"
def setVolume(value):
    if myro.globvars.robot:
        return myro.globvars.robot.set("volume", value)
    else:
        raise AttributeError, "need to initialize robot"
def setStartSong(songName):
    if myro.globvars.robot:
        return myro.globvars.robot.set("startsong", songName)
    else:
        raise AttributeError, "need to initialize robot"
def motors(left, right):
    if myro.globvars.robot:
        return myro.globvars.robot.motors(left, right)
    else:
        raise AttributeError, "need to initialize robot"
def restart():
    if myro.globvars.robot:
        return myro.globvars.robot.restart()
    else:
        raise AttributeError, "need to initialize robot"
def joyStick(showSensors = 0):
    if myro.globvars.robot:
        return myro.globvars.robot.joyStick(showSensors)
    else:
        raise AttributeError, "need to initialize robot"
def playSong(song, wholeNoteDuration = .545):
    if myro.globvars.robot:
        return myro.globvars.robot.playSong(song, wholeNoteDuration)
    else:
        raise AttributeError, "need to initialize robot"
def playNote(tup, wholeNoteDuration = .545):
    if myro.globvars.robot:
        return myro.globvars.robot.playNote(tup, wholeNoteDuration)
    else:
        raise AttributeError, "need to initialize robot"

def _startSimulator():
    globalspath, filename = os.path.split(myro.globvars.__file__)
    myro.globvars.myropath, directory = os.path.split(globalspath)
    simulator_file = os.path.join(myro.globvars.myropath, "simulator.py")
    if os.name in ['nt', 'dos', 'os2'] :
        print simulator_file
        os.system("""start c:\Python24\python.exe "%s" """ % simulator_file)
    elif os.name in ['posix']:
        os.system("""/usr/bin/env python "%s" &""" % simulator_file)
    else:
        raise AttributeError, "your operating system (%s) is not currently supported" % os.name
    
# --------------------------------------------------------
# Error handler:
# --------------------------------------------------------
import traceback
def _myroExceptionHandler(etype, value, tb):
    # make a window
    #win = HelpWindow()
    lines = traceback.format_exception(etype, value, tb)
    print >> sys.stderr, "Myro is stopping: -------------------------------------------"
    for line in lines:
        print >> sys.stderr, line.rstrip()
sys.excepthook = _myroExceptionHandler
