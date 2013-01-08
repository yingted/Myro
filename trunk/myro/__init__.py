"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://www.roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__VERSION__  = "2.9.9"
__AUTHOR__   = "Doug Blank <dblank@cs.brynmawr.edu>"

import sys, atexit, time, random, pickle, threading, os, types, copy
import StringIO, traceback, urllib, glob
import myro.globvars
from myro.media import *
from myro.speech import *
from myro.chat import *
from myro.system import *

# Check versions of things:
_pil_version = None
try:
    import PIL.Image as Image
    _pil_version = Image.VERSION
    del Image
except:
    print >> sys.stderr, "ERROR: you need to install Python Image Library to make pictures"
if _pil_version != None:
    if _pil_version.split(".") < ["1", "1", "5"]:
        print >> sys.stderr, ("ERROR: you need to upgrade Python Image Library to at least 1.1.5 (you're running %s)" % 
                              _pil_version)
del _pil_version

def timer(seconds=0):
    """ A function to be used with 'for' """
    start = time.time()
    while True:
        timepast = time.time() - start
        if seconds != 0 and timepast > seconds:
            raise StopIteration
        yield round(timepast, 3)

_timers = {}
def timeRemaining(seconds=0):
    """ Function to be used with 'while' """
    global _timers
    if seconds == 0: return True
    now = time.time()
    stack = traceback.extract_stack()
    filename, line_no, q1, q2 = stack[-2]
    if filename.startswith("<pyshell"):
        filename = "pyshell"
    if (filename, line_no) not in _timers:
        _timers[(filename, line_no)] = (now, seconds)
        return True
    start, duration = _timers[(filename, line_no)]
    if seconds != duration:
        _timers[(filename, line_no)] = (now, seconds)
        return True
    if now - start > duration:
        del _timers[(filename, line_no)]
        return False
    else:
        return True

pickled = None

def sendPicture(picture, photoname, password, robotname = None):
    global pickled
    photoname = photoname.replace(" ", "")
    photoname = photoname.replace("/", "")
    if robotname == None:
        if myro.globvars.robot != None:
            robotname = myro.globvars.robot.getName()
        else:
            raise AttributeError, "no robot name given and robot not connected"
    ch = Chat(robotname, password)
    if ch.ok == 1:
        image = picture.image
        if image.mode != "RGB":
            image = image.convert("RGB")
        sio = StringIO.StringIO()
        image.save(sio, "jpeg")
        compressed = sio.getvalue()
        pickled = pickle.dumps(compressed)
        try:
            ch.send("admin", ("photo\nname: %s\n" % photoname) + pickled)
        except IOError:
            print "ERROR: image file is too big"
            return
        print "Sent!"

def register(oldname = None):
    answers = ask(["Your email address",
                   "Your robot's name",
                   "Create a Myro password",
                   "Course keyword"], useCache = 1)
    ch = Chat(answers["Your robot's name"], answers["Create a Myro password"])
    if ch.ok == 1:
        oldstr = ""
        if oldname != None:
            oldstr += "rename: %s\n" % oldname
        email = answers["Your email address"]
        robot = answers["Your robot's name"]
        password = answers["Create a Myro password"]
        keyword = answers["Course keyword"]
        ch.send("admin", """register
email: %s
username: %s
password: %s
keyword: %s
%s""" % (email, robot, password, keyword, oldstr))
        # send a special message to create account
        # wait for response:
        messages = ch.receive()
        while len(messages) == 0:
            messages = ch.receive()
            wait(1)
            print "   waiting for confirmation..."
        print "received messages:"
        for message in messages:
            print message[1]
            print
        # if you have your robot on, then set its name:
        if myro.globvars.robot != None:
            myro.globvars.robot.set("name", answers["Your robot's name"])
            print "Your robot's name was set to", myro.globvars.robot.get("name")
    else:
        print "The name '%s' has already been taken. Please try another." % answers["Your robot's name"]

def setPassword(robotName, emailAddress, newPassword):
    ch = Chat("myro", "request")
    if ch.ok == 1:
        # send a special message to create account
        # wait for response:
        ch.send("admin", "password reset\nemail: %s\nusername: %s\npassword: %s"
                % (emailAddress, robotName, newPassword))
        messages = ch.receive()
        while len(messages) == 0:
            messages = ch.receive()
            wait(1)
            print "   waiting for confirmation..."
        print "received messages:"
        for message in messages:
            print message[1]
            print
    else:
        print "The Myro chat account doesn't seem to be taking requests right now."


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

def pickOneInRange(start, stop):
    """
    Randomly pick one of a list, or one between [0, arg).
    """
    return random.randrange(start, stop)

def heads(): return flipCoin() == "heads"
def tails(): return flipCoin() == "tails"

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

def gamepad(*phrases, **kwargs):
    """
    Run the gamepad controller.
    """
    if "darwin" in sys.platform:
        print "Sorry gamepad not supported on mac os x yet :("
        return
        
    if len(phrases) == 0:
        try:
            name = getName()
        except AttributeError:
            name = "Scribby"
        phrases = ["Hello. My name is %s." % name, 
                   "Ouch! I'm a sensitive robot.", 
                   "I'm hungry. Do you have any batteries?" ]
    elif type(phrases[0]) == type(gamepad):
        print "Gamepad is now running..."
        while True:
            retval = getGamepad()
            button = retval["button"]
            for i in range(len(phrases)):
                if button[i]:
                    retval = phrases[i]()
                    if retval == 'exit':
                        break
        return
    print "        Pad   Action"
    print "     ------   -------"
    print " Left/Right   turnLeft() and turnRight()"
    print "    Up/Down   forward() and backward()"
    print ""

    #Added by JWS to make sure that retval and button are defined.
    retval = getGamepadNow()
    button = retval["button"]

    if len(button) > 0:
        print "     Button   Action"
        print "     ------   -------"
        if len(button) > 0:
            print "          1   stop()"
        if len(button) > 1:
            print "          2   takePicture()"
        if len(button) > 2:
            print "          3   beep(.25, 523)"  
        if len(button) > 3:
            print "          4   beep(.25, 587)"  
        if len(button) > 4:
            print "          5   beep(.25, 659)"  
        if len(button) > 5:
            print "          6   speak('%s')" % phrases[0]
        if len(button) > 6:
            print "          7   speak('%s')" % phrases[1]
        if len(button) > 7:
            print "          8   speak('%s')" % phrases[2]
        print ""


    print "Gamepad is now running... Press button 1 to stop."
    lastMove = [0, 0]
    doneSpeaking = True
    retval = getGamepadNow()
    button = retval["button"]
    length = len(button)
    tryToMove = True
    while True:
        retval = getGamepad()		#changed to blocking, JWS
        button = retval["button"]
        axis = retval["axis"]
        freqs = [None, None]
        if length > 0 and button[0]:
            stop()
            break
        if length > 1 and button[1]:
            speak("Say cheese!", async=1)
            pic = takePicture()
            show(pic)
        if length > 2 and button[2]:
            freqs[0] = 523
        if length > 3 and button[3]:
            if freqs[0] == None:
                freqs[0] = 587
            else:
                freqs[1] = 587
        if length > 4 and button[4]:
            if freqs[0] == None:
                freqs[0] = 659
            else:
                freqs[1] = 659

        ## speak
        if length > 5 and button[5]:
            if doneSpeaking:
                speak(phrases[0], async=1)
                doneSpeaking = False
        elif length > 6 and button[6]:
            if doneSpeaking:
                speak(phrases[1], async=1)
                doneSpeaking = False
        elif length > 7 and button[7]:
            if doneSpeaking:
                speak(phrases[2], async=1)
                doneSpeaking = False
        else:
            doneSpeaking = True


        if tryToMove and (axis[0], axis[1]) != lastMove:
            try:
                move(-axis[1], -axis[0])
                lastMove = axis[0], axis[1]
            except:
                tryToMove = False
        if freqs != [None, None]:
            try:
                beep(.25, *freqs)
            except:
                computer.beep(.25, *freqs)
                
def getGamepad(*what, **kwargs):
    """
    Return readings from a gamepad/joystick when there is a change. 

    what can be empty, "init", "name", "axis", "ball", "button",
    "hat", or "count".  If what is more than 1 item, then getGamepad
    will return a dictionary, else it will return the 1 item's
    value(s). If the first arg given to this function is an int, then
    it refers to that joystick ID. If the first arg is a list of ints, 
    then it will return those joystick data.
    """
    if "count" in what:
        return _getGamepadNow(*what)
    if "wait" in kwargs:
        waitTime = kwargs["wait"]
        del kwargs["wait"]
    else:
        waitTime = 0.05
    if "any" in what:
        any = True
        what = list(what)
        what.remove("any")
    else:
        any = False
    retval = _getGamepadNow(*what)
    newRetval = _getGamepadNow(*what)
    while retval == newRetval:
        newRetval = _getGamepadNow(*what)
        wait(waitTime)
    if any:
        return _or(newRetval, retval)
    return newRetval

def _or(a, b):
    """
    For buttons, it is handy to just have a 1 if it was pressed
    or let up.
    """
    if type(a) == type(0):
        return a or b
    elif type(a) == type(True):
        return a or b
    elif type(a) == type(1.0):
        return a
    elif type(a) == type(""):
        return a
    elif type(a) in [type([]), type((0,))]:
        retval = []
        for i in range(len(a)):
            retval.append(_or(a[i], b[i]))
        return retval
    elif type(a) == type({}):
        retval = {}
        for k in a:
            retval[k] = _or(a[k], b[k])
        return retval
    else:
        raise AttributeError, ("invalid type: %s" % str(a))

def getGamepadNow(*what):
    """
    Return readings from a gamepad/joystick immediately. 

    what can be empty, "init", "name", "axis", "ball", "button",
    "hat", or "count".  If what is more than 1 item, then getGamepad
    will return a dictionary, else it will return the 1 item's
    value(s). If the first arg given to this function is an int, then
    it refers to that joystick ID. If the first arg is a list of ints, 
    then it will return those joystick data.
    """
    if len(what) > 0:
        if type(what[0]) == type(0): # particular gamepad id
            id = what[0]
            what = what[1:]
        elif type(what[0]) == type([]): # list of gamepad ids
            retval = []
            for i in what[0]:
                retval.append((i, _getGamepadNow(i, *what[1:])))
            return retval
        else:
            id = 0
    else:
        id = 0
    #myro.globvars.pygame.event.pump()
    pygame.event.pump()
    if id < len(myro.globvars.joysticks):
        js = myro.globvars.joysticks[id]
    else:
        js = None
    retval = {}
    if len(what) == 0:
        what = ["init", "name", "axis", "ball", "button", "hat"]
    for item in what:
        if item == "count":
            retval["count"] = pygame.joystick.get_count()
        elif js != None:
            if item == "init":
                retval["init"] = js.get_init()
            elif item == "name":
                retval["name"] = js.get_name()
            elif item == "robot":
                retval["robot"] = [-js.get_axis(1), -js.get_axis(0)]
            elif item == "axis":
                retval["axis"] = [js.get_axis(i) for i in range(js.get_numaxes())]
            elif item == "ball":
                retval["ball"] = [js.get_ball(i) for i in range(js.get_numballs())]
            elif item == "button":
                retval["button"] = [js.get_button(i) for i in range(js.get_numbuttons())]
            elif item == "hat":
                retval["hat"] = [js.get_hat(i) for i in range(js.get_numhats())]
        else:
            raise AttributeError, ("not a valid gamepad id: %d" % id)
    if len(retval.keys()) == 0:
        return None
    elif len(retval.keys()) == 1:
        return retval[retval.keys()[0]]
    else:
        return retval

_getGamepadNow = getGamepadNow

def ask(item, title = "Information Request", useCache=0, useDict=0):
    """ Ask the user for a value """
    if type(item) in [list, tuple] and len(item) == 1:
        item = item[0]
    retval = _ask(item, title = title, useCache=useCache)
    if useDict:
        if "ok" in retval:
            del retval["ok"]
        return retval
    elif len(retval.keys()) == 1:
        return retval[item]
    elif len(retval.keys()) == 2 and "ok" in retval.keys():
        return retval[item]
    else:
        return retval

def _ask(data, title = "Information Request", forceAsk=1, forceConsole=0, useCache=1):
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
        if myro.globvars.gui == None or forceConsole:
            _askConsole(data, title)
        else:
            data = _askGUI(data, title)
            if data["ok"] == 0:
                raise KeyboardInterrupt
        # cache data in globals:
        for text in data.keys():
            myro.globvars.askData[text] = data[text]
    return data

def _askGUI(qdict, title = "Information Request"):
   d = AskDialog(title, qdict)
   ok = d.run()
   if ok:
      retval = {"ok": 1}
      for name in qdict.keys():
          retval[name] = d.textbox[name].get()
      d.stop()
      return retval
   else:
      d.stop()
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
    _cal = None
    def __init__(self):
        """
        Base robot class.
        """
        self.lock = threading.Lock()
    
    def initializeRemoteControl(self, password):
        self.chat = Chat(self.getName(), password)

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

    def translate(self, amount):
        raise AttributeError, "this method needs to be written"

    def rotate(self, amount):
        raise AttributeError, "this method needs to be written"

    def move(self, translate, rotate):
        raise AttributeError, "this method needs to be written"

    def beep(self, duration, frequency1, frequency2 = None):
        import myro.graphics
        print "beep!"
        return myro.graphics._tkCall(myro.graphics._beep, duration, frequency1, frequency2)
        
    def getLastSensors(self):
        """ Should not get the current, but the last. This is default behavior. """
        return self.get("all")

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
    
    def getDistance(self, *position):
        """ Returns the S2 Distance readings. """
        return self.getDistance(*position)

    def getLine(self, *position):
        """ Returns the line sensor readings. """
        return self.get("line", *position)

    def getStall(self):
        """ Returns the stall reading. """
        return self.get("stall")

    def getInfo(self, *item):
        """ Returns the info. """
        retval = self.get("info", *item)
        retval["myro"] =  __VERSION__
        return retval

    def getName(self):
        """ Returns the robot's name. """
        return self.get("name")

    def getPassword(self):
        """ Returns the robot's password. """
        return self.get("password")

    def getForwardness(self):
        """ Returns the robot's directionality. """
        return self.get("forwardness")

    def getAll(self):
        return self.get("all")

    def setLED(self, position, value):
        return self.set("led", position, value)
        
    def setName(self, name):
        return self.set("name", name)

    def setPassword(self, password):
        return self.set("password", password)

    def setForwardness(self, value):
        return self.set("forwardness", value)
    
    def setVolume(self, value):
        return self.set("volume", value)

    def setStartSong(self, songName):
        return self.set("startsong", songName)

    def forward(self, speed=1, interval=None):
        self.move(speed, 0)
        if interval != None:
            time.sleep(interval)
            self.stop()

    def backward(self, speed=1, interval=None):
        self.move(-speed, 0)
        if interval != None:
            time.sleep(interval)
            self.stop()

    def turn(self, direction, value = .8, interval=None):
        if type(direction) in [float, int]:
            retval = self.move(0, direction)
        else:
            direction = direction.lower()
            if direction == "left":
                retval = self.move(0, value)
            elif direction == "right":
                retval = self.move(0, -value)
            elif direction in ["straight", "center"]:
                retval = self.move(0, 0) # aka, stop!
            else:
                retval = "error"
        if interval != None:
            time.sleep(interval)
            self.stop()
        return retval

    def turnLeft(self, speed=1, interval=None):
        retval = self.move(0, speed)
        if interval != None:
            time.sleep(interval)
            self.stop()
        return retval
    
    def turnRight(self, speed=1, interval=None):
        retval = self.move(0, -speed)
        if interval != None:
            time.sleep(interval)
            self.stop()
        return retval

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
        """ Plays a song [(freq, [freq2,] duration),...] """
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

class Computer(Robot):
    """ An interface to computer devices. """
    def __init__(self):
        """ Constructs a computer object. """
        Robot.__init__(self)
        self.lock = threading.Lock()
    def move(self, translate, rotate):
        """ Moves the robot translate, rotate velocities. """
        print "move(%f, %f)" % (translate, rotate)
    def speak(self, message, async = 0):
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
    if myro.globvars.robot:
            if "robot" in myro.globvars.robot.robotinfo:
                try:
                    myro.globvars.robot.stop() # hangs?
                    time.sleep(0.5)                        
                except: # catch serial.SerialException
                    # port already closed
                    pass
            try:
                myro.globvars.robot.close()
            except:
                pass

import signal

def ctrlc_handler(signum, frame):
    if myro.globvars.robot:
        #myro.globvars.robot.open()
        #print "done opening"
        myro.globvars.robot.manual_flush()
        if "robot" in myro.globvars.robot.robotinfo:
            myro.globvars.robot.hardStop()
    #raise KeyboardInterrupt
    orig_ctrl_handler()

orig_ctrl_handler = signal.getsignal(signal.SIGINT)
# Set the signal handler and a 5-second alarm
signal.signal(signal.SIGINT, ctrlc_handler)

# Get ready for user prompt; set up environment:
if not myro.globvars.setup:
    myro.globvars.setup = 1
    atexit.register(_cleanup)
    # Ok, now we're ready!
    print >> sys.stderr, "(c) 2006-2007 Institute for Personal Robots in Education"
    print >> sys.stderr, "[See http://www.roboteducation.org/ for more information]"
    print >> sys.stderr, "Myro version %s is ready!" % (__VERSION__, )

## Functional interface:

def requestStop():
    if myro.globvars.robot:
        myro.globvars.robot.requestStop = 1
def initialize(id = None):
    if id == "simulator":
        simulator(None)
    else:
        myro.globvars.robot = Scribbler(id)
    __builtins__["robot"] = myro.globvars.robot

init = initialize        

def simulator(id = None):
    _startSimulator()
    time.sleep(2)
    myro.globvars.robot = SimScribbler(id)
    __builtins__["robot"] = myro.globvars.robot
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
def forward(speed=1, seconds=None):
    if myro.globvars.robot:
        return myro.globvars.robot.forward(speed, seconds)
    else:
        raise AttributeError, "need to initialize robot"
    
def backward(speed=1, seconds=None):
    if myro.globvars.robot:
        return myro.globvars.robot.backward(speed, seconds)
    else:
        raise AttributeError, "need to initialize robot"
def turn(direction, amount = .8, seconds=None):
    if myro.globvars.robot:
        return myro.globvars.robot.turn(direction, amount, seconds)
    else:
        raise AttributeError, "need to initialize robot"
def turnLeft(speed=1, seconds=None):
    if myro.globvars.robot:
        return myro.globvars.robot.turnLeft(speed, seconds)
    else:
        raise AttributeError, "need to initialize robot"
def turnRight(speed=1, seconds=None):
    if myro.globvars.robot:
        return myro.globvars.robot.turnRight(speed, seconds)
    else:
        raise AttributeError, "need to initialize robot"
def stop():
    if myro.globvars.robot:
        return myro.globvars.robot.stop()
def getPosition():
    """This returns the x and y coordinates of the scribbler 2"""   
    if myro.globvars.robot:
        return myro.globvars.robot.getPosition()
    else:
        raise AttributeError, "need to initialize robot"    
def hereIs(x=0, y=0):
    if myro.globvars.robot:
        return myro.globvars.robot.setHereIs(x, y)
    else:
        raise AttributeError, "need to initialize robot"
def getAngle():
    """This returns the current angle of the scribbler 2"""   
    if myro.globvars.robot:
        return myro.globvars.robot.getAngle()
    else:
        raise AttributeError, "need to initialize robot"        
def setAngle(angle):
    if myro.globvars.robot:
        return myro.globvars.robot.setAngle(angle)
    else:
        raise AttributeError, "need to initialize robot"    
def beginPath():
    """Speed can be a value from 1 to 15"""   
    if myro.globvars.robot:
        return myro.globvars.robot.setBeginPath()
    else:
        raise AttributeError, "need to initialize robot"
def moveTo(x, y):
    if myro.globvars.robot:
        return myro.globvars.robot.setMove(x, y, "to")
    else:
        raise AttributeError, "need to initialize robot"
def moveBy(x, y):
    if myro.globvars.robot:
        return myro.globvars.robot.setMove(x, y, "by")
    else:
        raise AttributeError, "need to initialize robot"    
def turnTo(angle, radsOrDegrees):
    if myro.globvars.robot:
        return myro.globvars.robot.setTurn(angle, "to", radsOrDegrees)
    else:
        raise AttributeError, "need to initialize robot" 
def turnBy(angle, radsOrDegrees):
    if myro.globvars.robot:
        return myro.globvars.robot.setTurn(angle, "by", radsOrDegrees)
    else:
        raise AttributeError, "need to initialize robot"
def arcTo(x, y, radius):
    if myro.globvars.robot:
        return myro.globvars.robot.setArc(x, y, radius, "to")
    else:
        raise AttributeError, "need to initialize robot"
def arcBy(x, y, radius):
    if myro.globvars.robot:
        return myro.globvars.robot.setArc(x, y, radius, "by")
    else:
        raise AttributeError, "need to initialize robot"
def endPath():
    if myro.globvars.robot:
        return myro.globvars.robot.setEndPath()
    else:
        raise AttributeError, "need to initialize robot"
def getMicEnvelope():
    """Returns a number representing the microphone envelope noise"""   
    if myro.globvars.robot:
        return myro.globvars.robot.getMicEnvelope()
    else:
        raise AttributeError, "need to initialize robot" 
def getMotorStats():
    '''Return the current motion status as a packed long and single additional byte showing if motors are ready for commands (1=ready, 0=busy):
 Left wheel and right wheel are signed, twos complement eight bit velocity values,
 Idler timer is the time in 1/10 second since the last idler edge,
 Idler spd is an unsigned six-bit velocity value, and
 Mov is non-zero iff one or more motors are turning.
 Left and right wheel velocities are instanteous encoder counts over a 1/10-second interval.
 Idler wheel wheel velocity is updated every 1/10 second and represents the idler encoder count during the last 1.6 seconds.'''   
    if myro.globvars.robot:
        return myro.globvars.robot.getMotorStats()
    else:
        raise AttributeError, "need to initialize robot" 
def getEncoders(zeroEncoders=False):
    '''Gets the values for the left and right encoder wheels.  Negative value means they have moved
    backwards from the robots perspective.  Each turn of the encoder wheel is counted as and increment or
    decrement of 2 depending on which direction the wheels moved.  
    if zeroEncoders is set to True then the encoders will be set to zero after reading the values'''
    if myro.globvars.robot:
        return myro.globvars.robot.getEncoders(zeroEncoders)
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

def getDistance(*pos):
    if myro.globvars.robot:
        return myro.globvars.robot.getDistance(*pos)
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
        retval = myro.globvars.robot.getInfo(*item)
        retval["myro"] =  __VERSION__
        return retval
    else:
        return {"myro": __VERSION__}
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
def getPassword():
    if myro.globvars.robot:
        return myro.globvars.robot.get("password")
    else:
        raise AttributeError, "need to initialize robot"
def getForwardness():
    if myro.globvars.robot:
        return myro.globvars.robot.get("forwardness")
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
def beep(duration=.5, frequency1=None, frequency2=None):
    if type(duration) in [tuple, list]:
        frequency2 = frequency1
        frequency1 = duration
        duration =.5
    if frequency1 == None:
        frequency1 = random.randrange(200, 10000)
    if type(frequency1) in [tuple, list]:
        if frequency2 == None:
            frequency2 = [None for i in range(len(frequency1))]
        for (f1, f2) in zip(frequency1, frequency2):
            if myro.globvars.robot:
                myro.globvars.robot.beep(duration, f1, f2)
            else:
                computer.beep(duration, f1, f2)
    else:
        if myro.globvars.robot:
            myro.globvars.robot.beep(duration, frequency1, frequency2)
        else:
            computer.beep(duration, frequency1, frequency2)

def scaleDown(loopCount):
    beep(0.5, 9000 - 200 * loopCount)

def scaleUp(loopCount):
    beep(0.5, 200 + 200 * loopCount)

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
def setPassword(password):
    if myro.globvars.robot:
        return myro.globvars.robot.set("password", password)
    else:
        raise AttributeError, "need to initialize robot"
def setForwardness(value):
    if myro.globvars.robot:
        return myro.globvars.robot.set("forwardness", value)
    else:
        raise AttributeError, "need to initialize robot"
def setVolume(value):
    if myro.globvars.robot:
        return myro.globvars.robot.set("volume", value)
    else:
        raise AttributeError, "need to initialize robot"
def setS2Volume(value):
    """Level can be between 0-100 and represents the percent volume level of the speaker"""   
    if myro.globvars.robot:
        return myro.globvars.robot.setS2Volume(value)
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
        return Joystick(myro.globvars.robot, showSensors)
    else:
        raise AttributeError, "need to initialize robot"
def calibrate():
    if myro.globvars.robot:
        return Calibrate(myro.globvars.robot)
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

########################### New dongle commands

def getBright(position=None):
    if myro.globvars.robot:
        return myro.globvars.robot.getBright(position)
    else:
        raise AttributeError, "need to initialize robot"

def getBlob():
    if myro.globvars.robot:
        return myro.globvars.robot.getBlob()
    else:
        raise AttributeError, "need to initialize robot"

def getObstacle(position=None):
    if myro.globvars.robot:
        return myro.globvars.robot.getObstacle(position)
    else:
        raise AttributeError, "need to initialize robot"
    
def setIRPower(value):
    if myro.globvars.robot:
        return myro.globvars.robot.setIRPower(value)
    else:
        raise AttributeError, "need to initialize robot"

def getBattery():
    if myro.globvars.robot:
        return myro.globvars.robot.getBattery()
    else:
        raise AttributeError, "need to initialize robot"

def identifyRobot():
    if myro.globvars.robot:
        return myro.globvars.robot.identifyRobot()
    else:
        raise AttributeError, "need to initialize robot"

def getIRMessage():
    if myro.globvars.robot:
        return myro.globvars.robot.getIRMessage()
    else:
        raise AttributeError, "need to initialize robot"

def sendIRMessage(msg):
    if myro.globvars.robot:
        return myro.globvars.robot.sendIRMessage(msg)
    else:
        raise AttributeError, "need to initialize robot"

def setCommunicateLeft(on=True):
    if myro.globvars.robot:
        return myro.globvars.robot.setCommunicateLeft(on)
    else:
        raise AttributeError, "need to initialize robot"

def setCommunicateRight(on=True):
    if myro.globvars.robot:
        return myro.globvars.robot.setCommunicateLeft(on)
    else:
        raise AttributeError, "need to initialize robot"

def setCommunicateCenter(on=True):
    if myro.globvars.robot:
        return myro.globvars.robot.setCommunicateCenter(on)
    else:
        raise AttributeError, "need to initialize robot"

def setCommunicateAll(on=True):
    if myro.globvars.robot:
        return myro.globvars.robot.setCommunicateAll(on)
    else:
        raise AttributeError, "need to initialize robot"

def configureBlob(y_low=0, y_high=255,
                  u_low=0, u_high=255,
                  v_low=0, v_high=255,
                  smooth_thresh=4):
    if myro.globvars.robot:
        return myro.globvars.robot.configureBlob(y_low, y_high, u_low, u_high, v_low, v_high, smooth_thresh)
    else:
        raise AttributeError, "need to initialize robot"
    
def setWhiteBalance(value):
    if myro.globvars.robot:
        return myro.globvars.robot.setWhiteBalance(value)
    else:
        raise AttributeError, "need to initialize robot"

def darkenCamera(value=0):
    if myro.globvars.robot:
        return myro.globvars.robot.darkenCamera(value)
    else:
        raise AttributeError, "need to initialize robot"

def manualCamera(gain=0x00, brightness=0x80, exposure=0x41):
    if myro.globvars.robot:
        return myro.globvars.robot.manualCamera(gain, brightness, exposure)
    else:
        raise AttributeError, "need to initialize robot"

def autoCamera(value=0):
    if myro.globvars.robot:
        return myro.globvars.robot.autoCamera()
    else:
        raise AttributeError, "need to initialize robot"

def setLEDFront(value):
    """ Set the Light Emitting Diode on the robot's front. """
    if myro.globvars.robot:
        return myro.globvars.robot.setLEDFront(value)
    else:
        raise AttributeError, "need to initialize robot"

def setLEDBack(value):
    """ Set the Light Emitting Diode on the robot's back. """
    if myro.globvars.robot:
        return myro.globvars.robot.setLEDBack(value)
    else:
        raise AttributeError, "need to initialize robot"

################ New Fluke2 functions ###############

def setPicSize(value):
    """ Set the picture size """
    if myro.globvars.robot:
        return myro.globvars.robot.setPicSize(value)
    else:
        raise AttributeError, "need to initialize robot"

def servo(id, position):
    """ Commands servo number id to position position """
    if myro.globvars.robot:
        return myro.globvars.robot.servo(id, position)
    else:
        raise AttributeError, "need to initialize robot"

def getFlukeLog():
    """ Downloads and prints the fluke2 error log """
    if myro.globvars.robot:
        return myro.globvars.robot.getFlukeLog()
    else:
        raise AttributeError, "need to initialize robot"

def enablePanNetworking():
    """ Enables bluetooth PAN TCP/IP over bluetooth networking """
    if myro.globvars.robot:
        return myro.globvars.robot.enablePanNetworking()
    else:
        raise AttributeError, "need to initialize robot"

########################### Pictures:

def _ndim(n, *args, **kwargs):
    if not args:
        return [kwargs.get("value", 0)] * n
    A = []
    for i in range(n):
        A.append( _ndim(*args, **kwargs) )
    return A

class Column(object):
    def __init__(self, picture, column):
        self.picture = picture
        self.column = column
    def __getitem__(self, row):
        return self.picture.getPixel(self.column, row)

class Array(object):
    def __init__(self, n = 0, *args, **kwargs):
        if type(n) == Picture:
            self.data = n
        else:
            self.data = _ndim(n, *args, **kwargs)
    def __len__(self):
        return len(self.data)

    def __getitem__(self, *args):
        if type(self.data) == Picture:
            return Column(self.data, args[0])
        else:
            current = self.data
            for i in args:
                n, rest = args[0], args[1:]
                current = current[n]
            return current

def makeArray(*args, **kwargs):
    """ Returns an array of the given dimensions. """
    return Array(*args, **kwargs)

def takePicture(mode=None):
    """ Takes a picture using the camera. Mode can be 'color', 'gray', or 'blob' """
    if myro.globvars.robot:
        return myro.globvars.robot.takePicture(mode)
    else:
        raise AttributeError, "need to initialize robot"

def loadPicture(filename):
    """ Loads a picture from a filename. """
    picture = Picture()
    picture.load(filename)
    return picture

def copyPicture(picture):
    """ Takes a Picture object and returns a copy. """
    newPicture = Picture()
    newPicture.set(getWidth(picture), getHeight(picture),
                   picture.image, mode = "image")
    return newPicture

def makePicture(*args):
    """
    Takes zero or more args to make a picture.

    makePicture() - makes a 0x0 image
    makePicture(width, height)
    makePicture("filename")
    makePicture("http://image")
    makePicture(width, height, data)
    makePicture(width, height, data, "mode")
    """
    if len(args) == 0:
        retval = Picture()
    elif len(args) == 1:
        filename = args[0]
        retval = Picture()
        if filename.startswith("http://"):
            filename, message = urllib.urlretrieve(filename)
        retval.load(filename)
    elif len(args) == 2:
        x = args[0]
        y = args[1]
        retval = Picture()
        retval.set(x, y)
    elif len(args) == 3:
        x = args[0]
        y = args[1]
        if type(args[2]) in [Color, Pixel]:
            retval = Picture()
            retval.set(x, y, value=args[2].getRGB())
        elif type(args[2]) == int:
            retval = Picture()
            retval.set(x, y, value=args[2])
        elif type(args[2]) in [list, tuple]: # Undocumented
            array = args[2]
            retval = Picture()
            retval.set(x, y, value=args[2])
        else:
            raise AttributeError("unknown type: %s is '%s'; " +
                                 "should be Color, Pixel, int grayscale", 
                                 args[2], type(args[2]))
    elif len(args) == 4:
        x = args[0]
        y = args[1]
        array = args[2]
        mode = args[3]
        retval = Picture()
        retval.set(x, y, array, mode)
    return retval

def _mouseCallback(point, name="default", scale=1):
    window = myro.globvars.windows[name]
    picture = myro.globvars.pictures[name]
    if 0 <= point.x < getWidth(picture)*scale and \
            0 <= point.y < getHeight(picture)*scale:
        pixel = picture.getPixel(point.x, point.y)
        window.lastX, window.lastY = point.x, point.y
        rgba = pixel.getRGBA()
        window.setStatusDirect("(%d, %d): (%d,%d,%d,a=%d)" %
                               (point.x/scale, point.y/scale, rgba[0], rgba[1], rgba[2], rgba[3]))

def _mouseCallbackRelease(point, name="default", scale=1):
    window = myro.globvars.windows[name]
    picture = myro.globvars.pictures[name]
    if 0 <= point.x < getWidth(picture)*scale and \
            0 <= point.y < getHeight(picture)*scale:
        if abs(window.lastX - point.x) < 3 or abs(window.lastY - point.y) < 3:
            return
        if myro.globvars.robot != None:
            yMin,yMax,uMin,uMax,vMin,vMax = myro.globvars.robot.set_blob_yuv(
                                             picture, window.lastX/scale, window.lastY/scale,
                                             point.x/scale, point.y/scale)
            window.setStatusDirect("Set configureBlob(%d,%d,%d,%d,%d,%d)" % (yMin, yMax, uMin, uMax, vMin,vMax) )

def writePictureTo(picture, filename):
    return picture.image.save(filename)

def savePicture(picture, filename):
    if type(picture) == type([]):
        import ImageChops
        from GifImagePlugin import getheader, getdata
        # open output file
        fp = open(filename, "wb")
        previous = None
        for im in picture:
            if type(im) == type(""): # filename
                im = Image.open(im)
                im.load()
                im = im.convert("P") # in case jpeg, etc
            else:
                im = im.image.convert("P")
            if not previous:
                for s in getheader(im) + getdata(im):
                    fp.write(s)
            else:
                delta = ImageChops.subtract_modulo(im, previous)
                bbox = delta.getbbox()
                if bbox:
                    for s in getdata(im.crop(bbox), offset = bbox[:2]):
                        fp.write(s)
            previous = im.copy()
        fp.write(";")
        fp.close()
    else:
        return picture.image.save(filename)

def show(picture, name="default"):
    if myro.globvars.windows.get(name, None) == None:
        myro.globvars.windows[name] = GraphWin("Myro: %s" % name)
    try:
        myro.globvars.windows[name].delete("image")
    except:
        myro.globvars.windows[name] = GraphWin("Myro: %s" % name)
    if picture.displayScale != 1:
        picture = Picture(picture)
        picture.scale(picture.displayScale)
    myro.globvars.pictures[name] = picture
    myro.globvars.windows[name]['width'] = picture.width
    myro.globvars.windows[name]['height'] = picture.height
    myro.globvars.pixmaps[name] = makePixmap(picture)
    myro.globvars.windows[name].setMouseHandler(lambda point: _mouseCallback(point, name, picture.displayScale))
    myro.globvars.windows[name].setMouseReleaseHandler(lambda point: _mouseCallbackRelease(point, name, picture.displayScale))
    myro.globvars.images[name] = Image(Point(picture.width/2, picture.height/2),
                                    myro.globvars.pixmaps[name])
    myro.globvars.images[name].draw(myro.globvars.windows[name])

def repaint(picture = None, name="default"):
    if picture == None:
        picture = myro.globvars.pictures[name]
    elif picture.displayScale != 1:
        picture = Picture(picture)
        picture.scale(picture.displayScale)
        myro.globvars.pictures[name] = picture
    # get a new photoimage from data
    photoimage = ImageTk.PhotoImage(picture.image)
    # replace the pixmap data:
    myro.globvars.images[name].img = photoimage
    # refresh the canvas:
    myro.globvars.images[name].refresh(myro.globvars.windows[name])

def getWindow(name="default"):
    return myro.globvars.windows[name]

def draw(obj, win=None):
    if win == None:
        win = myro.globvars.windows["default"]
    obj.draw(win)

def undraw(obj):
    obj.undraw()

def getWidth(picture):
    return picture.width

def getHeight(picture):
    return picture.height

def getPixel(picture, x, y):
    return picture.getPixel(x, y)

def getPixels(picture):
    return picture.getPixels()

def setPixel(picture, x, y, color):
    return picture.setColor(x, y, color)

def getGray(picture, x, y):
    return sum((picture.getPixel(x, y)).getRGB())/3

def setGray(picture, x, y, gray):
    return getPixel(picture, x, y).setRGB([gray,gray,gray])

############################# Pixels and Colors

def getX(pixel):
    return pixel.x

def getY(pixel):
    return pixel.y

def getRed(pixel):
    return pixel.getRGB()[0]

def getGreen(pixel):
    return pixel.getRGB()[1]

def getBlue(pixel):
    return pixel.getRGB()[2]

def getColor(pixel):
    return pixel.getColor()

def getGray(pixel):
    return sum(pixel.getRGB())/3

def setRGB(pixel_or_color, rgb):
    return pixel_or_color.setRGB(rgb)

def setRGBA(pixel_or_color, rgba):
    return pixel_or_color.setRGBA(rgba)

def getRGB(pixel_or_color):
    return pixel_or_color.getRGB()

def getRGBA(pixel_or_color):
    return pixel_or_color.getRGBA()

def setRed(pixel, value):
    return pixel.setColor(Color(value, pixel.getRGB()[1], pixel.getRGB()[2]))

def setGreen(pixel, value):
    return pixel.setColor(Color(pixel.getRGB()[0], value, pixel.getRGB()[2]))

def setBlue(pixel, value):
    return pixel.setColor(Color(pixel.getRGB()[0], pixel.getRGB()[1], value))

def setGray(pixel, value):
    return pixel.setColor(Color(value, value, value))

def setAlpha(pixel, value):
    return pixel.setAlpha(value)

def getAlpha(pixel):
    return pixel.getAlpha()

def setColor(pixel, color):
    return pixel.setColor(color)

def makeColor(red, green, blue, alpha=255):
    return Color(red, green, blue, alpha)

def makeDarker(color):
    return color.makeDarker()

def makeLighter(color):
    return color.makeLighter()

def odd(n): return (n % 2) == 1
def even(n): return (n % 2) == 0
def wall(threshold=4500): return getObstacle(1) > threshold 

def loop(*functions):
    """
    Calls each of the given functions sequentially, N times.
    Example:

    >>> loop(f1, f2, 10)
    will call f1() then f2(), 10 times.
    """
    assert len(functions) > 1,"loop: takes 1 (or more) functions and an integer"
    assert type(functions[-1]) == int, "loop: last parameter must be an integer"
    count = functions[-1]
    for i in range(count):
        for function in functions[:-1]:
            print "   loop #%d: running %s()... " % (i + 1, function.__name__),
            try:
                retval = function()
            except TypeError:
                retval = function(i + 1)
            if retval:
                print " => %s" % retval
            else:
                print ""
    stop()
    return "ok"

def doTogether(*functions):
    """
    Runs each of the given functions at the same time.
    Example:

    >>> doTogether(f1, f2, f3)
    will call f1() f2() and f3() together.
    """
    thread_results = [None] * len(functions)
    def makeThread(function, position):
        def newfunction():
            result = function()
            thread_results[position] = result
            return result
        import threading
        thread = threading.Thread()
        thread.run = newfunction
        return thread
    assert len(functions) >= 2, "doTogether: takes 2 (or more) functions"
    thread_list = []
    # first make the threads:
    for i in range(len(functions)):
        thread_list.append(makeThread(functions[i], i))
    # now, start them:
    for thread in thread_list:
        thread.start()
    # wait for them to finish:
    for thread in thread_list:
        thread.join()
    if thread_results == [None] * len(functions):
        print 'ok'
    else:
        return thread_results

def beepScale(duration, start, stop, factor=2):
    """
    Calls computer.beep(duration, Hz) repeatedly, where Hz is between
    the given start and stop frequencies, incrementing by the given
    factor.
    """
    hz = start
    while hz <= stop:
        computer.beep(duration, hz)
        hz *= factor

def getFilenames(pattern):
    """ Get a list of filenames via a pattern, like "z??.jpg"."""
    filenames = glob.glob(pattern)
    filenames.sort() # get in order, back to front
    return filenames

############################

def _startSimulator():
    globalspath, filename = os.path.split(myro.globvars.__file__)
    myro.globvars.myropath, directory = os.path.split(globalspath)
    simulator_file = os.path.join(myro.globvars.myropath, "simulator.py")
    path = myro.globvars.myropath
    if os.name in ['nt', 'dos', 'os2'] :
        if "PYTHONPATH" in os.environ:
            os.environ["PYTHONPATH"] = path + ";" + os.getcwd() + ";" + os.environ["PYTHONPATH"] 
        else:
            os.environ["PYTHONPATH"] = path
        os.system("""start c:\Python24\python.exe "%s" """ % simulator_file)
    elif os.name in ['posix']:
        if "PYTHONPATH" in os.environ:
            os.environ["PYTHONPATH"] = path + ":" + os.getcwd() + ":" + os.environ["PYTHONPATH"]
        else:
            os.environ["PYTHONPATH"] = path
        os.system("""/usr/bin/env python "%s" &""" % simulator_file)
    else:
        raise AttributeError, "your operating system (%s) is not currently supported" % os.name
    
# --------------------------------------------------------
# Error handler:
# --------------------------------------------------------
def _myroExceptionHandler(etype, value, tb):
    # make a window
    #win = HelpWindow()
    lines = traceback.format_exception(etype, value, tb)
    print >> sys.stderr, "Myro is stopping: -------------------------------------------"
    for line in lines:
        print >> sys.stderr, line.rstrip()
sys.excepthook = _myroExceptionHandler

from myro.robots.scribbler import Scribbler
from myro.robots.surveyor import Surveyor, watch
#from myro.robots.roomba import Roomba, Create
from myro.robots.epuck import Epuck
from myro.robots.simulator import SimScribbler
from myro.graphics import *

#######
## have to load pygame after mostly everything
if not "darwin" in sys.platform:
    try:
        import pygame
        pygame.init()
    
        for i in range(pygame.joystick.get_count()):
            js = pygame.joystick.Joystick(i)
            js.init()
            myro.globvars.joysticks.append(js)
    except:
        pygame = None
        pass


_functions = ("timer", 
              "time Remaining", 
              "send Picture",
              "register",
              "set Password",
              "set Forwardness",
              "wait",
              "current Time",
              "pick One",
              "flip Coin",
              "random Number",
              "get Gamepad",
              "get Gamepad Now",
              "ask",
              "request Stop",
              "initialize",
              "simulator",
              "translate",
              "rotate",
              "move",
              "forward",
              "backward",
              "turn",
              "turn Left",
              "turn Right",
              "stop",
              "open Connection",
              "close Connection",
              "get",
              "get Version",
              "get Light",
              "get I R",
              "get Line",
              "get Stall",
              "get Info",
              "get All",
              "get Name",
              "get Start Song",
              "get Volume",
              "update",
              "beep",
              "set",
              "set L E D",
              "set Name",
              "set Volume",
              "set Start Song",
              "motors",
              "restart",
              "joy Stick",
              "calibrate",
              "play Song",
              "play Note",
              "get Bright",
              "get Obstacle",
              "set I R Power",
              "get Battery",
              "set White Balance",
              "set L E D Front",
              "set L E D Back",
              "make Array",
              "take Picture",
              "load Picture",
              "copy Picture",
              "make Picture",
              "write Picture To",
              "save Picture",
              "show",
              "repaint",
              "get Width",
              "get Height",
              "get Pixel",
              "get Pixels",
              "set Pixel",
              "get X",
              "get Y",
              "get Red",
              "get Green",
              "get Blue",
              "get Color",
              "set Red",
              "set Green",
              "set Blue",
              "set Color",
              "make Color",
              "make Darker",
              "make Lighter",
              )

myro.globvars.makeEnvironment(locals(), _functions)
