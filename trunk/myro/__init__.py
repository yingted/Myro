"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__BUILD__    = "$Build: 7 $"
__VERSION__  = "0.3." + __BUILD__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@cs.brynmawr.edu>"

import sys, atexit, time, os, random
import myro.globals
try:
    import tkSnack
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

class Robot(object):
    def __init__(self):
        """
        Base robot class.
        """
        pass
    
    def translate(self, amount):
        raise AttributeError, "this method needs to be written"

    def rotate(self, amount):
        raise AttributeError, "this method needs to be written"

    def move(self, translate, rotate):
        raise AttributeError, "this method needs to be written"

    def beep(self, duration, frequency, frequency2 = None):
        raise AttributeError, "this method needs to be written"

    def readLight(self, position):
        raise AttributeError, "this method needs to be written"

    def readIR(self, position):
        raise AttributeError, "this method needs to be written"

    def readLine(self, position):
        raise AttributeError, "this method needs to be written"

    def readStall(self):
        raise AttributeError, "this method needs to be written"

    def setLED(self, position, value):
        raise AttributeError, "this method needs to be written"

### The rest of these methods are just rearrangements of the above

    def read(self, sensor, *positions):
        sensor = sensor.lower()
        if sensor == "stall":
            return self.readStall()
        else:
            retvals = []
            if len(positions) == 0:
                if sensor == "light":
                    positions = range(3)
                elif sensor == "ir":
                    positions = range(2)
                elif sensor == "line":
                    positions = range(2)
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            for position in positions:
                if sensor == "light":
                    retvals.append(self.readLight(position))
                elif sensor == "ir":
                    retvals.append(self.readIR(position))
                elif sensor == "line":
                    retvals.append(self.readLine(position))
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def set(self, item, position, value):
        item = item.lower()
        if item == "led":
            return self.setLED(position, value)
        else:
            raise ("invalid item name: '%s'" % item)

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

from myro.robot.scribbler import Scribbler

class SimScribbler(Robot):
    def __init__(self, id):
        import myro.simulator
        globalspath, filename = os.path.split(myro.globals.__file__)
        myro.globals._myropath, directory = os.path.split(globalspath)
        self._simulator = myro.simulator.INIT(
            os.path.join(myro.globals._myropath, "worlds", "MyroWorld"))
        if (tkSnack):
            tkSnack.initializeSnack(myro.globals._gui)
        for port in self._simulator.ports:
            print "Simulator starting listener on port", port, "..."
            thread = myro.simulator.Thread(self._simulator, port)
            thread.start()
        # start the client(s):
        from myro.robot.symbolic import TCPRobot
        self._clients = []
        for port in self._simulator.ports:
            self._clients.append(TCPRobot("localhost", port))
        myro.globals._robot = self
        myro.globals._simulator = self._simulator
        atexit.register(_cleanup) # FIX: hack to get _cleanup called before Tk exitfunc, which hangs
    def translate(self, amount):
        return self._clients[0].translate(amount)
    def rotate(self, amount):
        return self._clients[0].rotate(amount)
    def move(self, translate, rotate):
        return self._clients[0].move(translate, rotate)
    def readLight(self, pos):
        self._clients[0].update()
        return self._clients[0].light[0].value[pos]
    def readIR(self, pos):
        self._clients[0].update()
        return self._clients[0].ir[0].value[pos]
    def readLine(self, pos):
        self._clients[0].update()
        return self._clients[0].line[0].value[pos]
    def readStall(self):
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
    if myro.globals._robot != None:
        myro.globals._robot.stop() # hangs?
	time.sleep(.5)
        myro.globals._robot.close()
    if myro.globals._simulator != None:
       myro.globals._simulator.destroy()

# Get ready for user prompt; set up environment:
if not myro.globals._setup:
    myro.globals._setup = 1
    atexit.register(_cleanup)
    # Ok, now we're ready!
    print >> sys.stderr, "Myro, (c) 2006 Institute for Personal Robots in Education"
    print >> sys.stderr, "Version %s, Revision %s, ready!" % (__VERSION__, __REVISION__.split()[1])

## Non-object interface:

def initialize(id):
    global robot
    myro.globals._robot = Scribbler(id)
    robot = myro.globals._robot
def simulator(id):
    global robot
    myro.globals._robot = SimScribbler(id)
    robot = myro.globals._robot
def translate(amount):
    return myro.globals._robot.translate(amount)
def rotate(amount):
    return myro.globals._robot.rotate(amount)
def move(translate, rotate):
    return myro.globals._robot.move(rotate, translate)
def forward(amount):
    return myro.globals._robot.forward(amount)
def backward(amount):
    return myro.globals._robot.backward(amount)
def turn(direction, amount = .8):
    return myro.globals._robot.turn(direction, amount)
def turnLeft(amount):
    return myro.globals._robot.left(amount)
def turnRight(amount):
    return myro.globals._robot.right(amount)
def stop():
    return myro.globals._robot.stop()
def openConnection():
    return myro.globals._robot.open()
def closeConnection():
    return myro.globals._robot.close()
def read(sensor, *pos):
    return myro.globals._robot.read(sensor, *pos)
def readLight(pos):
    return myro.globals._robot.readLight(pos)
def readIR(pos):
    return myro.globals._robot.readIR(pos)
def readLine(pos):
    return myro.globals._robot.readLine(pos)
def readStall():
    return myro.globals._robot.readStall()
def update():
    return myro.globals._robot.update()
def beep(self, duration, frequency, frequency2 = None):
    return myro.globals._robot.beep(duration, frequency, frequency2)
def set(item, position, value):
    return myro.globals._robot.set(item, position, value)
def setLED(position, value):
    return myro.globals._robot.setLED(position, value)
def motors(left, right):
    return myro.globals._robot.motors(left, right)
def restart():
    return myro.globals._robot.restart()

# --------------------------------------------------------
# Error handler:
# --------------------------------------------------------
import traceback
try:
    import Tkinter
except:
    Tkinter = None

class HelpWindow(Tkinter.Toplevel): 
    def __init__(self):
        root = None
        Tkinter.Toplevel.__init__(self, root)
        root.withdraw()
        self.frame = Tkinter.Frame(self)
        self.frame.pack(side = 'bottom', expand = "yes", anchor = "n",
                        fill = 'both')

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
    if myro.globals._robot != None:
        print "Stopping robot..."
        myro.globals._robot.stop()
    raise KeyboardInterrupt
signal.signal(signal.SIGINT, _interruptHandler)
# --------------------------------------------------------
