"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__BUILD__    = "$Build: 7 $"
__VERSION__  = "0.1." + __BUILD__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@brynmawr.edu>"

import sys, atexit, time, os
import myro.globals

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

    def forward(self, amount):
        raise AttributeError, "this method needs to be written"

    def backward(self, amount):
        raise AttributeError, "this method needs to be written"

    def left(self, amount):
        raise AttributeError, "this method needs to be written"

    def right(self, amount):
        raise AttributeError, "this method needs to be written"

    def quit(self):
        raise AttributeError, "this method needs to be written"

    def stop(self):
        raise AttributeError, "this method needs to be written"

class Scribbler(Robot):
    def __init__(self, id):
        Robot.__init__(self)
        self.id = id
    # add Bluetooth/Serial interface stuff here

class SimScribbler(Scribbler):
    def __init__(self, id):
        import myro.simulator
        Scribbler.__init__(self, id)
        globalspath, filename = os.path.split(myro.globals.__file__)
        myro.globals._myropath, directory = os.path.split(globalspath)
        self._simulator = myro.simulator.INIT(
            os.path.join(myro.globals._myropath, "worlds", "MyroWorld"))
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
    def forward(self, amount):
        return self._clients[0].translate(amount)
    def backward(self, amount):
        return self._clients[0].translate(-amount)
    def left(self, amount):
        return self._clients[0].rotate(amount)
    def right(self, amount):
        return self._clients[0].rotate(-amount)
    def stop(self):
        return self._clients[0].move(0, 0)
    def quit(self):
        return self._clients[0].move("quit")
    def readLight(self, pos):
        self._clients[0].update()
        return self._clients[0].light[0].value[pos]
    def readIR(self, pos):
        self._clients[0].update()
        return self._clients[0].ir[0].value[pos]
    def update(self):
        return self._clients[0].update()
    def beep(self, frequency, duration):
        print chr(7)

# functions:
def _cleanup():
    if myro.globals._robot != None:
        #myro.globals._robot.stop() # causes hang?!
        myro.globals._robot.quit()
    if myro.globals._simulator != None:
       myro.globals._simulator.destroy()

# Get ready for user prompt; set up environment:
if not myro.globals._setup:
    myro.globals._setup = 1
    atexit.register(_cleanup)
    # Ok, now we're ready!
    print >> sys.stderr, "Myro, (c) 2006 Institute for Personal Robots in Education"
    print >> sys.stderr, "Version %s, ready!" % (__VERSION__)

## Non-object interface:

def initialize(id):
    myro.globals._robot = Scribbler(id)
def simulator(id):
    myro.globals._robot = SimScribbler(id)
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
def left(amount):
    return myro.globals._robot.left(amount)
def right(amount):
    return myro.globals._robot.right(amount)
def stop():
    return myro.globals._robot.stop()
def quit():
    return myro.globals._robot.quit()
def readLight(pos):
    return myro.globals._robot.readLight(pos)
def readIR(pos):
    return myro.globals._robot.readIR(pos)
def update():
    return myro.globals._robot.update()
def beep(frequency, duration):
    return myro.globals._robot.beep(frequency, duration)
