"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__VERSION__  = "0." + __REVISION__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@brynmawr.edu>"

import sys, atexit, posixpath, time
import myro.globals

class Robot(object):
    def __init__(self):
        """
        Base robot class.
        """
        self.devices = []

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

    def _getNextDeviceNumber(self, devname):
        """
        Gets the next device number of a particular type.

        >>> robot._getNextDeviceNumber("sonar")
        0
        >>> robot._getNextDeviceNumber("sonar")
        1
        """
        if devname not in self.__dict__:
            self.devices.append( devname ) # keep track of all of the loaded types
            self.__dict__[devname] = [None]
            return 0
        else:
            self.__dict__[devname].append( None )
            return len(self.__dict__[devname]) - 1

    def startDevice(self, item, **args):
        """
        Loads and starts a device.

        item - can be a builtin or a filename. Filenames should start
               with an uppercase letter.

        Returns a pointer to the device object.

        >>> robot.startDevice("camera")
        <Object>
        >>> robot.startDevice("FilenameDevice")
        <Object>
        """
        dev = self.startDevices(item, **args)
        if len(dev) < 1:
            print "Error loading device: '%s'" % item
        else:
            return dev[0]
        
    def startDevices(self, item, override = False, **args):
        """Load devices can take a dict, list, builtin name, or filename """
        # Item can be: dict, list, or string. string can be name or filename
        if type(item) == type({}):
            # this is the only one that does anything
            retval = []
            for dev in item.keys():
                deviceNumber = self._getNextDeviceNumber(dev)
                print "Loading device %s[%d]..." % (dev, deviceNumber)
                self.__dict__[dev][deviceNumber] = item[dev]
                item[dev].setTitle( dev + "[" + str(deviceNumber) + "]" )
                item[dev].index = deviceNumber
                retval.append(item[dev]) # return object
            return retval
        elif item in self.builtinDevices: # built-in name
            # deviceBuiltin returns dictionary
            deviceList = self.startDeviceBuiltin(item)
            if type(deviceList) == type("device"): # loaded it here, from the robot
                return [ deviceList ]
            else:
                return self.startDevices( deviceList, **args ) # dict of objs
        elif isinstance(item, (type((1,)), type([1,]))):
            retval = []
            for i in item:
                retval.append( self.startDevice(i, **args) )
            return retval
        else: # from a file
            file = item
            if file == None:
                return []
            if len(file) > 3 and file[-3:] != '.py':
                file = file + '.py'
            if file_exists(file):
                return self.startDevices( loadINIT(file, self), **args )
            elif file_exists(os.getenv('PYROBOT') + \
                                    '/plugins/devices/' + file): 
                return self.startDevices( loadINIT(os.getenv('PYROBOT') + \
                                                   '/plugins/devices/'+ \
                                                   file, self), **args)
            else:
                print 'Device file not found: ' + file
                return []

class Scribbler(Robot):
    def __init__(self, id):
        Robot.__init__(self)
        self.id = id
    # add Bluetooth/Serial interface stuff here

class SimScribbler(Scribbler):
    def __init__(self, id):
        import myro.simulator
        Scribbler.__init__(self, id)
        globalspath, filename = posixpath.split(myro.globals.__file__)
        myro.globals._myropath, directory = posixpath.split(globalspath)
        self.sim = myro.simulator.INIT(
            posixpath.join(myro.globals._myropath, "worlds", "MyroWorld"))
        for port in self.sim.ports:
            print "Simulator starting listener on port", port, "..."
            thread = myro.simulator.Thread(self.sim, port)
            thread.start()
        # start the client(s):
        from myro.robot.symbolic import TCPRobot
        self.client = []
        for port in self.sim.ports:
            self.client.append(TCPRobot("localhost", port))
        myro.globals._robot = self
        myro.globals._simulator = self.sim
        atexit.register(_cleanup) # FIX: hack to get _cleanup called before Tk exitfunc, which hangs
    def translate(self, amount):
        self.client[0].translate(amount)
    def rotate(self, amount):
        self.client[0].rotate(amount)
    def move(self, translate, rotate):
        self.client[0].move(translate, rotate)
    def forward(self, amount):
        self.client[0].translate(amount)
    def backward(self, amount):
        self.client[0].translate(-amount)
    def left(self, amount):
        self.client[0].rotate(amount)
    def right(self, amount):
        self.client[0].rotate(-amount)
    def stop(self):
        self.client[0].move(0, 0)
    def quit(self):
        self.client[0].move("quit")

# functions:
def _cleanup():
    if myro.globals._robot != None:
        myro.globals._robot.quit()
    if myro.globals._simulator != None:
       myro.globals._simulator.destroy()

# Get ready for user prompt; set up environment:
if not myro.globals._setup:
    myro.globals._setup = 1
    atexit.register(_cleanup)
    # Ok, now we're ready!
    print "Myro, (c) 2006 Institute for Personal Robots in Education"
    print "Version %s, ready!" % (__VERSION__)
