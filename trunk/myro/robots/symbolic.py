"""
The client robot connection programs for the PyrobotSimulator
non-symbolic robots.

(c) 2005, PyrobRobotics.org. Licenced under the GNU GPL.
"""

__author__ = "Douglas Blank <dblank@brynmawr.edu>"
__version__ = "$Revision$"

import socket, threading, random, time
from myro.robots import Robot
from myro.robots.device import * # Device, GripperDevice, SensorValue
try:
	import cPickle as pickle
except:
	import pickle

colorMap = {"red": (255, 0,0),
            "green": (0, 255,0),
            "blue": (0, 0,255),
            "white": (255, 255, 255),
            "black": (0, 0, 0),
            "cyan": (0, 255, 255),
            "yellow": (255, 255, 0),
            "brown": (165, 42, 42),
            "orange": (255, 165, 0),
            "pink": (255, 192, 203),
            "violet": (238, 130, 238),
            "purple": (160, 32, 240),
            }
colorUnCode = {1: colorMap["red"],
	       2: colorMap["green"],
	       3: colorMap["blue"],
	       4: colorMap["white"],
	       5: colorMap["black"],
	       6: colorMap["cyan"],
	       7: colorMap["yellow"],
	       8: colorMap["brown"],
	       9: colorMap["orange"],
	       10: colorMap["pink"],
	       11: colorMap["violet"],
	       12: colorMap["purple"],
	       }

class PositionSimDevice(Device):
	def __init__(self, robot):
		Device.__init__(self, "position")
		self._dev = robot
		self.startDevice()
	def addWidgets(self, window):
		window.addData("x", ".x:", self._dev.x)
		window.addData("y", ".y:", self._dev.y)
		window.addData("thr", ".th (angle in radians):", self._dev.th)
		window.addData("th", ".thr (angle in degrees):", self._dev.thr)
		window.addData("stall", ".stall:", self._dev.stall)
	def updateWindow(self):
		if self.visible:
			self.window.updateWidget("x", self._dev.x)
			self.window.updateWidget("y",self._dev.y)
			self.window.updateWidget("thr", self._dev.th)
			self.window.updateWidget("th", self._dev.thr)
			self.window.updateWidget("stall",self._dev.stall)

class SimulationDevice(Device):
	def __init__(self, robot):
		Device.__init__(self, "simulation")
		self._dev = robot
		self.startDevice()
	def setPose(self, name, x = 0, y = 0, thr = 0):
		self._dev.move("a_%s_%f_%f_%f" % (name, x, y, thr))
		self._dev.localize(0,0,0)
		return None
	def getPose(self, name):
		retval = self._dev.move("c_%s" % (name, ))
		return retval
	def eval(self, command):
		retval = self._dev.move("!%s" % (command,))
		return retval
	def addWidgets(self, window):
		window.addCommand("eval", "Evaluate exp!", "self.", self.onCommand)
	def onCommand(self, command):
		return self.eval(command)

class RangeSimDevice(Device):
	def __init__(self, name, index, robot, geometry, groups):
		Device.__init__(self, name)
		self._geometry = geometry
		self.groups = groups
		self.startDevice()
		self._dev = robot
		self.index = index
		self.maxvalueraw = geometry[2]
		self.rawunits = "M"
		self.units = "ROBOTS"
		self.radius = robot.radius
		self.count = len(self)
		self._noise = [0.00] * self.count
		
	def __len__(self):
		return len(self._geometry[0])
	def getSensorValue(self, pos):
		try:
			v = self._dev.__dict__["%s_%d" % (self.type, self.index)][pos]
		except:
			v = 0.0
		try:
			value = SensorValue(self, v, pos,
					    (self._geometry[0][pos][0], # x in meters
					     self._geometry[0][pos][1], # y
					     0.03,                    # z
					     self._geometry[0][pos][2], # th
					     self._geometry[1]),        # arc rads
					    noise=self._noise[pos]
					    )
		except:
			value = SensorValue(self, 0, 0,
					    (self._geometry[0][pos][0], # x in meters
					     self._geometry[0][pos][1], # y
					     0.03,                    # z
					     self._geometry[0][pos][2], # th
					     self._geometry[1]),        # arc rads
					    noise=self._noise[pos]
					    )
		return value

class LineSimDevice(Device):
	def __init__(self, name, index, robot, geometry, groups):
		Device.__init__(self, name)
		self._geometry = geometry # [(x, y, z), ]
		self.groups = groups
		self.startDevice()
		self._dev = robot
		self.index = index
		self.maxvalueraw = 1.0
		self.rawunits = "M"
		self.units = "RAW"
		self.count = len(self)
		self._noise = [0.0] * self.count
		self._data = [0] * len(geometry)
	def updateDevice(self):
		if self.active == 0: return
		newData = self._dev.move("line_%d" % 0) # get all of the data
		for i in range(len(newData)):
			self._data[i] = newData[i]
	def __len__(self):
		return len(self._geometry)
	def getSensorValue(self, pos):
		v = self._data[pos]
		value = SensorValue(self, v, pos,
				    (self._geometry[pos][0], # x in meters
				     self._geometry[pos][1], # y
				     0.03,                    # z
				     self._geometry[pos][2], # th
				     0.0),        # arc rads
				    noise=self._noise[pos]
				    )
		return value

class LightSimDevice(RangeSimDevice):
	def __init__(self, *args, **kwargs):
		RangeSimDevice.__init__(self, *args, **kwargs)
		self.units = "SCALED"
	def _getRgb(self):
		retval = []
		for i in range(len(self)):
			retval.append( self.getSensorValue(i).rgb )
		return retval
	def getSensorValue(self, pos):
		retval = RangeSimDevice.getSensorValue(self, pos)
		if retval != None:
			retval.rgb = self._dev.move("f_%d_%d" % (self.index, pos)) # rgb
		return retval
	rgb = property(_getRgb)
	def addWidgets(self, window):
		for i in range(min(self.count, 24)):
			window.addData("%d.value" % i, "[%d].value:" % i, self[i].value)
			window.addData("%d.rgb" % i, "[%d].rgb:" % i, self[i].rgb)
	def updateWindow(self):
		if self.visible:
			for i in range(min(self.count, 24)):
				self.window.updateWidget("%d.value" % i, self[i].value)
				self.window.updateWidget("%d.rgb" % i, self[i].rgb)

class BulbSimDevice(Device):
	def __init__(self, robot):
		Device.__init__(self)
		self.type = "bulb"
		self._dev = robot
	def setBrightness(self, value):
		return self._dev.move("h_%f" % value)
	def addWidgets(self, window):
		b = 1.0
		window.addCommand("brightness", "Brightness!", str(b),
				  lambda b: self.setBrightness(float(b)))
		
class PTZSimDevice(Device):
	def __init__(self, robot):
		Device.__init__(self)
		self.type = "ptz"
		self._dev = robot
	def setPose(self, p, t, z):
		return self._dev.move("j_%d_%s_%s_%s" % (0, p, t, z))
	def getPose(self):
		return self._dev.move("k_%d" % 0)
	def addWidgets(self, window):
		p, t, z = self._dev.move("k_%d" % 0)
		window.addCommand("pan", "Pan!", str(p), self.onPan)
		window.addCommand("tilt", "Tilt!", str(t), self.onTilt)
		window.addCommand("zoom", "Zoom!", str(z), self.onZoom)
	def onPan(self, command):
		return self.setPose(command, None, None)
	def onTilt(self, command):
		return self.setPose(None, command, None)
	def onZoom(self, command):
		return self.setPose(None, None, command)
	
class GripperSimDevice(GripperDevice):
	def __init__(self, robot):
		Device.__init__(self)
		self.type = "gripper"
		self._dev = robot
		self.data = [0, 0, 0, 0, 0]
		self.startDevice()
	def updateDevice(self):
		if self.active == 0: return
		newData = self._dev.move("gripper_%d" % 0)
		for i in range(len(newData)):
			self.data[i] = newData[i]
	def close(self):  return self._dev.move("z_gripper_0_close")
	def open(self):   return self._dev.move("z_gripper_0_open")
	def up(self):     pass
	def down(self):   pass
	def stop(self):   return self._dev.move("z_gripper_0_stop")
	def store(self):  return self._dev.move("z_gripper_0_store")
	def deploy(self): return self._dev.move("z_gripper_0_deploy")
	def halt(self):   return self.stop()
	# accessor values
	def getBreakBeam(self, which):
		if which == 'inner':
			return self.data[0]
		elif which == 'outer':
			return self.data[1]
		else:
			raise AttributeError, "invalid breakBeam: '%s'" % which
	def isClosed(self): return self.data[2]
	def isOpened(self): return self.data[3]
	def isMoving(self): return self.data[4]
	def isLiftMoving(self): return 0

class Simbot(Robot):
	def __init__(self, simulator, port, connectionNum, startDevices=1):
		Robot.__init__(self)
		self.simulator = simulator
		self.connectionNum = connectionNum
		self.port = port
		self.init(startDevices)
	def init(self, startDevices=1):
		self.radius = self.getItem("radius")
		self.properties = self.getItem("properties")
		self.builtinDevices = self.getItem("builtinDevices")
		self.builtinDevices.append("simulation")
		self.builtinDevices.append("position")
		self.supportedFeatures = self.getItem("supportedFeatures")
		self.name = self.getItem("name")
		self.id   = self.connectionNum
		if startDevices:
			for dev in self.builtinDevices:
				d = self.startDevice(dev)
				if dev in ["sonar", "laser", "ir"]:
					self.range = d
	def move(self, message, other = None):
		if type(message) in [type(1), type(1.)] and type(other) in [type(1), type(1.)]:
			message = "m_%.2f_%.2f" % (message, other)
			other = None
		retval = None
		if other != None: return # rotate,translate command ignored
		if message == "quit" or message == "exit" or message == "end" or message == "disconnect":
			self.simulator.process(message, self.port, 0)
			return None
		else:
			retval = self.simulator.process(message, self.port, 0)
		return retval
	def disconnect(self): pass
	def localize(self, x = 0, y = 0, th = 0):
		pass
	def update(self):
		for i in self.properties:
			self.__dict__[i] = self.getItem(i)
		self.updateDevices()
	def getItem(self, item):
		return self.move(item)
	def eat(self, amt):
		return self.move("e_%f" % (amt))
	def play(self, item):
		return self.move(item)
	def tell(self, item):
		return self.move(item)
	def ask(self, item):
		return self.move(item)
	def _moveDir(self, dir):
		if dir == 'L':
			self.move("left")
		elif dir == 'R':
			self.move("right")
		elif dir == 'F':
			self.move("forward")
		elif dir == 'B':
			self.move("back")
	def localize(self, x = 0, y = 0, thr = 0):
		return self.move("b_%f_%f_%f" % (x, y, thr))

	def startDeviceBuiltin(self, name, index = 0):
		if name == "simulation":
			return {"simulation": SimulationDevice(self)}
		elif name == "bulb":
			self.move("s_%s_%d" % (name, index))
			return {name: BulbSimDevice(self)}
		elif name == "ptz":
			self.move("s_%s_%d" % (name, index))
			return {name: PTZSimDevice(self)}
		elif name == "gripper":
			self.move("s_%s_%d" % (name, index))
			return {name: GripperSimDevice(self)}
		elif name == "position":
			return {name: PositionSimDevice(self)}
		elif name == "light":
			self.properties.append("%s_%d" % (name, index))
			self.move("s_%s_%d" % (name, index))
			geometry = self.move("g_%s_%d" % (name, index))
			groups = self.move("r_%s_%d" % (name, index))
			return {name: LightSimDevice(name, index, self, geometry, groups)}
		elif name == "line":
			self.properties.append("%s_%d" % (name, index))
			self.move("s_%s_%d" % (name, index))
			geometry = self.move("g_%s_%d" % (name, index))
			groups = self.move("r_%s_%d" % (name, index))
			return {name: LineSimDevice(name, index, self, geometry, groups)}
		else:
			self.properties.append("%s_%d" % (name, index))
			self.move("s_%s_%d" % (name, index))
			geometry = self.move("g_%s_%d" % (name, index))
			groups = self.move("r_%s_%d" % (name, index))
			dev = RangeSimDevice(name, index, self, geometry, groups)
			if name == "bumper":
				dev.units = "SCALED"
			return {name: dev}

	def translate(self, value):
		self.move("t_%f" % value)

	def rotate(self, value):
		self.move("o_%f" % value)

class TCPRobot(Simbot):
	"""
	A simple TCP-based socket robot for talking to PyrobotSimulator.
	"""
	BUFSIZE = 4096 # 2048 # 1024
	def __init__(self, host, port, startDevices=1):
		Robot.__init__(self)
		self.lock = threading.Lock()
		# Set the socket parameters
		self.host = host
		self.port = port
		self.addr = (host, port)
		self.type = "Pyrobot"
		# Create socket
		self.socket = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
		try:
			self.socket.settimeout(1)
		except:
			print "WARN: entering deadlock zone; upgrade to Python 2.3 to avoid"
		done = 0
		while not done:
			try:
				self.socket.connect( self.addr )
				done = 1
			except:
				print "Waiting on PyrobotSimulator..."
				time.sleep(1)
		self.connectionNum = self.getItem("connectionNum:%d" % self.port)
		self.init(startDevices)

	def move(self, message, other = None):
                #print "message:", message
                self.interruptRequest = 0
                exp = None
                try:
                    self.lock.acquire()
                    if type(message) in [type(1), type(1.)] and type(other) in [type(1), type(1.)]:
                            message = "m_%.2f_%.2f" % (message, other)
                            other = None
                    if self.socket == 0:
                        exp = None # not connected
                    if other != None:
                        exp = None # rotate,translate command ignored
                    if message == "quit" or message == "exit" or message == "end" or message == "disconnect":
                            self.socket.sendto(message, self.addr)
                            self.socket.close()
                            self.socket = 0
                            exp = None
                    else:
                            #print "sending", message, self.addr
                            try:
                                self.socket.sendto(message, self.addr)
                            except:
                                self.socket.close()
                                self.socket = 0
                                exp = None
                            retval = ""
                            try:
                                    retval, addr = self.socket.recvfrom(self.BUFSIZE)
                            except KeyboardInterrupt:
                                    self.interruptRequest = 1
                            except:
                                    retval = ""
                            retval = retval.strip()
                            try:
                                    exp = pickle.loads( retval )
                            except KeyboardInterrupt:
                                    self.interruptRequest = 1
                            except:
                                    exp = retval
                except KeyboardInterrupt:
                    self.interruptRequest = 1
                self.lock.release()
                if self.interruptRequest == 1:
                    self.move(0,0)
                    raise KeyboardInterrupt
		return exp

	def disconnect(self):
		if self.connectionNum == 0: # the main one, let's close up!
			# Close socket
			self.getItem("quit")
		else:
			self.getItem("disconnect")


