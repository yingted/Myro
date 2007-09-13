"""
Module that supports robot-to-robot communication via IM.

This module uses the jabber protocol for allowing
robot-to-robot communication. You can run your own
server and use that, as long as it supports
open registration of new users.

See the Myro Technical Guide for more details at:
http://wiki.roboteducation.org/
"""

import threading, time, random, pickle, sys

__version__ = "$Revision$"
__author__  = "Doug Blank <dblank@cs.brynmawr.edu>"

class FakeFile:
    """ To trick stderr into not printing warnings from xmpp. """
    def write(self, *args, **keys):
        pass
temp = sys.stderr        # save the stderr
sys.stderr = FakeFile()  # replace with fake
try:
    import xmpp              # import xmpp
except:
    xmpp = None
    print "WARNING: xmpp was not found: chat and webservices will not be available"
sys.stderr = temp        # replace stderr
temp = None              # clean up
del temp

class RemoteRobot:
    def __init__(self, name, debug = []):
        self.name = name
        self.returnValues = {}
        self.chat = Chat("control-%05d" % random.randint(1,10000),
                         "password", debug)
    def __repr__(self):
        return "<myro.robot.chat.RemoteRobot object>"

    def _eval(self, item, *args, **kwargs):
        commandArgs = ""
        for a in args:
            if commandArgs != "":
                commandArgs += ", "
            commandArgs += str(a)
        for a in kwargs.keys():
            if commandArgs != "":
                commandArgs += ", "
            commandArgs += a + "=" + str(kwargs[a])
        self.chat.send(self.name.lower(), "robot." + item + "(" + commandArgs + ")")
        retval = self.chat.receive()
        while len(retval) == 0:
            #print "waiting to receive from remote robot..."
            retval = self.chat.receive()
        values = []
        for _from, s in retval:
            values.append(pickle.loads(s))
        if len(values) == 1:
            return values[0]
        else:
            return values

    def __getattr__(self, item):
        def getArgs(*args, **kwargs):
            return self._eval(item, *args, **kwargs)
        return getArgs

class LocalRobot:
    def __init__(self, robot, name, password, debug = []):
        self.chat = Chat(name, password, debug)
        self.robot = robot

    def process(self):
        messages = self.chat.receive()
        for _from, m in messages:
            if message.startswith("robot."):
                # For user IM messages
                print "self." + command
                retval = eval("self.robot." + command)
                self.chat.send(_from.lower(), repr(retval))
            else:
                print _from + ":", message

    def run(self):
        while 1:
            self.process()
            time.sleep(1)
        

class Chat(object):
    def __init__(self, name, password, debug = []):
	"""
        Constructs a connection for communicating to an IM server.

        name: can be "laura" to login into the default IM server,
        or can be "laura@yourserver.edu" to connect to the server
        running at "yourserver.edu".

        password: any password of your choosing. First time use
        will require the password to match the username on subsequent
        use.

        debug: can be [] for no debugging information, or can be
        ['always'] to get see all of the debugging information.
        """
        self.lock = threading.Lock()
        self.messages = []
        # Start a thread up in here
        self.password = password
	if "@" not in name:
	    self.name, self.server = name, "myro.roboteducation.org"
	else:
            self.name, self.server = name.split("@")
	self.debug = debug
        self.client = xmpp.Client(self.server, debug=self.debug)
        print "Making connection to server..."
        self.client.connect()
        print "Registering '%s'..." % self.name
        self.register(self.name.lower(), self.password)
        try:
            self.open()
        except AttributeError:
            print "Help! It appears that the Myro Chat Server is down."

    def register(self, name, password):
	""" Register a username/password. """
        xmpp.features.register(self.client, self.server,
                               {"username": name.lower(),
                                "password": password})

    def messageCB(self, conn, msg):
	""" Message handling callback function. """
        self.lock.acquire()
        self.messages.append(msg)
        self.lock.release()

    def receive(self):
	"""
	Get all of the pending messages, and return them as a list.
	"""
	try:
            self.client.Process(1) # this should be in a thread
        except xmpp.NotAuthorized:
            raise ValueError, "bad password?"
        self.lock.acquire()
        retval = self.messages
        self.messages = []
        self.lock.release()
        retvalList = []
        for m in retval:
            fromName = str(m.getFrom().node + "@" + m.getFrom().getDomain())
            message = str(m.getBody())
            retvalList.append( (fromName, message) )
        return retvalList

    def send(self, to, message):
	""" 
	Send a message to a named recipient. They must be logged in.
	"""
        self.client.send(
           xmpp.protocol.Message(to.lower() + "@" + self.server, message))

    def open(self):
	"""
	Open a connection to the server.
	"""
        print "Authenticating password for '%s'..." % self.name
        try:
            self.client.auth(self.name.lower(), self.password)
        except IOError:
            self.client = xmpp.Client(self.server, debug=self.debug)
            self.client.connect()
            self.client.auth(self.name.lower(), self.password)
        print "Registering message handler..."
        self.client.RegisterHandler('message', self.messageCB) 
        self.client.sendInitPresence()
        self.send("", "2") # make this the only place I'm logged in        
        messages = self.receive()
        count = 0
        while len(messages) == 0 and count < 5:
            print "   waiting for response..."
            time.sleep(1)
            messages = self.receive()
            count += 1
        if count >= 5:
            print "Giving up! Are you sure you have the right password?"
            self.ok = 0
        else:
            print "Done!"
            self.ok = 1

    def close(self):
	"""
	Close the connection to the server.
	"""
        self.client.disconnect()
        print "Disconnected!"

    def __del__(self):
	""" Close the connection on destruction. """
        self.close()
