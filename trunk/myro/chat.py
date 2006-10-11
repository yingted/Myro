"""
Module that supports robot-to-robot communication via IM.

This module uses the jabber protocol for allowing
robot-to-robot communication. You can run your own
server and use that, as long as it supports
open registration of new users.

See the Myro Technical Guide for more details at:
http://blog.roboteducation.org/wiki/
"""

import xmpp, threading, time

__version__ = "$Revision$"
__author__  = "Doug Blank <dblank@cs.brynmawr.edu>"

class Chat:
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
	    self.name, self.server = name, "blog.roboteducation.org"
	else:
            self.name, self.server = name.split("@")
	self.debug = debug
        self.client = xmpp.Client(self.server, debug=self.debug)
        print "Making connection to server..."
        self.client.connect()
        print "Registering '%s'..." % self.name
        self.register(self.name, self.password)
        self.open()

    def register(self, name, password):
	""" Register a username/password. """
        xmpp.features.register(self.client, self.server,
                               {"username": name,
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
        self.client.Process(1) # this should be in a thread
        self.lock.acquire()
        retval = self.messages
        self.messages = []
        self.lock.release()
        return retval

    def send(self, to, message):
	""" 
	Send a message to a named recipient. They must be logged in.
	"""
        self.client.send(
           xmpp.protocol.Message(to + "@" + self.server, message))

    def open(self):
	"""
	Open a connection to the server.
	"""
        print "Authenticating password for '%s'..." % self.name
        try:
            self.client.auth(self.name, self.password)
        except IOError:
            self.client = xmpp.Client(self.server, debug=self.debug)
            self.client.connect()
            self.client.auth(self.name, self.password)
        print "Registering message handler..."
        self.client.RegisterHandler('message', self.messageCB) 
        self.client.sendInitPresence()
        self.send("", "2") # make this the only place I'm logged in        
        messages = self.receive()
        while len(messages) == 0:
            print "   waiting for response..."
            time.sleep(1)
            messages = self.receive()
        print "Done!"

    def close(self):
	"""
	Close the connection to the server.
	"""
        self.client.disconnect()
        print "Disconnected!"

    def __del__(self):
	""" Close the connection on destruction. """
        self.close()
