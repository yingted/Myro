"""
Code to allow users to chat with each other.
"""

import xmpp, threading, time

class Chat:
    def __init__(self, name, password):
        self.lock = threading.Lock()
        self.messages = []
        # Start a thread up in here
        self.name = name
        self.password = password
        self.masterServer = "mightymouse.brynmawr.edu"
        self.masterJID = "test@" + self.masterServer
        self.masterPassword = "test"
        jid = xmpp.protocol.JID(self.masterJID)
        self.client = xmpp.Client(jid.getDomain(),debug=[])
        print "Making master connection to server..."
        self.client.connect()
        print "Authenticating master password..."
        self.client.auth(jid.getNode(),self.masterPassword)
        print "Registering '%s'..." % self.name
        self.register(self.name, self.password)
        self.close()
        self.open()

    def register(self, name, password):
        xmpp.features.register(self.client, self.masterServer,
                               {"username": name,
                                "password": password})
    def messageCB(self, conn, msg):
        self.lock.acquire()
        self.messages.append(msg)
        self.lock.release()

    def receive(self):
        self.client.Process(1) # this should be in a thread
        self.lock.acquire()
        retval = self.messages
        self.messages = []
        self.lock.release()
        return retval

    def send(self, to, message):
        self.client.send(xmpp.protocol.Message(to + "@" + self.masterServer,
                                               message))

    def open(self):
        jid = xmpp.protocol.JID(self.name + "@" + self.masterServer)
        self.client = xmpp.Client(jid.getDomain(),debug=[])
        print "Making new connection to server..."
        self.client.connect()
        print "Authenticating '%s' password..." % self.name
        self.client.auth(jid.getNode(),self.password)
        print "Registering message handler..."
        self.client.RegisterHandler('message', self.messageCB) 
        self.client.sendInitPresence()
        self.send("", "2") # make this the only place I'm logged in        
        messages = self.receive()
        while len(messages) == 0:
            print "waiting for response..."
            time.sleep(1)
            messages = self.receive()
        print "Done!"

    def close(self):
        self.client.disconnect()
        print "Disconnected!"

    def __del__(self):
        self.close()
