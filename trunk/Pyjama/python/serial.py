import clr
# FIXME: weirdness trying to import from dlls:
clr.AddReference("System")
import System
import System.IO
from System.IO.Ports import SerialPort
import string
from serialutil import *

VERSION = string.split("$Revision$")[1]

class Serial(SerialBase):
    def __init__(self, *args, **kwargs):
        self.ser = SerialPort()
        SerialBase.__init__(self, *args, **kwargs)
        self.ser.NewLine = '\n'
        self.portstr = self.ser.PortName

    def _reconfigurePort(self):
        pass

    def open(self):
        self.ser.Open()

    def getTimeout(self):
        return int(self.ser.ReadTimeout / 1000.0) # Milliseconds to Seconds

    def setTimeout(self, timeout):
        if timeout == None:
            self.ser.ReadTimeout = -1
        else:
            self.ser.ReadTimeout = int(timeout * 1000) # Seconds to Milliseconds

    def getWriteTimeout(self):
        return int(self.ser.WriteTimeout / 1000.0) # Milliseconds to Seconds

    def setWriteTimeout(self, timeout=None):
        if timeout == None:
            self.ser.WriteTimeout = -1
        else:
            self.ser.WriteTimeout = int(timeout * 1000) # Seconds to Milliseconds

    def getPort(self):
        return self.ser.PortName 

    def setPort(self, value):
        self.ser.PortName = value
        self.portstr = value

    def getByteSize(self):
        return self.ser.DataBits

    def setByteSize(self, value):
        self.ser.DataBits = value

#     def setXonXoff(self, xonxoff):
#         """Change XonXoff setting."""
        
    
#     def getXonXoff(self):
#         """Get the current XonXoff setting."""

#     def setDsrDtr(self, dsrdtr=None):
#         """Change DsrDtr flow control setting."""
#         self.ser.DtrEnable
#         self.ser.DsrHolding
#         self.ser.Handshake = dsrdtr

#     def getDsrDtr(self):
#         """Get the current DsrDtr flow control setting."""
#         self.ser.DtrEnable
#         self.ser.DsrHolding
#         return self.ser.Handshake

    def getParity(self):
        return self.ser.Parity

    def setParity(self, value):
        if value == 'N':
            value = self.ser.Parity.None
        elif value == 'O':
            value = self.ser.Parity.Odd
        elif value == 'E':
            value = self.ser.Parity.Even
        self.ser.Parity = value

    def getStopbits(self):
        return self.ser.StopBits

    def setStopbits(self, value):
        if value == 1:
            value = self.ser.StopBits.One
        elif value == 1.5:
            value = self.ser.StopBits.OnePointFive
        elif value == 2:
            value = self.ser.StopBits.Two
        else:
            print "ignoring stopbit value:", value
            return
        self.ser.StopBits = value

    def setBaudrate(self, value):
        self.ser.BaudRate = value

    def getBaudrate(self):
        return self.ser.BaudRate 

    def flushInput(self):
        timeout = self.timeout 
        self.timeout = .1
        self.read(100000);
        self.timeout = timeout

    def flushOutput(self):
        pass

    def read(self, size=1):
        retval = ''
        try:
            b = self.ser.ReadByte()
            retval += chr(b)
            while len(retval) < size:
                b = self.ser.ReadByte() # or ReadChar
                retval += chr(b)
        except:
            #print "timeout"
            pass
        return retval

    def inWaiting(self):
        # FIXME
        return 0

    def write(self, s):
        return self.ser.Write(s) # FIXME: what is the newline? \r

    def close(self):
        self.ser.Close()

    port = property(getPort, setPort, doc="Port setting")
    baudrate = property(getBaudrate, setBaudrate, doc="Baudrate setting")
    bytesize = property(getByteSize, setByteSize, doc="Byte size setting")
    parity = property(getParity, setParity, doc="Parity setting")
    stopbits = property(getStopbits, setStopbits, doc="Stopbits setting")
    timeout = property(getTimeout, setTimeout, doc="Timeout setting for read()")
    writeTimeout = property(getWriteTimeout, setWriteTimeout, doc="Timeout setting for write()")
    #xonxoff = property(getXonXoff, setXonXoff, doc="Xon/Xoff setting")
    #rtscts = property(getRtsCts, setRtsCts, doc="RTS/CTS flow control setting")
    #dsrdtr = property(getDsrDtr, setDsrDtr, "DSR/DTR flow control setting")
