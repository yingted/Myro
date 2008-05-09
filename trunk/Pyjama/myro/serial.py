import clr
import System.IO.Ports.SerialPort as SerialPort
import string

from serialutil import *

VERSION = string.split("$Revision$")[1]

class Serial(SerialBase):
    def __init__(self, *args, **kwargs):
        SerialBase.__init__(self, *args, **kwargs)
        self.ser = SerialPort(self.portstr, self.baudrate)
        try:
            self.ser.Open()
        except:
            raise SerialException("can't open serial port")

    def open(self):
        pass

    def setTimeout(self, timeout):
        self.ser.ReadTimeout = timeout

    def flushInput(self):
        pass

    def flushOutput(self):
        pass

    def read(self, size=1):
        retval = ''
        b = self.ser.ReadByte()
        while b != 255:
            retval += str(b)
            b = self.ser.ReadByte() # or ReadChar
        return retval

    def readline(self):
        return self.ser.ReadLine()

    def write(self, s):
        return self.ser.Write(s) # FIXME: what is the newline? \r

    def close(self):
        self.ser.Close()

