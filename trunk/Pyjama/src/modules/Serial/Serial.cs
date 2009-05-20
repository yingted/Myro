using System.IO.Ports;

namespace Serial
{
    public class Serial
    {
        public SerialPort ser = null;

        Serial()
        {
            ser = new SerialPort();
        }

        public string read()
        {
            return read(1);
        }

        public string read(int size)
        {
            string retval = "";
            try
            {
                int b = ser.ReadByte();
                retval = retval + (char)b;
                while (retval.Length < size)
                {
                    b = ser.ReadByte(); // or ReadChar
                    retval = retval + (char)b;
                }
            }
            catch
            {
                //print "timeout"
            }
            return retval;
        }

        public void write(string s)
        {
            ser.Write(s);
        }

        public void close()
        {
            ser.Close();
        }

        public string port
        {
            get { return getPort(); }
            set { setPort(value); }
        }

        public string getPort()
        {
            return ser.PortName;
        }

        public void setPort(string value)
        {
            ser.PortName = value;
        }

        //baudrate = property(getBaudrate, setBaudrate, doc="Baudrate setting")
        //bytesize = property(getByteSize, setByteSize, doc="Byte size setting")
        //parity = property(getParity, setParity, doc="Parity setting")
        //stopbits = property(getStopbits, setStopbits, doc="Stopbits setting")
        //timeout = property(getTimeout, setTimeout, doc="Timeout setting for read()")
        //writeTimeout = property(getWriteTimeout, setWriteTimeout, doc="Timeout setting for write()")

        /*
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

        def getByteSize(self):
            return self.ser.DataBits

        def setByteSize(self, value):
            self.ser.DataBits = value

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
        */

    }
}