#!/usr/bin/env python

# Myro upgrade.py program
# For upgrading Fluke and Scribbler over Bluetooth
# (c) 2011, Institute for Personal Robots in Education
# Douglas Blank, <dblank@cs.brynmawr.edu>
# Keith O'Hara <kohara@bard.edu>
# Jay Summet <summetj@gatech.edu> - GUI sugar coating...

VERSION = "1.0.1"

# Global variable robot, to set SerialPort()
robot = None
pythonVer = "?"
rbString = None
ptString = None
statusText = None

# Now, let's import things
import urllib
import tempfile
import os, sys, time
try:
    import serial
except:
    print("WARNING: pyserial not loaded: can't upgrade!")
    sys.exit()
try:
    input = raw_input # Python 2.x
except:
    pass # Python 3 and better, input is defined

try:
    from tkinter import *
    pythonver = "3"
except:
    try:
        from Tkinter import *
        pythonver = "2"
    except:
        pythonver = "?"

# intelhex.py
#!/usr/bin/python

# Copyright (c) 2005-2007, Alexander Belchenko
# All rights reserved.
#
# Redistribution and use in source and binary forms,
# with or without modification, are permitted provided
# that the following conditions are met:
#
# * Redistributions of source code must retain
#   the above copyright notice, this list of conditions
#   and the following disclaimer.
# * Redistributions in binary form must reproduce
#   the above copyright notice, this list of conditions
#   and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the <Alexander Belchenko>
#   nor the names of its contributors may be used to endorse
#   or promote products derived from this software
#   without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
# OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

'''Intel HEX file format reader and converter.

This script also may be used as hex2bin convertor utility.

@author     Alexander Belchenko (bialix AT ukr net)
@version    0.9.0
@date       2007/06/16
'''

__docformat__ = "javadoc"

from array import array
from binascii import hexlify, unhexlify

class IntelHex:
    ''' Intel HEX file reader. '''

    def __init__(self, source=None):
        ''' Constructor.

        @param  source      source for initialization
                            (file name of HEX file or file object)
        '''
        #public members
        self.padding = 0x0FF
        # Start Address
        self.start_addr = None

        # private members
        self._buf = {}
        self._offset = 0

        if source is not None:
            if isinstance(source, basestring) or hasattr(source, "read"):
                # load hex file
                self.loadhex(source)
            else:
                raise ValueError("source: bad initializer type")

    def _decode_record(self, s, line=0):
        '''Decode one record of HEX file.

        @param  s       line with HEX record.
        @param  line    line number (for error messages).

        @raise  EndOfFile   if EOF record encountered.
        '''
        s = s.rstrip('\r\n')
        if not s:
            return          # empty line

        if s[0] == ':':
            try:
                bin = array('B', unhexlify(s[1:]))
            except TypeError:
                # this might be raised by unhexlify when odd hexascii digits
                raise HexRecordError(line=line)
            length = len(bin)
            if length < 5:
                raise HexRecordError(line=line)
        else:
            raise HexRecordError(line=line)

        record_length = bin[0]
        if length != (5 + record_length):
            raise RecordLengthError(line=line)

        addr = bin[1]*256 + bin[2]

        record_type = bin[3]
        if not (0 <= record_type <= 5):
            raise RecordTypeError(line=line)

        crc = sum(bin)
        crc &= 0x0FF
        if crc != 0:
            raise RecordChecksumError(line=line)

        if record_type == 0:
            # data record
            addr += self._offset
            for i in xrange(4, 4+record_length):
                if not self._buf.get(addr, None) is None:
                    raise AddressOverlapError(address=addr, line=line)
                self._buf[addr] = bin[i]
                addr += 1   # FIXME: addr should be wrapped 
                            # BUT after 02 record (at 64K boundary)
                            # and after 04 record (at 4G boundary)

        elif record_type == 1:
            # end of file record
            if record_length != 0:
                raise EOFRecordError(line=line)
            raise _EndOfFile

        elif record_type == 2:
            # Extended 8086 Segment Record
            if record_length != 2 or addr != 0:
                raise ExtendedSegmentAddressRecordError(line=line)
            self._offset = (bin[4]*256 + bin[5]) * 16

        elif record_type == 4:
            # Extended Linear Address Record
            if record_length != 2 or addr != 0:
                raise ExtendedLinearAddressRecordError(line=line)
            self._offset = (bin[4]*256 + bin[5]) * 65536

        elif record_type == 3:
            # Start Segment Address Record
            if record_length != 4 or addr != 0:
                raise StartSegmentAddressRecordError(line=line)
            if self.start_addr:
                raise DuplicateStartAddressRecordError(line=line)
            self.start_addr = {'CS': bin[4]*256 + bin[5],
                               'IP': bin[6]*256 + bin[7],
                              }

        elif record_type == 5:
            # Start Linear Address Record
            if record_length != 4 or addr != 0:
                raise StartLinearAddressRecordError(line=line)
            if self.start_addr:
                raise DuplicateStartAddressRecordError(line=line)
            self.start_addr = {'EIP': (bin[4]*16777216 +
                                       bin[5]*65536 +
                                       bin[6]*256 +
                                       bin[7]),
                              }

    def loadhex(self, fobj):
        """Load hex file into internal buffer.

        @param  fobj        file name or file-like object
        """
        if not hasattr(fobj, "read"):
            fobj = file(fobj, "r")
            fclose = fobj.close
        else:
            fclose = None

        self._offset = 0
        line = 0

        try:
            decode = self._decode_record
            try:
                for s in fobj:
                    line += 1
                    decode(s, line)
            except _EndOfFile:
                pass
        finally:
            if fclose:
                fclose()

    def loadbin(self, fobj, offset=0):
        """Load bin file into internal buffer.

        @param  fobj        file name or file-like object
        @param  offset      starting address offset
        """
        fread = getattr(fobj, "read", None)
        if fread is None:
            f = file(fobj, "rb")
            fread = f.read
            fclose = f.close
        else:
            fclose = None

        try:
            for b in fread():
                self._buf[offset] = ord(b)
                offset += 1
        finally:
            if fclose:
                fclose()
        

    def loadfile(self, fobj, format):
        """Load data file into internal buffer.

        @param  fobj        file name or file-like object
        @param  format      file format ("hex" or "bin")
        """
        if format == "hex":
            self.loadhex(fobj)
        elif format == "bin":
            self.loadbin(fobj)
        else:
            raise ValueError('format should be either "hex" or "bin"')

    def _get_start_end(self, start=None, end=None):
        """Return default values for start and end if they are None
        """
        if start is None:
            start = min(self._buf.keys())
        if end is None:
            end = max(self._buf.keys())
        if start > end:
            start, end = end, start
        return start, end

    def tobinarray(self, start=None, end=None, pad=None):
        ''' Convert to binary form.
        @param  start   start address of output bytes.
        @param  end     end address of output bytes.
        @param  pad     fill empty spaces with this value
                        (if None used self.padding).
        @return         array of unsigned char data.
        '''
        if pad is None:
            pad = self.padding

        bin = array('B')

        if self._buf == {}:
            return bin

        start, end = self._get_start_end(start, end)

        for i in xrange(start, end+1):
            bin.append(self._buf.get(i, pad))

        return bin

    def tobinstr(self, start=None, end=None, pad=0xFF):
        ''' Convert to binary form.
        @param  start   start address of output bytes.
        @param  end     end address of output bytes.
        @param  pad     fill empty spaces with this value
                        (if None used self.padding).
        @return         string of binary data.
        '''
        return self.tobinarray(start, end, pad).tostring()

    def tobinfile(self, fobj, start=None, end=None, pad=0xFF):
        '''Convert to binary and write to file.

        @param  fobj    file name or file object for writing output bytes.
        @param  start   start address of output bytes.
        @param  end     end address of output bytes.
        @param  pad     fill empty spaces with this value
                        (if None used self.padding).
        '''
        if not hasattr(fobj, "write"):
            fobj = file(fobj, "wb")
            close_fd = True
        else:
            close_fd = False

        fobj.write(self.tobinstr(start, end, pad))

        if close_fd:
            fobj.close()

    def minaddr(self):
        ''' Get minimal address of HEX content. '''
        aa = self._buf.keys()
        if aa == []:
            return 0
        else:
            return min(aa)

    def maxaddr(self):
        ''' Get maximal address of HEX content. '''
        aa = self._buf.keys()
        if aa == []:
            return 0
        else:
            return max(aa)

    def __getitem__(self, addr):
        ''' Get byte from address.
        @param  addr    address of byte.
        @return         byte if address exists in HEX file, or self.padding
                        if no data found.
        '''
        return self._buf.get(addr, self.padding)

    def __setitem__(self, addr, byte):
        self._buf[addr] = byte

    def writefile(self, f, write_start_addr=True):
        """Write data to file f in HEX format.

        @param  f                   filename or file-like object for writing
        @param  write_start_addr    enable or disable writing start address
                                    record to file (enabled by default).
                                    If there is no start address nothing
                                    will be written.

        @return True    if successful.
        """
        fwrite = getattr(f, "write", None)
        if fwrite:
            fobj = f
            fclose = None
        else:
            fobj = file(f, 'w')
            fwrite = fobj.write
            fclose = fobj.close

        # start address record if any
        if self.start_addr and write_start_addr:
            keys = self.start_addr.keys()
            keys.sort()
            bin = array('B', '\0'*9)
            if keys == ['CS','IP']:
                # Start Segment Address Record
                bin[0] = 4      # reclen
                bin[1] = 0      # offset msb
                bin[2] = 0      # offset lsb
                bin[3] = 3      # rectyp
                cs = self.start_addr['CS']
                bin[4] = (cs >> 8) & 0x0FF
                bin[5] = cs & 0x0FF
                ip = self.start_addr['IP']
                bin[6] = (ip >> 8) & 0x0FF
                bin[7] = ip & 0x0FF
                bin[8] = (-sum(bin)) & 0x0FF    # chksum
                fwrite(':')
                fwrite(hexlify(bin.tostring()).upper())
                fwrite('\n')
            elif keys == ['EIP']:
                # Start Linear Address Record
                bin[0] = 4      # reclen
                bin[1] = 0      # offset msb
                bin[2] = 0      # offset lsb
                bin[3] = 5      # rectyp
                eip = self.start_addr['EIP']
                bin[4] = (eip >> 24) & 0x0FF
                bin[5] = (eip >> 16) & 0x0FF
                bin[6] = (eip >> 8) & 0x0FF
                bin[7] = eip & 0x0FF
                bin[8] = (-sum(bin)) & 0x0FF    # chksum
                fwrite(':')
                fwrite(hexlify(bin.tostring()).upper())
                fwrite('\n')
            else:
                self.Error = ('Invalid start address value: %r'
                              % self.start_addr)
                return False

        # data
        minaddr = IntelHex.minaddr(self)
        maxaddr = IntelHex.maxaddr(self)
        if maxaddr > 65535:
            offset = int(minaddr/65536)*65536
        else:
            offset = None

        while True:
            if offset != None:
                # emit 32-bit offset record
                high_ofs = int(offset / 65536)
                offset_record = ":02000004%04X" % high_ofs
                bytes = divmod(high_ofs, 256)
                csum = 2 + 4 + bytes[0] + bytes[1]
                csum = (-csum) & 0x0FF
                offset_record += "%02X\n" % csum 

                ofs = offset
                if (ofs + 65536) > maxaddr:
                    rng = xrange(maxaddr - ofs + 1)
                else:
                    rng = xrange(65536)
            else:
                ofs = 0
                offset_record = ''
                rng = xrange(maxaddr + 1)

            csum = 0
            k = 0
            record = ""
            for addr in rng:
                byte = self._buf.get(ofs+addr, None)
                if byte != None:
                    if k == 0:
                        # optionally offset record
                        fobj.write(offset_record)
                        offset_record = ''
                        # start data record
                        record += "%04X00" % addr
                        bytes = divmod(addr, 256)
                        csum = bytes[0] + bytes[1]

                    k += 1
                    # continue data in record
                    record += "%02X" % byte
                    csum += byte

                    # check for length of record
                    if k < 16:
                        continue

                if k != 0:
                    # close record
                    csum += k
                    csum = (-csum) & 0x0FF
                    record += "%02X" % csum
                    fobj.write(":%02X%s\n" % (k, record))
                    # cleanup
                    csum = 0
                    k = 0
                    record = ""
            else:
                if k != 0:
                    # close record
                    csum += k
                    csum = (-csum) & 0x0FF
                    record += "%02X" % csum
                    fobj.write(":%02X%s\n" % (k, record))

            # advance offset
            if offset is None:
                break

            offset += 65536
            if offset > maxaddr:
                break

        # end-of-file record
        fobj.write(":00000001FF\n")
        if fclose:
            fclose()

        return True
#/IntelHex

class IntelHex16bit(IntelHex):
    """Access to data as 16-bit words."""

    def __init__(self, source):
        """Construct class from HEX file
        or from instance of ordinary IntelHex class.

        @param  source  file name of HEX file or file object
                        or instance of ordinary IntelHex class
        """
        if isinstance(source, IntelHex):
            # from ihex8
            self.padding = source.padding
            # private members
            self._buf = source._buf
            self._offset = source._offset
        else:
            IntelHex.__init__(self, source)

        if self.padding == 0x0FF:
            self.padding = 0x0FFFF

    def __getitem__(self, addr16):
        """Get 16-bit word from address.
        Raise error if found only one byte from pair.

        @param  addr16  address of word (addr8 = 2 * addr16).
        @return         word if bytes exists in HEX file, or self.padding
                        if no data found.
        """
        addr1 = addr16 * 2
        addr2 = addr1 + 1
        byte1 = self._buf.get(addr1, None)
        byte2 = self._buf.get(addr2, None)

        if byte1 != None and byte2 != None:
            return byte1 | (byte2 << 8)     # low endian

        if byte1 == None and byte2 == None:
            return self.padding

        raise BadAccess16bit(address=addr16)

    def __setitem__(self, addr16, word):
        addr_byte = addr16 * 2
        bytes = divmod(word, 256)
        self._buf[addr_byte] = bytes[1]
        self._buf[addr_byte+1] = bytes[0]

    def minaddr(self):
        '''Get minimal address of HEX content in 16-bit mode.'''
        aa = self._buf.keys()
        if aa == []:
            return 0
        else:
            return int(min(aa)/2)

    def maxaddr(self):
        '''Get maximal address of HEX content in 16-bit mode.'''
        aa = self._buf.keys()
        if aa == []:
            return 0
        else:
            return int(max(aa)/2)

#/class IntelHex16bit


def hex2bin(fin, fout, start=None, end=None, size=None, pad=0xFF):
    """Hex-to-Bin convertor engine.
    @return     0   if all OK

    @param  fin     input hex file (filename or file-like object)
    @param  fout    output bin file (filename or file-like object)
    @param  start   start of address range (optional)
    @param  end     end of address range (optional)
    @param  size    size of resulting file (in bytes) (optional)
    @param  pad     padding byte (optional)
    """
    try:
        h = IntelHex(fin)
    except HexReaderError as e:
        printStatus("Error: bad HEX file: %s" % str(e))
        return 1

    # start, end, size
    if size != None and size != 0:
        if end == None:
            if start == None:
                start = h.minaddr()
            end = start + size - 1
        else:
            if (end+1) >= size:
                start = end + 1 - size
            else:
                start = 0

    try:
        h.tobinfile(fout, start, end, pad)
    except IOError:
        printStatus("Could not write to file: %s" % fout)
        return 1

    return 0
#/def hex2bin


##
# IntelHex Errors Hierarchy:
#
#  IntelHexError    - basic error
#       HexReaderError  - general hex reader error
#           AddressOverlapError - data for the same address overlap
#           HexRecordError      - hex record decoder base error
#               RecordLengthError    - record has invalid length
#               RecordTypeError      - record has invalid type (RECTYP)
#               RecordChecksumError  - record checksum mismatch
#               EOFRecordError              - invalid EOF record (type 01)
#               ExtendedAddressRecordError  - extended address record base error
#                   ExtendedSegmentAddressRecordError   - invalid extended segment address record (type 02)
#                   ExtendedLinearAddressRecordError    - invalid extended linear address record (type 04)
#               StartAddressRecordError     - start address record base error
#                   StartSegmentAddressRecordError      - invalid start segment address record (type 03)
#                   StartLinearAddressRecordError       - invalid start linear address record (type 05)
#                   DuplicateStartAddressRecordError    - start address record appears twice
#       _EndOfFile  - it's not real error, used internally by hex reader as signal that EOF record found
#       BadAccess16bit - not enough data to read 16 bit value

class IntelHexError(Exception):
    '''Base Exception class for IntelHex module'''

    _fmt = 'IntelHex base error'   #: format string

    def __init__(self, message=None, **kw):
        self.message = message
        for key, value in kw.items():
            setattr(self, key, value)

    def __str__(self):
        if self.message:
            return self.message
        try:
            return self._fmt % self.__dict__
        except (NameError, ValueError, KeyError) as e:
            return 'Unprintable exception %s: %s' \
                % (self.__class__.__name__, str(e))

class _EndOfFile(IntelHexError):
    _fmt = 'EOF record reached -- signal to stop read file'

class HexReaderError(IntelHexError):
    _fmt = 'Hex reader base error'

class AddressOverlapError(HexReaderError):
    _fmt = 'Hex file has data overlap at address 0x%(address)X on line %(line)d'

# class NotAHexFileError was removed in trunk.revno.54 because it's not used


class HexRecordError(HexReaderError):
    _fmt = 'Hex file contains invalid record at line %(line)d'


class RecordLengthError(HexRecordError):
    _fmt = 'Record at line %(line)d has invalid length'

class RecordTypeError(HexRecordError):
    _fmt = 'Record at line %(line)d has invalid record type'

class RecordChecksumError(HexRecordError):
    _fmt = 'Record at line %(line)d has invalid checksum'

class EOFRecordError(HexRecordError):
    _fmt = 'File has invalid End-of-File record'


class ExtendedAddressRecordError(HexRecordError):
    _fmt = 'Base class for extended address exceptions'

class ExtendedSegmentAddressRecordError(ExtendedAddressRecordError):
    _fmt = 'Invalid Extended Segment Address Record at line %(line)d'

class ExtendedLinearAddressRecordError(ExtendedAddressRecordError):
    _fmt = 'Invalid Extended Linear Address Record at line %(line)d'


class StartAddressRecordError(HexRecordError):
    _fmt = 'Base class for start address exceptions'

class StartSegmentAddressRecordError(StartAddressRecordError):
    _fmt = 'Invalid Start Segment Address Record at line %(line)d'

class StartLinearAddressRecordError(StartAddressRecordError):
    _fmt = 'Invalid Start Linear Address Record at line %(line)d'

class DuplicateStartAddressRecordError(StartAddressRecordError):
    _fmt = 'Start Address Record appears twice at line %(line)d'


class BadAccess16bit(IntelHexError):
    _fmt = 'Bad access at 0x%(address)X: not enough data to read 16 bit value'

# end intelhex.py

class SerialRobot:
    def __init__(self, serialport=None, baudrate=38400):
        self.robotinfo = {}
        if serialport == None:
            serialport = input("Port: ")
        # Deal with requirement that Windows "COM#" names where # >= 9 needs to
        # be in the format "\\.\COM#"
        if type(serialport) == str and serialport.lower().startswith("com"):
            portnum = int(serialport[3:])
            if portnum >= 10:
                serialport = r'\\.\COM%d' % (portnum)
        self.serialPort = serialport
        self.baudRate = baudrate
        try:
            robot.ser.close()
        except KeyboardInterrupt:
            raise
        except:
            pass
        self.ser = serial.Serial(self.serialPort, timeout = 2) 
    def getInfo(self): return {"robot":"Serial", "mode": "serial"}
    def restart(self):
        printStatus("Please start myro to connect onto robot")

def upgrade_scribbler(url=None, port=None, scrib_version=1):
    """
    Takes a url or filename and upgrades Myro.
    """
    global robot
    if robot == None:
        # force upgrade
        printStatus("Connecting to Scribbler for initial firmware installation...")
        try:
           robot = SerialRobot(port)
        except:
           printStatus("Error! That serial port not found!" )
           raise 

    s = robot.ser

    info = get_info_timeout(s)
    
    scribbler_ver = [0, 0, 0]
    if "robot-version" in info.keys():            
        scribbler_ver = info["robot-version"].split(".")
    elif "api" in info.keys():            
        scribbler_ver = info["api"].split(".")

    if "robot" in info.keys():
        robot_version = info["robot"]
        if robot_version == "Scribbler2":
            scrib_version = 2
    else:
        robot_type = get_robot_type(s) 
        printStatus("using robot: %s" % robot_type)
        if robot_type == "SCRIBBLER-2\n":
            scrib_version = 2

            
    if url == None:
        url = "http://myro.roboteducation.org/upgrade/scribbler/"
        startswith = "scribbler-upgrade-"
        endswidth = ".bytecode"
        startpos = 18
        if scrib_version == 2:
            url = "http://myro.roboteducation.org/upgrade/scribbler2/"
            startswith = "scribbler2-upgrade-"
            endswidth = ".binary"
            startpos = 19

    install_count = 0
    if not url.startswith("http://"):
        printStatus("Looking for Scribbler %s upgrades in file %s..." % (scrib_version, url))
        if scrib_version == 2:
            f = open(url, 'rb')
        else:
            f = open(url, 'r')
            
        install_count += load_scribbler(s, f, True, scrib_version) # which is a filename
    else:        
        printStatus("Looking for Scribbler upgrades at %s..." % url)

        info = get_info_timeout(s)

        scribbler_ver = [0, 0, 0]
        if "robot-version" in info.keys():            
            scribbler_ver = info["robot-version"].split(".")
        elif "api" in info.keys():            
            scribbler_ver = info["api"].split(".")

        # go to site, check for latest greater than our version
        try:
            infp = urllib.urlopen(url)
        except:
            printStatus("ERROR: There was an error connecting to the web to download updates. Please check your internet connection. For example, see if you can access %s using a web browser." % url)
            return
        
        printStatus("Opened url...")
        contents = infp.read()
        lines = contents.split("\n")
        infp.close()
        consider = {}
        # find the biggest matching one:
        for filename in lines:
            filename = filename.strip()
            if filename != "" and filename[0] != '#':
                printStatus("Considering %s ..." % filename)
                if filename.startswith(startswith):
                    end = filename.index(endswidth)
                    patch_ver = filename[startpos:end].split(".")
                    try:
                        scribbler_ver = map(int, scribbler_ver)
                    except:
                        #scribbler_ver has letters in it (and so is really old)
                        scribbler_ver = [0, 0, 0]
                    if map(int, patch_ver) > scribbler_ver:
                        # consider it:
                        consider[tuple(map(int, patch_ver))] = url + filename

        consider_keys = consider.keys()
        consider_keys.sort()
        if len(consider_keys) > 0:
            full_url = consider[consider_keys[-1]]
            printStatus("Loading %s" % full_url)
            f = urllib.urlopen(full_url)
            install_count += load_scribbler(s, f, True, scrib_version)
    if install_count > 0:
        printStatus("Done upgrading!")
    else:
        printStatus("Nothing to upgrade on the Scribbler; it's up-to-date.")
    return install_count

def manual_flush(ser):
    old = ser.timeout
    ser.setTimeout(1)
    l = 'a'
    count = 0;
    while (len(l) != 0 and count < 50000):
        l = ser.read(1)
        count += len(l)
    ser.setTimeout(old)

    
def get_info_timeout(s):
    GET_INFO=80  
    oldtimeout = s.timeout
    s.setTimeout(4)
    manual_flush(s)
    s.write(chr(GET_INFO) + (' ' * 8))
    retval = s.readline()
    #print "Got", retval   
    s.write(chr(GET_INFO) + (' ' * 8))
    retval = s.readline()
    # remove echoes
    #print "Got", retval
    
    if retval == None or len(retval) == 0:
        return {}
    
    if retval[0] == 'P' or retval[0] == 'p':
        retval = retval[1:]

    if retval[0] == 'P' or retval[0] == 'p':
        retval = retval[1:]

    s.setTimeout(oldtimeout)
    
    retDict = {}
    for pair in retval.split(","):
        if ":" in pair:
            it, value = pair.split(":")
            retDict[it.lower().strip()] = value.strip()
    return retDict
            
def load_scribbler(s, f, force=False, scrib_version = 1):
    
    # check to see if we need to send magicKey when upgrading
    if (robot and 
        "dongle" in dir(robot) and 
        robot.dongle):
        info = robot.dongle
    else:
        info = get_info_timeout(s)
        if "fluke" in info:
            info = info["fluke"]
        else:
            info = "0.0.0"

    printStatus(info)

    sendMagicKey = False

    version = map(int, info.split("."))
    printStatus("Version of fluke %s" % version)
    
    if version > [2, 5, 0] or force:
        sendMagicKey = True

    if sendMagicKey:
        printStatus("Sending magic key")
    else:
        printStatus("Older firmware version, Not sending magic key")

    bytes=[]
    if scrib_version == 2:
        bytes = f.read()
    else:
        for t in f:        
            t = t.strip()
            if (len(t) > 0):               
                nv = int(t)
                bytes.append(nv)
    printStatus("Program size (bytes) = %d; scribbler version = %d" % (len(bytes), scrib_version))
    f.close()
    printStatus("Storing program in memory...")
    if scrib_version == 2:        
        set_scribbler2_memory_batch(s, bytes)
    else:
        for i in range(0, len(bytes)):
            set_scribbler_memory(s, i, bytes[i])
            
    printStatus("Programming scribbler %d..." % scrib_version)
    if sendMagicKey:
        printStatus("sending magic key")
        if scrib_version == 2:
            set_scribbler2_start_program(s, len(bytes))
        else:
            set_scribbler_start_program(s, len(bytes))
    else:
        printStatus("older version, not sending magic key")
        set_scribbler_start_program_old(s, len(bytes))

    time.sleep(30)

    printStatus("Wait for the robot to reboot!")
    robot.restart()

    return 1

GET_SCRIB_PROGRAM=91  # with offset, returns the scribbler program buffer
SET_SCRIB_PROGRAM=122   # set scribbler program memory byte
SET_START_PROGRAM=123   # initiate scribbler programming process
SET_START_PROGRAM2=153   # initiate scribbler 2 programming process
SET_SCRIB2_RESET=154   # initiate scribbler 2 programming process
SET_SCRIB_BATCH=155   # initiate scribbler 2 programming process
GET_ROBOT_ID = 156    # find out which type of robot - scribbler 1 or 2
UPDATE_FIRMWARE = 40	# Updates the firmware of the robot 

def get_robot_type(ser):
    ser.write(chr(GET_ROBOT_ID))
    return ser.readline()

def set_scribbler_memory(ser, offset, byte):
    ser.write(chr(SET_SCRIB_PROGRAM))
    write_2byte(ser, offset)
    ser.write(chr(byte))

def set_scribbler2_memory(ser, offset, byte):
    ser.write(chr(SET_SCRIB_PROGRAM))
    write_2byte(ser, offset)
    ser.write(byte)

def set_scribbler2_memory_batch(ser, bytes):
    ser.write(chr(SET_SCRIB_BATCH))
    write_2byte(ser, len(bytes))
    for byte in bytes:
        ser.write(byte)

def get_scribbler_memory(ser, offset):
    ser.write(chr(GET_SCRIB_PROGRAM))
    write_2byte(ser, offset)
    v = ord(ser.read(1))
    return v
    
def set_scribbler_start_program(ser, size):
    ser.write(chr(SET_START_PROGRAM))
    # magic code to ensure we don't enter scribbler program by accident
    ser.write(chr(0x01))
    ser.write(chr(0x23))
    write_2byte(ser, size)

def set_scribbler_start_program_old(ser, size):
    ser.write(chr(SET_START_PROGRAM))
    write_2byte(ser, size)

    
def set_scribbler2_start_program(ser, size):
    ser.write(chr(SET_START_PROGRAM2))
    # magic code to ensure we don't enter scribbler program by accident
    ser.write(chr(0x01))
    ser.write(chr(0x23))
    write_2byte(ser, size)

def write_2byte(ser, value):
    ser.write(chr((value >> 8) & 0xFF))
    ser.write(chr(value & 0xFF))

def uf_sendPage(s,page,binarray):
    segment = 0
    while segment < int(264/132) :
        i = 0
        sum = 0
        for i in range (0,132) :
            s.write(chr(binarray[page*264 + segment*132 + i]))
            sum = sum + binarray[page*264 + segment*132 + i]
        s.write(chr(sum % 256))
        retval = ord(s.read(1))
        #print "Sum: %d Return: %d" % (sum % 256,retval)
        if retval == 42 :
            segment = segment + 1

def uf_recvPage(s,page,binarray):
    segment = 0
    while segment < int(264/132) :
        i = 0
        sum = 0
        chksum = 0
        for i in range (0,132) :
            recv = ord(s.read(1))
            binarray[page*264 + segment*132 + i] = recv 
            sum = sum + recv 
        chksum = ord(s.read(1))
        #print "My sum: %d Recd chksum: %d" % (sum % 256,chksum)
        if chksum == sum % 256 :
            segment = segment + 1
            s.write(chr(42))
        else:
            s.write(chr(1))

def uf_saveEEPROMdump(s,eepromdump):
    for i in range (0,512) :
        printStatus("%d %%" % ((i*100)/512))
        sys.stdout.flush()
        uf_recvPage(s,i,eepromdump)
    printStatus("")

def uf_restoreEEPROMdump(s,eepromdump):
    for i in range (0,512) :
        printStatus("%d %%" % ((i*100)/512))
        sys.stdout.flush()
        uf_sendPage(s,i,eepromdump)
    printStatus("")

def uf_storeinEEPROM(s, arlen, binarray):
    segs = int(arlen / 264)
    if segs*264 < arlen :
        segs = segs + 1
        for i in range (arlen,segs*264):
            binarray.append(0)
    #print "Writing %d segments" % segs
    write_2byte(s,segs)
    for i in range (0,segs) :
        printStatus("%d %%" % ((i*100)/segs))
        sys.stdout.flush()
        uf_sendPage(s,i,binarray)
    printStatus("")

def check_sum(binarray, arlen):
    for i in range(20,24):
        binarray[i] = 0
    sum=0
    for i in range(0,8):
        temp_int = 0
        temp_int = binarray[i*4 + 0] | binarray[i*4 + 1] << 8 | binarray[i*4 + 2] << 16 | binarray[i*4 + 3] << 24
        sum = sum + temp_int
    sum = -sum
    binarray[20] = sum & 0x000000ff
    binarray[21] = (sum >> 8) & 0x000000ff
    binarray[22] = (sum >> 16) & 0x000000ff
    binarray[23] = (sum >> 24) & 0x000000ff 
    sum=0
    for i in range(0,8):
        temp_int = 0
        temp_int = binarray[i*4 + 0] | binarray[i*4 + 1] << 8 | binarray[i*4 + 2] << 16 | binarray[i*4 + 3] << 24
        sum = sum + temp_int
    for i in range(0,arlen):
        if i % 8192 == 0 :
            sum = 0
        sum = sum + binarray[i]
    return sum

def url_retrieve(url, tmp_dir = None):
    """ Retrieves the contents of a url. """
    if tmp_dir == None:
        if "TMP" in os.environ:
            tmp_dir = os.environ["TMP"]
        elif "TEMP" in os.environ:
            tmp_dir = os.environ["TEMP"]
        else:
            tmp_dir = tempfile.gettempdir()
    # get url into tmp_file
    path, file = url.rsplit("/", 1)
    tmp_file = tmp_dir + os.sep + file
    infp = urllib.urlopen(url)
    contents = infp.read()
    infp.close()
    outfp = open(tmp_file, "wb")
    outfp.write(contents)
    outfp.close()
    return tmp_file

def upgrade_fluke(url=None, port=None):
    global robot
    #define UF_SUCCESS 42
    #define UF_ERROR 1
    #define UF_SEGMENT_SIZE 132

    if robot == None:
        printStatus("Connecting to Fluke for firmware installation...")
        try:
           robot = SerialRobot(port)
        except:
           printStatus("Error! That serial port not found!" )
           raise 

        s = robot.ser
        info = get_info_timeout(s)
        if "fluke" in info:
            info = info["fluke"]
        else:
            info = "0.0.0"            
    elif robot.dongle:
        info = robot.dongle
        s = robot.ser

    printStatus(info)
    version = map(int, info.split("."))
    printStatus("Version of fluke %s" % version)
    
    if version <= [2, 4, 0]:
        printStatus("(If you just upgraded Myro, please restart Python.)")
        printStatus("Sorry, I can't upgrade the Fluke over Bluetooth.")
        printStatus("It must be upgraded manually over the serial port using lpc21isp.")
        printStatus("Please see http://wiki.roboteducation.org/IPRE_Fluke_Setup")
        return

    if url == None:
        url = "http://myro.roboteducation.org/upgrade/fluke/"
    install_count = 0
    filename = None
    if url.startswith("http://"):
        #fluke_ver = info["fluke"].split(".")
        printStatus("Looking for Fluke upgrade at %s..." % url)
        # go to site, check for latest greater than our version
        #infp = urllib.urlopen(url)
        try:
            infp = urllib.urlopen(url)
        except:
            printStatus("ERROR: There was an error connecting to the web to download updates. Please check your internet connection. For example, see if you can access %s using a web browser." % url)
            return

        contents = infp.read()
        lines = contents.split("\n")
        infp.close()
        for file in lines:
            file = file.strip()
            if file != "" and file[0] != '#':
                printStatus("Considering %s ..." % file)
                if file.startswith("/fluke-upgrade-"):
                    end = file.index(".hex")
                    patch_ver = file[15:end].split(".")
                    printStatus("%s %s" % (patch_ver, version))
                    if map(int, patch_ver) > map(int, version):
                        # download it
                        printStatus("   Downloading...")
                        filename = url_retrieve(url + file)
                        break
    else:
        filename = url

    if filename == None:
        printStatus("Nothing found to upgrade!")
        return
    #info = myro.globvars.robot.getInfo()
    sendMagicKey = True

    if version <= [2, 5, 0]:
        sendMagicKey = False
        printStatus("Older firmware version, Not sending magic key")
    else:
        printStatus("Sending magic key")
        
    ih = IntelHex(filename)
    binarray = ih.tobinarray()
    arlen = len(binarray)    
    printStatus("%d bytes of firmware." % arlen)
    printStatus("checksumming interrupt vectors")
    sum = check_sum(binarray, arlen)
    #declare a finite sized array to hold eeprom dump. 
    #Dynamic appending of lists always comes with a performance hit
    eepromdump = [0] * 135168
    s.flushOutput()
    s.flushInput()
       
    #print "Getting old EEPROM"
    #s.write(chr(SAVE_EEPROM))
    #uf_saveEEPROMdump()
    printStatus("Sending firmware")
    s.write(chr(UPDATE_FIRMWARE))
    if sendMagicKey:        
        # magic code to ensure we don't enter program by accident    
        s.write(chr(0x01))
        s.write(chr(0x23))
        
    uf_storeinEEPROM(s, arlen, binarray)
    printStatus("Waiting for reboot...")
    time.sleep(2)
    printStatus("Done upgrading! Please turn your robot off and then back on." )
    s.close()

def printStatus(string):
    """
    """
    if statusText == None:
        print(string)
    else:
        print(string)
        statusText.insert(END, str(string) + "\n" )
        statusText.yview(MOVETO, 1.0)
        statusText.update()	#Make sure updates happen even on long running
				#Event handlers.

def graphicalMain():
    global pythonVer
    global rbString
    global ptString
    global statusText
    #print("Python version is:", pythonVer)
    mainWin = Tk()
    mainWin.title("IPRE Stand Alone Scribbler / Fluke upgrade tool " +str(VERSION) )
    rbString = StringVar()
    ptString = StringVar()

    #Set up the text window & scrollbar frame for status messages.
    f = Frame(mainWin)
    scrollbar = Scrollbar(f)
    statusText = Text(f, yscrollcommand=scrollbar.set, height=10 )
    statusText.insert(END, "Welcome to the Standalone Scribbler / Fluke upgrader tool, version %s!\n" % VERSION)
    scrollbar.config(command=statusText.yview)
    statusText.focus_set()
    statusText.grid(column=0,row=0,) 
    scrollbar.grid(column=1,row=0, sticky=N+S)
    f.grid(column=0, row=4,columnspan=3)



    Radiobutton(mainWin, variable=rbString, 
                value="fluke", text="Upgrade Fluke").grid(row=0,column=0)
    Radiobutton(mainWin, variable=rbString, 
                value="scribbler", text="Upgrade Scribbler").grid(row=0,column=1)
    Label(mainWin, text="Bluetooth COM port:").grid(row=1,column=0)
    Entry(mainWin, textvariable=ptString).grid(row=1,column=1)
    Label(mainWin, text="e.g. COM4 or /dev/tty.scribbler").grid(row=1,column=2)
    rbString.set("scribbler")
    
    Button(mainWin, command=graphicalCallback, 
           text="Upgrade!").grid(row=2,column=0,columnspan=3,sticky=E+W)
    Label(mainWin, text="Watch here for important status messages:").grid(row=3,column=0)
    mainWin.mainloop()
    
def usage():
    printStatus("upgrade.py, version %s, usage:" % VERSION)
    printStatus("   python upgrade.py --gui=VALUE --url=URL --port=PORT WHAT")
    printStatus("")
    printStatus("     VALUE - is True OR False")
    printStatus("     URL - is an internat address to use")
    printStatus("     PORT - is the serial port address to use")
    printStatus("     WHAT - is scribbler OR fluke")
    printStatus("")
    printStatus("Options with examples:")
    printStatus("  --gui=False                      (optional)")
    printStatus("  --port=COM8                      (optional)")
    printStatus("  --url=http://myurl.com/fluke.bin (optional)")
    printStatus("  --help")
    printStatus("")
    printStatus("Full Examples:")
    printStatus("")
    printStatus("   python upgrade.py")
    printStatus("   python upgrade.py fluke")
    printStatus("   python upgrade.py scribbler")
    printStatus("   python upgrade.py --gui=False")
    printStatus("   python upgrade.py --port=COM5 fluke")
    printStatus("   python upgrade.py --port=/dev/rfcomm3 scribbler")
    printStatus("   python upgrade.py --url=http://myurl.com/file.bin fluke")
    printStatus("   python upgrade.py --help")
    printStatus("")

def upgrade(what, url, port):
    if what == "scribbler":
        printStatus("Upgrading scribbler...")
        if port != None:
            printStatus("   with port %s" % port)
        if url != None:
            printStatus("   with url %s" % url)
        upgrade_scribbler(url=url, port=port)
    if what == "fluke":
        printStatus("Upgrading fluke...")
        if port != None:
            printStatus("   with port", port)
        if url != None:
            printStatus("   with url", url)
        upgrade_fluke(url=url, port=port)

def main():
    printStatus("Fluke and Scribbler/Scribbler2 Upgrade Program, version %s" 
                % VERSION)
    printStatus("-" * 75)
    port = None
    url = None
    what = None
    needhelp = False
    gui = True
    for arg in sys.argv:
        if arg == "scribbler":
            what = "scribbler"
        elif arg == "fluke":
            what = "fluke"
        elif arg == "--help":
            needhelp = True
        elif "=" in arg:
            option, value = arg.split("=", 1)
            if option == "--port":
                port = value
            elif option == "--url":
                url = value
            elif option == "--gui":
                gui = eval(value)
            else:
                raise Exception("Invalid option: " + option)
    if needhelp:
        usage()
    elif what in ["scribbler", "fluke"]:
        upgrade(what, url, port)
    else:
        if not gui: # do not use gui
            what = input("Upgrade (fluke or scribbler): ")
            if what in ["scribbler", "fluke"]:
                upgrade(what, url, port)
            else:
                usage()
        else:
            try:
                graphicalMain()
            except: # fallback
                what = input("Upgrade (fluke or scribbler): ")
                if what in ["scribbler", "fluke"]:
                    upgrade(what, url, port)
                else:
                    usage()

def graphicalCallback():
    port = ptString.get() 
    url = None
    if rbString.get() == "fluke":
        upgrade_fluke(url=url, port=port)
    elif rbString.get() == "scribbler":
        upgrade_scribbler(url=url, port=port)
    else:
        usage()

if __name__ == "__main__":
    main()

