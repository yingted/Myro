import zipfile, tarfile, urllib
import os, string, sys, time, tempfile
try:
    import serial
except:
    print "WARNING: pyserial not loaded: can't upgrade robot!"
from myro import __VERSION__ as myro_version
import myro.globvars
# copied below from scribbler.py:
#from myro.robots.scribbler import set_scribbler_start_program, set_scribbler_memory

class RegFile:
    """ Class for treating a regular file like other archives. """
    def __init__(self, filename, mode="rb"):
        path, file = filename.rsplit(os.sep, 1)
        self.filename = file
        self.fp = open(filename, mode)
    def read(self, name):
        if name == self.filename:
            return self.fp.read()
    def close(self):
        self.fp.close()
    def namelist(self):
        return [self.filename]

def import_url(url, tmp_dir = None):
    """ Retrieves and imports file. """
    tmp_file = url_retrieve(url, tmp_dir)
    return import_file(tmp_file)

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

def import_file(filename):
    """
    Given the path to a file/archive, put the contents
    where the MANIFEST says to.
    """
    if not os.path.exists(filename): return 0
    if zipfile.is_zipfile(filename):
        infp = zipfile.ZipFile(filename)
    elif tarfile.is_tarfile(filename):
        infp = tarfile.TarFile(filename)
    else: # regular file
        infp = RegFile(filename)
    name_list =infp.namelist()
    director = {}
    VALUES = {} 
    if "USERNAME" in os.environ:
        VALUES["USER"] = os.environ["USERNAME"] # NameId
    if "HOMEPATH" in os.environ:
        VALUES["HOME"] = 'C:' + os.sep + os.environ["HOMEPATH"]
    if "HOME" in os.environ:
        VALUES["HOME"] = os.environ["HOME"]
    if "USERPROFILE" in os.environ:
        VALUES["HOME"] = os.environ["USERPROFILE"]
    globalspath, f = myro.globvars.__file__.rsplit(os.sep, 1)
    #print "globalspath:", globalspath
    myropath, f = globalspath.rsplit(os.sep, 1)
    #print "myropath:", myropath
    sitepath, f = myropath.rsplit(os.sep, 1)
    #print "sitepath:", sitepath
    myroparts = myropath.split(os.sep)
    pythonpath = myroparts[0] + os.sep + myroparts[1]
    VALUES["DESKTOP"] = VALUES["HOME"] + os.sep + "DESKTOP" 
    VALUES["PYTHONDIR"] = pythonpath
    VALUES["MYRODIR"] = myropath
    VALUES["PYTHONSITEDIR"] = sitepath
    VALUES["PYTHONDIR"] = pythonpath
    install_count = 0
    if "MANIFEST" in name_list:
        manifest = infp.read("MANIFEST")
        lines = manifest.split("\n")
        for line in lines:
            if ":" in line:
                f, dest = map(string.strip, line.strip().split(":"))
                director[f] = dest % VALUES
        for name in name_list:
            if name == "MANIFEST": continue
            contents = infp.read(name)
            print "   writing:", director[name], "..."
            # first write to temp file:
            try:
                outfp = open(director[name], "wb")
            except:
                makePath(director[name])
                outfp = open(director[name], "wb")
            outfp.write(contents)
            outfp.close()
            install_count += 1
    else:
        print "   ERROR: no MANIFEST in Myro upgrade; skipping"
    infp.close()
    return install_count

def makePath(path):
    from os import makedirs
    from os.path import normpath,dirname,exists,abspath
    print "      Checking directory", path
    dpath = normpath(dirname(path))
    if not exists(dpath):
        print "         Making directory", dpath
        makedirs(dpath)
    return normpath(abspath(path))

def upgrade_myro(url=None, version=None):
    """
    Takes a url or filename and upgrades Myro.
    """
    if url == None:
        url = "http://myro.roboteducation.org/upgrade/"
    if version != None:
        version = version.split(".")
    install_count = 0
    if not url.startswith("http://"):
        print "Looking for Myro upgrades in file", url, "..."
        install_count += import_file(url) # which is a filename
    else:        
        print "Looking for Myro upgrades at", url, "..."
        myro_ver = myro_version.split(".")
        # go to site, check for latest greater than our version
        infp = urllib.urlopen(url)
        contents = infp.read()
        lines = contents.split("\n")
        infp.close()
        for filename in lines:
            filename = filename.strip()
            if filename != "" and filename[0] != '#':
                print "Considering", filename, "..."
                if filename.startswith("myro-upgrade-"):
                    end = filename.index(".zip")
                    patch_ver = filename[13:end].split(".")
                    if (version != None): # get specific version
                        if map(int, patch_ver) == map(int, version):
                            print "   Downloading..."
                            install_count += import_url(url + filename)
                    elif map(int, patch_ver) > map(int, myro_ver):
                        # download it
                        print "   Downloading..."
                        install_count += import_url(url + filename)
    if install_count > 0:
        print "Done upgrading! Please exit and restart Python and Myro"
    else:
        print "Nothing to upgrade in Myro; it's up-to-date."
    return install_count

class SerialRobot:
    def __init__(self, serialport=None, baudrate=38400):
        from myro import ask
        self.robotinfo = {}
        if serialport == None:
            serialport = ask("Port", useCache=0)
        # Deal with requirement that Windows "COM#" names where # >= 9 needs to
        # be in the format "\\.\COM#"
        if type(serialport) == str and serialport.lower().startswith("com"):
            portnum = int(serialport[3:])
            if portnum >= 10:
                serialport = r'\\.\COM%d' % (portnum)
        self.serialPort = serialport
        self.baudRate = baudrate
        try:
            myro.globvars.robot.ser.close()
        except KeyboardInterrupt:
            raise
        except:
            pass
        self.ser = serial.Serial(self.serialPort, timeout = 2) 
    def getInfo(self): return {"robot":"Serial", "mode": "serial"}
    def restart(self):
        print "Please run initialize() to connect onto robot"

def upgrade_scribbler(url=None, scrib_version=1):
    """
    Takes a url or filename and upgrades Myro.
    """
    if myro.globvars.robot == None:
        # force upgrade
        print "Connecting to Scribbler for initial firmware installation..."
        myro.globvars.robot = SerialRobot()

    s = myro.globvars.robot.ser

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
        print "using robot: ", robot_type
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
        print "Looking for Scribbler", scrib_version, "upgrades in file", url, "..."
        if scrib_version == 2:
            f = open(url, 'rb')
        else:
            f = open(url, 'r')
            
        install_count += load_scribbler(s, f, True, scrib_version) # which is a filename
    else:        
        print "Looking for Scribbler upgrades at", url, "..."

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
            print "ERROR: There was an error connecting to the web to download updates. Please check your internet connection. For example, see if you can access", url, "using a web browser."
            return
        
        print "Opened url..."
        contents = infp.read()
        lines = contents.split("\n")
        infp.close()
        consider = {}
        # find the biggest matching one:
        for filename in lines:
            filename = filename.strip()
            if filename != "" and filename[0] != '#':
                print "Considering", filename, "..."
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
            print "Loading", full_url
            f = urllib.urlopen(full_url)
            install_count += load_scribbler(s, f, True, scrib_version)
    if install_count > 0:
        print "Done upgrading!"
    else:
        print "Nothing to upgrade on the Scribbler; it's up-to-date."
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
    if (myro.globvars.robot and 
        "dongle" in dir(myro.globvars.robot) and 
        myro.globvars.robot.dongle):
        info = myro.globvars.robot.dongle
    else:
        info = get_info_timeout(s)
        if "fluke" in info:
            info = info["fluke"]
        else:
            info = "0.0.0"

    print info

    sendMagicKey = False

    version = map(int, info.split("."))
    print "Version of fluke", version
    
    if version > [2, 5, 0] or force:
        sendMagicKey = True

    if sendMagicKey:
        print "Sending magic key"
    else:
        print "Older firmware version, Not sending magic key"            

    bytes=[]
    if scrib_version == 2:
        bytes = f.read()
    else:
        for t in f:        
            t = t.strip()
            if (len(t) > 0):               
                nv = int(t)
                bytes.append(nv)
    print "Program size (bytes) = %d; scribbler version = %d" % (len(bytes), scrib_version)
    f.close()
    print "Storing program in memory..."
    if scrib_version == 2:        
        set_scribbler2_memory_batch(s, bytes)
    else:
        for i in range(0, len(bytes)):
            set_scribbler_memory(s, i, bytes[i])
            
    print "Programming scribbler %d..." % scrib_version
    if sendMagicKey:
        print "sending magic key"
        if scrib_version == 2:
            set_scribbler2_start_program(s, len(bytes))
        else:
            set_scribbler_start_program(s, len(bytes))
    else:
        print "older version, not sending magic key"
        set_scribbler_start_program_old(s, len(bytes))

    time.sleep(30)

    print "Wait for the robot to reboot!"
    myro.globvars.robot.restart()

    return 1

def upgrade(what="myro", url = None, version=None):
    if what.lower() == "myro":
        return upgrade_myro(url, version)
    elif what.lower() == "scribbler":
        return upgrade_scribbler(url)
    elif what.lower() == "fluke":
        return upgrade_fluke(url)
    elif what.lower() == "all":
        install_count = 0
        install_count += upgrade_myro(url)
        install_count += upgrade_scribbler(url)
        return install_count

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
    while segment < 264/132 :
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
    while segment < 264/132 :
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
        print '\r' + "%d %%" % ((i*100)/512),
        sys.stdout.flush()
        uf_recvPage(s,i,eepromdump)
    print ""

def uf_restoreEEPROMdump(s,eepromdump):
    for i in range (0,512) :
        print '\r' + "%d %%" % ((i*100)/512),
        sys.stdout.flush()
        uf_sendPage(s,i,eepromdump)
    print ""

def uf_storeinEEPROM(s, arlen, binarray):
    segs = arlen / 264
    if segs*264 < arlen :
        segs = segs + 1
        for i in range (arlen,segs*264):
            binarray.append(0)
    #print "Writing %d segments" % segs
    write_2byte(s,segs)
    for i in range (0,segs) :
        print '\r' + "%d %%" % ((i*100)/segs),
        sys.stdout.flush()
        uf_sendPage(s,i,binarray)
    print ""

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

def upgrade_fluke(url=None):
    #define UF_SUCCESS 42
    #define UF_ERROR 1
    #define UF_SEGMENT_SIZE 132

    if myro.globvars.robot == None:
        print "Connecting to Fluke for firmware installation..."
        myro.globvars.robot = SerialRobot()
        s = myro.globvars.robot.ser
        info = get_info_timeout(s)
        if "fluke" in info:
            info = info["fluke"]
        else:
            info = "0.0.0"            
    elif myro.globvars.robot.dongle:
        info = myro.globvars.robot.dongle
        s = myro.globvars.robot.ser

    print info
    version = map(int, info.split("."))
    print "Version of fluke", version
    
    if version <= [2, 4, 0]:
        print "(If you just upgraded Myro, please restart Python.)"
        print "Sorry, I can't upgrade the Fluke over Bluetooth."
        print "It must be upgraded manually over the serial port using lpc21isp."
        print "Please see http://wiki.roboteducation.org/IPRE_Fluke_Setup"
        return

    if url == None:
        url = "http://myro.roboteducation.org/upgrade/fluke/"
    install_count = 0
    filename = None
    if url.startswith("http://"):
        #fluke_ver = info["fluke"].split(".")
        print "Looking for Fluke upgrade at", url, "..."
        myro_ver = myro_version.split(".")
        # go to site, check for latest greater than our version
        #infp = urllib.urlopen(url)
        try:
            infp = urllib.urlopen(url)
        except:
            print "ERROR: There was an error connecting to the web to download updates. Please check your internet connection. For example, see if you can access", url, "using a web browser."
            return

        contents = infp.read()
        lines = contents.split("\n")
        infp.close()
        for file in lines:
            file = file.strip()
            if file != "" and file[0] != '#':
                print "Considering", file, "..."
                if file.startswith("/fluke-upgrade-"):
                    end = file.index(".hex")
                    patch_ver = file[15:end].split(".")
                    print patch_ver, version
                    if map(int, patch_ver) > map(int, version):
                        # download it
                        print "   Downloading..."
                        filename = url_retrieve(url + file)
                        break
    else:
        filename = url

    if filename == None:
        print "Nothing found to upgrade!"
        return
    #info = myro.globvars.robot.getInfo()
    sendMagicKey = True

    if version <= [2, 5, 0]:
        sendMagicKey = False
        print "Older firmware version, Not sending magic key"
    else:
        print "Sending magic key"
        
    from intelhex import IntelHex
    import time
    ih = IntelHex(filename)
    binarray = ih.tobinarray()
    arlen = len(binarray)    
    print "%d bytes of firmware." % arlen
    print "checksumming interrupt vectors"
    sum = check_sum(binarray, arlen)
    #declare a finite sized array to hold eeprom dump. 
    #Dynamic appending of lists always comes with a performance hit
    eepromdump = [0] * 135168
    s.flushOutput()
    s.flushInput()
       
    #print "Getting old EEPROM"
    #s.write(chr(SAVE_EEPROM))
    #uf_saveEEPROMdump()
    print "Sending firmware"
    s.write(chr(UPDATE_FIRMWARE))
    if sendMagicKey:        
        # magic code to ensure we don't enter program by accident    
        s.write(chr(0x01))
        s.write(chr(0x23))
        
    uf_storeinEEPROM(s, arlen, binarray)
    print "Waiting for reboot..."
    time.sleep(2)
    print "Done upgrading! Please turn your robot off and then back on, and exit and restart Python and Myro." 
    s.close()
