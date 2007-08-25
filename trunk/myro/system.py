import zipfile, tarfile, urllib
import os, string
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
    """ Retrieves the contents of a url, and then calls import_file. """
    if tmp_dir == None:
        if "TMP" in os.environ:
            tmp_dir = os.environ["TMP"]
        elif "TEMP" in os.environ:
            tmp_dir = os.environ["TEMP"]
        else:
            tmp_dir = "."
    # get url into tmp_file
    path, file = url.rsplit("/", 1)
    tmp_file = tmp_dir + os.sep + file
    infp = urllib.urlopen(url)
    contents = infp.read()
    infp.close()
    outfp = open(tmp_file, "wb")
    outfp.write(contents)
    outfp.close()
    return import_file(tmp_file)

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
        VALUES["HOME"] = 'C:' + os.sep + os.environ["HOMEPATH"] + os.sep
    if "USERPROFILE" in os.environ:
        VALUES["HOME"] = os.environ["USERPROFILE"] + os.sep
    globalspath, f = myro.globvars.__file__.rsplit(os.sep, 1)
    #print "globalspath:", globalspath
    myropath, f = globalspath.rsplit(os.sep, 1)
    #print "myropath:", myropath
    sitepath, f = myropath.rsplit(os.sep, 1)
    #print "sitepath:", sitepath
    myroparts = myropath.split(os.sep)
    pythonpath = myroparts[0] + os.sep + myroparts[1] + os.sep
    VALUES["DESKTOP"] = VALUES["HOME"] + "DESKTOP" + os.sep
    VALUES["PYTHONDIR"] = pythonpath
    VALUES["MYRODIR"] = myropath + os.sep
    VALUES["PYTHONSITEDIR"] = sitepath + os.sep
    VALUES["PYTHONDIR"] = pythonpath
    install_count = 0
    if "MANIFEST" in name_list:
        manifest = infp.read("MANIFEST")
        lines = manifest.split("\n")
        for line in lines:
            f, dest = map(string.strip, line.strip().split(":"))
            director[f] = dest % VALUES
        for name in name_list:
            if name == "MANIFEST": continue
            contents = infp.read(name)
            print "   writing:", director[name], "..."
            # first write to temp file:
            outfp = open(director[name], "wb")
            outfp.write(contents)
            outfp.close()
            install_count += 1
    else:
        print "   ERROR: no MANIFEST in Myro upgrade; skipping"
    infp.close()
    return install_count

def upgrade_myro(url=None):
    """
    Takes a url or filename and upgrades Myro.
    """
    if url == None:
        url = "http://myro.roboteducation.org/upgrade/"
    install_count = 0
    if not url.startswith("http://"):
        print "Looking for Myro upgrades in file", url, "..."
        install_count += import_file(url) # which is a filename
    else:        
        print "Looking for Myro upgrades at", url, "..."
        myro_ver = map(int, myro_version.split("."))
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
                    patch_ver = map(int, filename[13:end].split("."))
                    if patch_ver > myro_ver:
                        # download it
                        print "   Downloading..."
                        install_count += import_url(url + filename)
    if install_count > 0:
        print "Done upgrading! Please exit and restart Python and Myro"
    else:
        print "Nothing to upgrade in Myro; it's up to date."
    return install_count

def upgrade_dongle(url=None):
    """
    Takes a url or filename and upgrades Myro.
    """
    if myro.globvars.robot != None:
        s = myro.globvars.robot.ser
    else:
        raise AttributeError, "need connection to robot: initialize() first"
    if url == None:
        url = "http://myro.roboteducation.org/upgrade/dongle/"
    install_count = 0
    if not url.startswith("http://"):
        print "Looking for Dongle upgrades in file", url, "..."
        f = open(url, 'r')
        install_count += load_dongle(s, f) # which is a filename
    else:        
        print "Looking for Dongle upgrades at", url, "..."
        dongle_ver = map(int, myro.globvars.robot.getInfo()["API"].split("."))
        # go to site, check for latest greater than our version
        infp = urllib.urlopen(url)
        contents = infp.read()
        lines = contents.split("\n")
        infp.close()
        consider = {}
        # find the biggest matching one:
        for filename in lines:
            filename = filename.strip()
            if filename != "" and filename[0] != '#':
                print "Considering", filename, "..."
                if filename.startswith("dongle-upgrade-"):
                    end = filename.index(".bytecode")
                    patch_ver = map(int, filename[15:end].split("."))
                    if patch_ver > dongle_ver:
                        # consider it:
                        consider[tuple(patch_ver)] = url + filename
        consider_keys = consider.keys()
        consider_keys.sort()
        if len(consider_keys) > 0:
            full_url = consider[consider_keys[-1]]
            f = urllib.urlopen(full_url)
            install_count += load_dongle(s, f)
    if install_count > 0:
        print "Done upgrading! Please exit and restart Python and Myro"
    else:
        print "Nothing to upgrade in the Dongle; it's up to date."
    return install_count

def load_dongle(s, f):
    bytes=[]
    for t in f:
        t = t.strip()
        if (len(t) > 0):
            nv = int(t)
            bytes.append(nv)
    print "Program size (bytes) = ", len(bytes)    
    f.close()
    print "Storing program in memory"
    for i in range(0, len(bytes)):
        set_scribbler_memory(s, i, bytes[i])
    print "Programming scribbler"
    set_scribbler_start_program(s, len(bytes))
    return 1

def upgrade(what="all", url = None):
    if what.lower() == "myro":
        return upgrade_myro(url)
    elif what.lower() == "dongle":
        return upgrade_dongle(url)
    elif what.lower() == "all":
        install_count = 0
        install_count += upgrade_myro(url)
        install_count += upgrade_dongle(url)
        return install_count

GET_SCRIB_PROGRAM=91  # with offset, returns the scribbler program buffer
SET_SCRIB_PROGRAM=122   # set scribbler program memory byte
SET_START_PROGRAM=123   # initiate scribbler programming process

def set_scribbler_memory(ser, offset, byte):
    ser.write(chr(SET_SCRIB_PROGRAM))
    write_2byte(ser, offset)
    ser.write(chr(byte))
    
def set_scribbler_start_program(ser, size):
    ser.write(chr(SET_START_PROGRAM))
    write_2byte(ser, size)
            
