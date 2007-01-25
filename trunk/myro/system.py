import zipfile, tarfile, urllib
import os, string
from myro import __VERSION__ as myro_version
import myro.globvars

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
    else:
        print "   ERROR: no MANIFEST in upgrade; skipping"
    infp.close()
    return 1

def upgrade(school = None):
    url = "http://myro.roboteducation.org/upgrade/"
    if school != None:
        url += school + "/"        
    print "Looking for upgrades at", url, "..."
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
                    import_url(url + filename)
    print "Done upgrading! Please exit and restart Python"
