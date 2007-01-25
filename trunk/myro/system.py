import zipfile, tarfile, urllib
import os, string
from myro import __VERSION__ as myro_version

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

def import_url(url, tmp_dir = "."):
    """ Retrieves the contents of a url, and then calls import_file. """
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
    VALUES = {"PYTHONDIR": "c:\\Python24\\",
              "HOME": "c:\\Documents and Settings\\%USERNAME%\\",
              "DESKTOP" : "c:\\Documents and Settings\\%USERNAME%\\DESKTOP\\",
              }
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
            # now use system copy to put it where it goes
            # so to use system expansions
            # or maybe get these from environment?
            #os.system("copy file file")
    else:
        print "ERROR: no MANIFEST in upgrade; skipping"
    infp.close()
    return 1

def upgrade():
    url = "http://myro.roboteducation.org/upgrade/"
    myro_ver = map(int, myro_version.split("."))
    # go to site, check for latest greater than our version
    infp = urllib.urlopen(url)
    contents = infp.read()
    lines = contents.split("\n")
    infp.close()
    for filename in lines:
        filename = filename.strip()
        if filename != "" and filename[0] != '#':
            print filename
            if filename.startswith("myro-upgrade-"):
                end = filename.index(".zip")
                patch_ver = map(int, filename[13:end].split("."))
                if patch_ver > myro_ver:
                    print patch_ver
                    # download it
                    import_url(url + filename)
