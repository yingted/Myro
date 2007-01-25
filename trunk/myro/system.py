import zipfile, tarfile, urllib
import os, string

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
    VALUES = {"PYTHONDIR": "c:\\Python24"}
    if "MANIFEST" in name_list:
        manifest = infp.read("MANIFEST")
        for line in manifest:
            f, dest = map(string.strip, line.strip().split(":"))
            director[f] = dest % VALUES
        for name in name_list:
            contents = infp.read(name)
            outfp = open(director[name], "wb")
            outfp.write(contents)
            outfp.close()
    else:
        print "error: no MANIFEST in upgrade"
    infp.close()
    return 1

def upgrade():
    # go to site, check for latest greater than our version
    infp = urllib.urlopen("http://myro.roboteducation.org/upgrade/")
    contents = infp.read()
    infp.close()
    
    # download it
    # get manifest, and put each file where it belongs
    # if upgraded, should close idle
    # else, no updates available
