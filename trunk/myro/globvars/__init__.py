# Myro globals and constants

# globals:
import sys
simulator = None
myropath  = None
robot     = None
gui       = None
tts       = None
setup     = 0
mediaFolder = ""
askData   = {}
sound     = 0
if type(sys.stdout) == file:
    runtkthread = 0
else:
    runtkthread = 1
joysticks = []
formats = ('itemName', 'ItemName', 'itemName') # function, class, variables
windows = {}
pictures = {}
pixmaps = {}
images = {}
del sys

# constants:
import math
TOLERANCE    = .0001
PIOVER180    = math.pi / 180.0
PIOVER2      = math.pi / 2.0
PITIMES180   = math.pi * 180.0
DEG90RADS    = 0.5 * math.pi
COSDEG90RADS = math.cos(DEG90RADS) / 1000.0
SINDEG90RADS = math.sin(DEG90RADS) / 1000.0
del math


def getObject(env, name, type="function"):
    if type == "function":
        pattern = formats[0]
    elif type == "class":
        pattern = formats[1]
    else: # variables
        pattern = formats[2]
    return env[makeName(name, pattern)]

def makeName(name, format):
    """
    Return a name in the given format example. 
    name = "get item", 
    format = "itemName", "item_name"
    return = "getItem", "get_item"
    """
    if format[4].lower() != 'n': # name
        sep = format[4]
        cases = format.split(format[4])
    else:
        sep = ''
        cases = format[:4], format[4:]
    names = name.split(" ")
    retval = []
    # first word:
    if cases[0] == "Item":
        retval.append(names[0].title())
    elif cases[0] == "ITEM":
        retval.append(names[0].upper())
    else: # cases[0] == "item":
        retval.append(names[0].lower())
    # the rest:
    if cases[1] == "Name":
        retval.extend([word.title() for word in names[1:]])
    elif cases[1] == "NAME":
        retval.extend([word.upper() for word in names[1:]])
    else: # cases[1] == "item":
        retval.extend([word.lower() for word in names[1:]])
    sretval = ""
    w = 0
    while w < len(retval):
        if sretval != '':
            sretval += sep
        if len(retval[w]) < 2:
            word = ""
            while w < len(retval) and len(retval[w]) < 2:
                word += retval[w]
                w += 1
            sretval += word
        else:
            sretval += retval[w]
            w += 1
    return sretval
    
def makeEnvironment(local, funcs, type="function"):
    if type == "function":
        pattern = formats[0]
    elif type == "class":
        pattern = formats[1]
    else: # variables
        pattern = formats[2]
    for f in funcs:
        n = makeName(f, pattern) 
        if n not in local:
            old = makeName(f, "itemName") # delete default
            local[n] = local[old]
            del local[old]

