# Myro globals and constants

# globals:
simulator = None
myropath  = None
robot     = None
gui       = None
tts       = None
setup     = 0
mediaFolder = ""
askData   = {}
sound     = 0
runtkthread = 1
joysticks = []

windows = {}
pictures = {}
pixmaps = {}
images = {}

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

# initialize all joysticks:
try:
    import pygame
    pygame.init()
except:
    pygame = None

if pygame != None:
    for i in range(pygame.joystick.get_count()):
        js = pygame.joystick.Joystick(i)
        js.init()
        joysticks.append(js)
