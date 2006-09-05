# Myro globals and constants

# globals:
_simulator = None
_myropath = None
_robot = None
_gui = None
_setup = 0

# constants:
import math
TOLERANCE    = .0001
PIOVER180    = math.pi / 180.0
PIOVER2      = math.pi / 2.0
PITIMES180   = math.pi * 180.0
DEG90RADS    = 0.5 * math.pi
COSDEG90RADS = math.cos(DEG90RADS) / 1000.0
SINDEG90RADS = math.sin(DEG90RADS) / 1000.0

