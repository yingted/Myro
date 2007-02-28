"""
A simulated world. 

(c) 2006, Institute for Personal Robots in Education.
Licensed with Shared Source.
"""

from myro.simulator import *

def INIT():
    # (width, height), (offset x, offset y), scale:
    sim = TkSimulator((425,423), (5,420), 208.005558)
    # x1, y1, x2, y2 in meters:
    sim.addBox(0, 0, 2, 2)
    sim.addLight(.25, .25, 1.0) # x, y, brightness
    sim.addRobot(60000, TkMyro("BlueMyro",
                               1.69, 1.68, 2.03,
                               ((.09, .09,-.09,-.09),
                                (.08,-.08,-.08, .08)), "blue"))
    for i in range(len(sim.robots)):
        #sim.robots[i].addDevice(BulbDevice(-.10, 0))
        sim.robots[i].addDevice(MyroIR()) # infrared
        #sim.robots[i].addDevice(MyroBumper()) # bumpers
        sim.robots[i].addDevice(MyroLightSensors()) # light sensors 
        sim.robots[i].addDevice(MyroLineSensors()) # downward-facing sensor
    sim.addRobot(None, TkPuck("Puck1", 1.52, 0.62, 0,
                              ((.05, .05, -.05, -.05),
                               (.05, -.05, -.05, .05)), "red"))
    # lines to follow:
    sim.addShape('line', 0.31939309860289899, 1.7424667934891489, 1.1888520892441, 1.7247227324556547, fill='black')
    sim.addShape('line', 0.2377704178488248, 0.70976244133977551, 0.3477835962564, 1.7318203568690522, fill= 'black')
    sim.addShape('line', 0.7913851220938497, 0.64588382161919577, 0.2377704178488, 0.74170175120006543, fill= 'black')
    sim.addShape('line', 0.89075186388141825, 0.25196566667562031, 0.770092248853, 0.64588382161919577, fill= 'black')
    sim.addShape('line', 1.7318203568690522, 0.52522420659143387, 0.8836542394680, 0.27325853991581356, fill= 'black')
    sim.addShape('line', 1.6679417371484726, 1.1143270329034476, 1.71407629583555, 0.51102895776463841, fill= 'black')
    sim.addShape('line', 1.1711080282106296, 1.714076295835558, 1.667941737148472, 1.0965829718699531, fill= 'black')
    return sim
