"""
A simulated world. 

(c) 2006, Institute for Personal Robots in Education.
Licensed with Shared Source.
"""

from myro.simulator import *

def INIT():
    # (width, height), (offset x, offset y), scale:
    sim = TkSimulator((585,596), (33,545), 173.983100)
    # x1, y1, x2, y2 in meters:
    sim.addBox(0, 0, 3, 3)
    sim.addLight(.5, .5, .5)
    sim.addRobot(60000, TkMyro("BlueMyro",
                               2.69, 2.68, 2.03,
                               ((.20, .20, -.10, -.10),
                                (.18, -.18, -.18, .18)), "blue"))
    #sim.addLine(x, y, x, y, thickness, color)
    for i in range(len(sim.robots)):
        #sim.robots[i].addDevice(BulbDevice(-.10, 0))
        sim.robots[i].addDevice(MyroIR()) # infrared
        #sim.robots[i].addDevice(MyroBumper()) # bumpers
        sim.robots[i].addDevice(MyroLightSensors()) # bumpers
    return sim
