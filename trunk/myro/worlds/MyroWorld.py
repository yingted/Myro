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
        sim.robots[i].addDevice(MyroLightSensors()) # light sensors 
        sim.robots[i].addDevice(MyroLineSensors()) # downward-facing sensor
    # lines to follow:
    sim.addShape('line', 0.49430088324670612, 2.5577196865672582, 2.2530923980547537, 2.5232335784337674)
    sim.addShape('line', 0.5402823607580276, 1.2414998928056804,  0.57476846889151878, 2.5519720018783434)
    sim.addShape('line', 1.1552846224719526, 0.93112491960426036,  0.5402823607580276, 1.3334628478283235)
    sim.addShape('line', 1.1897707306054439, 0.42532866697972388,  1.1093031449606312, 0.95986334304883636)
    sim.addShape('line', 2.2415970286769231, 0.50579625262453654,  1.195518415294359, 0.48280551386887577)
    sim.addShape('line', 1.9772035329868245, 1.471407280362288,  2.1841201817877711, 0.50579625262453654)
    sim.addShape('line', 2.6381872722120709, 1.9369697401644181,  1.9886989023646549, 1.4196781181620513)
    sim.addShape('line', 2.1841201817877711, 2.5232335784337674,  2.5979534793896648, 1.8909882626530967)
    return sim
