"""
Myro Simulator Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Doug Blank"

import os, atexit, time
from myro import Robot
import myro.globals

def _cleanup(): # copy from myro/__init__.py
    if myro.globals.robot != None:
        myro.globals.robot.stop() # hangs?
	time.sleep(.5)
        myro.globals.robot.close()
    if myro.globals.simulator != None:
       myro.globals.simulator.destroy()

class SimScribbler(Robot):
    def __init__(self, id = None):
        Robot.__init__(self)
        import myro.simulator
        globalspath, filename = os.path.split(myro.globals.__file__)
        myro.globals.myropath, directory = os.path.split(globalspath)
        self._simulator = myro.simulator.INIT(
            os.path.join(myro.globals.myropath, "worlds", "MyroWorld"))
        for port in self._simulator.ports:
            print "Simulator starting listener on port", port, "..."
            thread = myro.simulator.Thread(self._simulator, port)
            thread.start()
        # start the client(s):
        from myro.robot.symbolic import TCPRobot
        self._clients = []
        for port in self._simulator.ports:
            self._clients.append(TCPRobot("localhost", port))
        myro.globals.robot = self
        myro.globals.simulator = self._simulator
        # FIX: hack to get _cleanup called before Tk exitfunc, which hangs
        atexit.register(_cleanup)
        self.volume = 1
        self.name = "Scribby"
        self.startsong = "tada"
    def translate(self, amount):
        return self._clients[0].translate(amount)
    def rotate(self, amount):
        return self._clients[0].rotate(amount)
    def move(self, translate, rotate):
        return self._clients[0].move(translate, rotate)
    def update(self):
        return self._clients[0].update()
    def get(self, sensor = "all", *positions):
        self._clients[0].update()
        sensor = sensor.lower()
        if sensor == "stall":
            return self._clients[0].stall
        elif sensor == "startsong":
            return self.startsong
        elif sensor == "name":
            return self.name
        elif sensor == "volume":
            return self.volume
        else:
            retvals = []
            if len(positions) == 0:
                if sensor == "light":
                    return self.get("light", 0, 1, 2)
                elif sensor == "ir":
                    return self.get("ir", 0, 1)
                elif sensor == "line":
                    return self.get("line", 0, 1)
                elif sensor == "all":
                    return {"light": self.get("light"),
                            "ir": self.get("ir"),
                            "line": self.get("line"),
                            "stall": self.get("stall")}
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            for position in positions:
                position = int(position)
                if sensor == "light":
                    retvals.append(self._clients[0].light[0].value[position])
                elif sensor == "ir":
                    retvals.append(self._clients[0].ir[0].value[position])
                elif sensor == "line":
                    retvals.append(self._clients[0].line[0].value[position])
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def set(self, item, position, value = None):
        item = item.lower()
        if item == "led":
            return None
        elif item == "name":
            self.name = position
            return None
        elif item == "volume":
            self.volume = position
            return None
        elif item == "startsong":
            self.startsong = position
            return None
        else:
            raise ("invalid set item name: '%s'" % item)

