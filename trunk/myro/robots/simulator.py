"""
Myro Simulator Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__AUTHOR__   = "Doug Blank"

import os, atexit, time, threading
from myro import Robot
from myro.robot.symbolic import TCPRobot
import myro.globvars

class SimScribbler(Robot):
    def __init__(self, id = None):
        Robot.__init__(self)
        # start the client(s):
        self._clients = []
        for port in [60000]:
            self._clients.append(TCPRobot("localhost", port))
        self.volume = 1
        self.name = "Scribby"
        self.startsong = "tada"
        self.lock = threading.Lock()
        self.delay = 0.1
    def translate(self, amount):
        return self._clients[0].translate(amount)
    def rotate(self, amount):
        return self._clients[0].rotate(amount)
    def move(self, translate, rotate):
        self.lock.acquire()
        retval = self._clients[0].move(translate, rotate)
        self.lock.release()
        return retval
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

