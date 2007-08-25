"""
A simple simulator.
"""
 
import SocketServer, socket, sys, threading, time, os
import myro.globvars
from myro.graphics import _tkCall, _tkExec
import Tkinter, time, math, random
try:
    import cPickle as pickle
except:
    import pickle

PIOVER180 = myro.globvars.PIOVER180
PIOVER2   = myro.globvars.PIOVER2

def INIT(filename):
    path = filename.split(os.path.sep)
    modulefile = path.pop() # module name
    module = modulefile.split(".")[0]
    search = os.path.join(*path)
    # FIX: this can't be the best way to deal with relative/absolute paths:
    if ":" in search: # DOS
        search = search.replace(":", ":" + os.path.sep)
    elif filename[0] == os.path.sep: # UNIX
        search = os.path.sep + search
    oldpath = sys.path[:] # copy
    sys.path.insert(0, search)
    print "Attempting to import '%s'..." % module
    exec("import " + module + " as userspace")
    reload(userspace)
    print "Loaded '%s'!" % userspace.__file__
    sys.path = oldpath
    try:
        userspace.INIT
    except AttributeError:
        raise ImportError, "world file needs an INIT() function"
    retval = userspace.INIT()
    return retval

class Server(SocketServer.TCPServer):
    def __init__(self, connection, handler, gui):
        handler.gui = gui
        handler.connection = connection
        SocketServer.TCPServer.__init__(self, connection, handler)
                
class Handler(SocketServer.BaseRequestHandler):
    def handle(self):
        #print 1
        self.request.setblocking(1)
        self.request.settimeout(1)
        self.gui.done = 0
        #print 2
        while not self.gui.done:
            #print 3
            try:
                request = self.request.recv(10240)
                #print "request:", request
            except:
                continue
            sockname = self.request.getsockname()
            #print "sockname:", sockname
            #print 4
            try:
                retval = self.gui.process(request, sockname)
                #print "retval:", retval
            except:
                continue
            if request == "disconnect":
                break
            try:
                self.request.send(retval)
                #print "sent ok"
            except: # broken pipe, etc
                print "broken pipe"
                self.gui.done = 1
        self.request.close()

class Thread(threading.Thread):
    def __init__(self, gui, port):
        threading.Thread.__init__(self)
        self.gui = gui
        self.ok = 1
        try:
            self.server = Server(('', port),  Handler, gui)
        except:
            print "Simulator seems to be already running."
            self.ok = 0
            return
        try:
            self.server.socket.settimeout(1) # checks to see if need to quit
            #self.server.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        except:
            print "WARN: entering deadlock zone; upgrade to Python 2.3 to avoid"

    def run(self):
        if self.ok == 0: return
        while not self.gui.quit:
            #print "running", self.gui.quit
            if "server" in dir(self):
                self.server.handle_request()
            else:
                self.gui.quit = 1
        #self.gui.destroy()
        #print "Exiting run"
        sys.exit(1)

class Updater(threading.Thread):
    def __init__(self, gui):
        threading.Thread.__init__(self)
        self.gui = gui
    def run(self):
        self.gui.quit = 0
        while not self.gui.quit:
            #print "updating"
            self.gui.step()
            time.sleep(.1)
        #print "Exiting update"
        sys.exit(1)

class Segment:
    def __init__(self, start, end, id = None, partOf = None):
        self.start = start
        self.end = end
        self.id = id
        self.partOf = partOf
        self.vertical = self.start[0] == self.end[0]
        if not self.vertical:
            self.slope = (self.end[1] - self.start[1])/(self.end[0] - self.start[0])
            self.yintercept = self.start[1] - self.start[0] * self.slope
    def setAngle(self, angle):
        """ Keep first point, set second based on angle. """
        pass
    def length(self):
        return math.sqrt((self.start[0] - self.end[0])**2 +
                         (self.start[1] - self.end[1])**2)
    def angle(self):
        return math.atan2(self.end[1] - self.start[1], self.end[0] - self.start[0])
    def parallel(self, other):
        if self.vertical:
            return other.vertical
        elif other.vertical:
            return 0
        else:
            return self.slope == other.slope
    # return the point at which two segments would intersect if they extended
    # far enough
    def intersection(self, other, thisBounded = 1, otherBounded = 1):
        if self.parallel(other):
            # the segments may intersect, but we don't care
            return None
        elif self.vertical:
            return other.intersection(self, otherBounded, thisBounded)
        elif other.vertical:
            return (other.start[0],
                    self.yintercept + other.start[0] * self.slope)
        else:
            # m1x + b1 = m2x + b2; so
            # (m1 - m2)x + b1 - b2 == 0
            # (m1 - m2)x = b2 - b1
            # x = (b2 - b1)/(m1 - m2)
            # figure intersect:
            x = ((other.yintercept - self.yintercept) / (self.slope - other.slope))
            return (x, self.yintercept + x * self.slope)
    def in_bbox(self, point):
        return ((point[0] <= self.start[0] and point[0] >= self.end[0] or
                 point[0] <= self.end[0] and point[0] >= self.start[0]) and
                (point[1] <= self.start[1] and point[1] >= self.end[1] or
                 point[1] <= self.end[1] and point[1] >= self.start[1]))
    # is a point collinear with this line segment?
    def on_line(self, point):
        if self.vertical:
            return point[0] == self.start[0]
        else:
            return (point[0] * self.slope + self.yintercept == point[1])
    def intersects(self, other, thisBounded = 1, otherBounded = 1):
        if self.parallel(other):
            # they can "intersect" if they are collinear and overlap
            if not (self.in_bbox(other.start) or self.in_bbox(other.end)):
                return None
            elif self.vertical:
                if self.start[0] == other.start[0]:
                    return self.intersection(other, thisBounded, otherBounded)
                else:
                    return None
            else:
                if self.yintercept == other.yintercept:
                    return self.intersection(other, thisBounded, otherBounded)
                else:
                    return None
        else:
            i = self.intersection(other, thisBounded, otherBounded)
            if (((thisBounded and self.in_bbox(i)) or not thisBounded) and
                ((otherBounded and other.in_bbox(i)) or not otherBounded)):
                return i
            else:
                return None

MAXRAYLENGTH = 1000.0 # some large measurement in meters
colorMap = {"red": (255, 0,0),
            "green": (0, 255,0),
            "blue": (0, 0,255),
            "white": (255, 255, 255),
            "black": (0, 0, 0),
            "cyan": (0, 255, 255),
            "yellow": (255, 255, 0),
            "brown": (165, 42, 42),
            "orange": (255, 165, 0),
            "pink": (255, 192, 203),
            "violet": (238, 130, 238),
            "purple": (160, 32, 240),
            }
colorCode = {"red":     1,
             "green":   2,
             "blue":    3,
             "white":   4,
             "black":   5,
             "cyan":    6,
             "yellow":  7,
             "brown":   8,
             "orange":  9,
             "pink":   10,
             "violet": 11,
             "purple": 12
             }

def sgn(v):
    if v >= 0: return +1
    else:      return -1

class Simulator:
    def __init__(self, (width, height), (offset_x, offset_y), scale, run=1):
        self.robots = []
        self.robotsByName = {}
        self.lights = []
        self.trail = []
        self.needToMove = [] # list of robots that need to move (see step)
        self.maxTrailSize = 10 # 5 * 60 * 10 # 5 minutes (one timeslice = 1/10 sec)
        self.trailStart = 0
        self.world = []
        self.time = 0.0
        self.timeslice = 100 # in milliseconds
        self.scale = scale
        self.offset_x = offset_x
        self.offset_y = offset_y
        self._width, self._height = width, height
        self.lightAboveWalls = 0
        # connections to myro:
        self.ports = []
        self.assoc = {}
        self.done = 0
        self.quit = 0
        self.properties = ["stall", "x", "y", "th", "thr", "energy"]
        self.supportedFeatures = ["range-sensor", "continuous-movement", "odometry"]
        self.stepCount = 0
        self.display = {"wireframe": 0}
        self.running = 0
        self.stop = 0 # use to stop the sim
        self.mode = "view"
        self.lightsOrig = []
        self.shapesOrig = []
        self.worldOrig = []
    def resetPaths(self): pass
    def resetPath(self, pos): pass
    def update_idletasks(self): pass
    def mainloop(self):
        """ Simulates what TkSimulator does. """
        self.running = 1
        #print "starting loop..."
        while not self.done:
            #print "running"
            self.step()
            time.sleep(self.timeslice/1000.0) # to run in real time
        self.running = 0
    def destroy(self):
        self.done = 1 # stop processing requests, if handling
        self.quit = 1 # stop accept/bind toplevel
    def __getitem__(self, name):
        if name in self.robotsByName:
            return self.robotsByName[name]
        else:
            return None
    def remove(self, thing):
        pass
    def update(self):
        pass

    def quit(self):
        self.done = 1
        
    def addWall(self, x1, y1, x2, y2, color="black"):
        seg = Segment((x1, y1), (x2, y2), len(self.world) + 1, "wall")
        seg.color = color
        seg.type = "wall"
        self.world.append(seg)

    def addShape(self, name, *args, **nargs):
        # addShape("box", x, y, x, y, fill="black")
        # addShape("polygon", *points, fill = "black", outline = "purple")
        # addShape("line", x1, y1, x2, y2, fill = "purple", width?)
        # addShape("oval", x1, y1, x2, y2, fill = "purple", outline="yellow")
        self.shapes.append( (name, args, nargs) )
        self.redraw()

    def addBox(self, ulx, uly, lrx, lry, color="white", wallcolor="black"):
        self.addWall( ulx, uly, ulx, lry, wallcolor)
        self.addWall( ulx, uly, lrx, uly, wallcolor)
        self.addWall( ulx, lry, lrx, lry, wallcolor)
        self.addWall( lrx, uly, lrx, lry, wallcolor)

    def addLight(self, x, y, brightness, color="yellow"):
        self.lights.append(Light(x, y, brightness, color))
        self.redraw()

    def redraw(self): pass

    def addRobot(self, port, r):
        self.robots.append(r)
        self.robotsByName[r.name] = r
        self.trail.append([None] * self.maxTrailSize)
        r.simulator = self
        r._xya = r._gx, r._gy, r._ga # save original position for later reset
        if port != None:
            self.assoc[port] = r
            self.ports.append(port)

    def scale_x(self, x): return self.offset_x + (x * self.scale)
    def scale_y(self, y): return self.offset_y - (y * self.scale)

    def addTrail(self, pos, index, robot):
        self.trail[pos][index] = robot._gx, robot._gy, robot._ga


    def stepDirect(self, run = 1):
        return self._step_help(run)

    def step(self, run = 1):
        #return _tkCall(self._step_help, run)

    #def _step_help(self, run = 1):
        """
        Advance the world by timeslice milliseconds.
        """
        # might want to randomize this order so the same ones
        # don't always move first:
        self.needToMove = []
        self.time += (self.timeslice / 1000.0)
        i = 0
        for r in self.robots:
            # first, grab the velocities in case they try to change:
            r.ovx, r.ovy, r.ova = r.vx, r.vy, r.va
            #r.lock.acquire()
            resetVelocities = 0
            if r.stall:
                resetVelocities = 1
                ovx, r.ovx = r.ovx, r.ovx/5.0
                ovy, r.ovy = r.ovy, r.ovy/5.0
                ova, r.ova = r.ova, r.ova/5.0
            r.step(self.timeslice)
            if r.type != "puck" and resetVelocities:
                r.vx = ovx
                r.vy = ovy
                r.va = ova
            self.addTrail(i, self.stepCount % self.maxTrailSize, r)
            #r.lock.release()
            i += 1
        for r in self.needToMove:
            r.step(self.timeslice, movePucks = 0)
        if self.stepCount > self.maxTrailSize:
            self.trailStart = ((self.stepCount + 1) % self.maxTrailSize)
        self.stepCount += 1
    def drawLine(self, x1, y1, x2, y2, fill, tag, **args):
        pass
    def drawOval(self, x1, y1, x2, y2, **args):
        pass
    def castRay(self, robot, x1, y1, a, maxRange = MAXRAYLENGTH,
                ignoreRobot = "self",
                rayType = "range"):
        # ignoreRobot: all, self, other; 
        hits = []
        x2, y2 = math.sin(a) * maxRange + x1, math.cos(a) * maxRange + y1
        seg = Segment((x1, y1), (x2, y2))
        # go down list of walls, and see if it hit anything:
        # check if it is not a light ray, or if it is, and not above walls:
        if (rayType != "light") or (rayType == "light" and not self.lightAboveWalls):
            for w in self.world:
                retval = w.intersects(seg)
                if retval:
                    dist = Segment(retval, (x1, y1)).length()
                    if dist <= maxRange:
                        hits.append( (dist, retval, w) ) # distance, hit, obj
        # go down list of robots, and see if you hit one:
        if ignoreRobot != "all":
            for r in self.robots:
                # don't hit your own bounding box if ignoreRobot == "self":
                if r.name == robot.name and ignoreRobot == "self": continue
                # don't hit other's bounding box if ignoreRobot == "other":
                if r.name != robot.name and ignoreRobot == "other": continue
                a90 = r._ga + PIOVER2
                cos_a90 = math.cos(a90)
                sin_a90 = math.sin(a90)
                segments = []
                if r.boundingBox != []:
                    xys = map(lambda x, y: (r._gx + x * cos_a90 - y * sin_a90,
                                            r._gy + x * sin_a90 + y * cos_a90),
                              r.boundingBox[0], r.boundingBox[1])
                    # for each of the bounding box segments:
                    for i in range(len(xys)):
                        w = Segment( xys[i], xys[i - 1]) # using the previous one completes the polygon
                        w.color = r.color
                        w.type = r.type
                        w.robot = r
                        segments.append(w)
                if r.boundingSeg != []:
                    # bounding segments
                    xys = map(lambda x, y: (r._gx + x * cos_a90 - y * sin_a90,
                                            r._gy + x * sin_a90 + y * cos_a90),
                              r.boundingSeg[0], r.boundingSeg[1])
                    # for each of the bounding segments:
                    for i in range(0, len(xys), 2):
                        w = Segment( xys[i], xys[i + 1]) # assume that they come in pairs
                        w.color = r.color
                        w.type = r.type
                        w.robot = r
                        segments.append(w)
                for s in r.additionalSegments(r._gx, r._gy, cos_a90, sin_a90,
                                              color = r.color, type = r.type, robot = r):
                    segments.append(s)
                for w in segments:
                    retval = w.intersects(seg)
                    if retval:
                        dist = Segment(retval, (x1, y1)).length()
                        if dist <= maxRange:
                            hits.append( (dist, retval, w) ) # distance,hit,obj
        if len(hits) == 0:
            return (None, None, None)
        else:
            return min(hits)

    def process(self, request, sockname, pickleIt = 1):
        #return _tkCall(self._process_help(request, sockname, pickleIt))

    #def _process_help(self, request, sockname, pickleIt = 1):
        """
        Process does all of the work.
        request  - a string message
        sockname - (IPNUMBER (str), SOCKETNUM (int)) from client
        """
        retval = 'error'
        if request == 'reset':
            self.reset()
            retval = None
        elif request.count('connectionNum'):
            connectionNum, port = request.split(":")
            retval = self.ports.index( int(port) )
        elif request == 'end' or request == 'exit':
            retval = None
            self.done = 1
        elif request == 'quit':
            retval = None
            self.done = 1
            self.quit = 1
        elif request == "disconnect":
            retval = None
        elif request == 'properties':
            retval = self.properties
        elif request == 'supportedFeatures':
            retval = self.supportedFeatures
        elif request == 'builtinDevices':
            retval = self.assoc[sockname[1]].builtinDevices
        elif request == 'forward':
            self.assoc[sockname[1]].move(0.3, 0.0)
            retval = None
        elif request == 'left':
            self.assoc[sockname[1]].move(0.0, 0.3)
            retval = None
        elif request == 'right':
            self.assoc[sockname[1]].move(0.0, -0.3)
            retval = None
        elif request == 'back':
            self.assoc[sockname[1]].move(-0.3, 0.0)
            retval = None
        elif request == 'name':
            retval = self.assoc[sockname[1]].name
        elif request == 'x':
            retval = self.assoc[sockname[1]].x
        elif request == 'energy':
            retval = self.assoc[sockname[1]].energy
        elif request == 'y':
            retval = self.assoc[sockname[1]].y
        elif request == 'stall':
            retval = self.assoc[sockname[1]].stall
        elif request == 'radius':
            retval = self.assoc[sockname[1]].radius
        elif request == 'thr':
            retval = self.assoc[sockname[1]].a
        elif request == 'th':
            retval = self.assoc[sockname[1]].a / PIOVER180
        elif len(request) > 1 and request[0] == '!': # eval
            try:
                retval = eval(request[1:])
            except:
                try:
                    exec request[1:]
                    retval = None
                except:
                    retval = "error"
        else:
            # assume a package
            message = request.split("_")
            if message[0] == "m": # "m_t_r" move:translate:rotate
                t, r = 0, 0
                try: t, r = float(message[1]), float(message[2])
                except: pass
                retval = self.assoc[sockname[1]].move(t, r)
            elif message[0] == "a": # "a_name_x_y_th" simulation placement
                simulation, name, x, y, thr = None, None, None, None, None
                try:
                    simulation, name, x, y, thr = message
                    x = float(x)
                    y = float(y)
                    thr = float(thr)
                except: pass
                if name in self.robotsByName:
                    r = self.robotsByName[name]
                    r.setPose(x, y, thr, 1)#handofgod
                    r.localize(0, 0, 0)
                    return None
                elif name.isdigit():
                    pos = int(name)
                    r = self.robots[pos]
                    r.setPose(x, y, thr, 1)#handofgod
                    r.localize(0, 0, 0)
                    return None
                return "error: no such robot position '%s'" % name
            elif message[0] == "b": # "b_x_y_th" localize
                localization, x, y, thr = None, None, None, None
                try:
                    localization, x, y, thr = message
                    x = float(x)
                    y = float(y)
                    thr = float(thr)
                except: pass
                retval = self.assoc[sockname[1]].localize(x, y, thr)
            elif message[0] == "c": # "c_name" getpose
                simulation, name = None, None
                try:
                    simulation, name = message
                except: pass
                if name in self.robotsByName:
                    r = self.robotsByName[name]
                    retval = (r._gx, r._gy, r._ga)
                elif name.isdigit():
                    pos = int(name)
                    r = self.robots[pos]
                    retval = (r._gx, r._gy, r._ga)
            elif message[0] == "f": # "f_i_v" rgb[i][v]
                index, pos = 0, 0
                try:
                    index, pos = int(message[1]), int(message[2])
                except: pass
                device = self.assoc[sockname[1]].getIndex("light", index)
                if device:
                    retval = device.rgb[pos]
            elif message[0] == "h": # "h_v" bulb:value
                val = None
                try:
                    val = float(message[1])
                except: pass
                self.assoc[sockname[1]].bulb.brightness = val
                self.redraw()
                retval = None
            elif message[0] == "i": # "i_name_index_property_val"
                try:
                    code, dtype, index, property, val = message
                    index = int(index)
                    device = self.assoc[sockname[1]].getIndex(dtype, index)
                    oldval = device.__dict__[property]
                    if type(oldval) == str:
                        device.__dict__[property] = val
                    elif type(oldval) == int:
                        device.__dict__[property] = int(val)
                    elif type(oldval) == float:
                        device.__dict__[property] = float(val)
                    retval = None
                except: pass
            elif message[0] == "j": # "j_index_p_t_z" ptz[index].setPose(p, t, z)
                code, index, p, t, z = [None] * 5
                try:
                    code, index, p, t, z = message
                    index = int(index)
                except: pass
                device = self.assoc[sockname[1]].getIndex("ptz", index)
                if device:
                    if p == "None": p = None
                    else:          p = float(p)
                    if t == "None": t = None
                    else:          t = float(t)
                    if z == "None": z = None
                    else:          z = float(z)
                    retval = device.setPose(p, t, z)
            elif message[0] == "k": # "k_index" ptz[index].getPose()
                try:
                    code, index = message
                    index = int(index)
                except: pass
                device = self.assoc[sockname[1]].getIndex("ptz", index)
                if device:
                    retval = device.getPose()
            elif message[0] == "t": # "t_v" translate:value
                val = 0
                try:
                    val = float(message[1])
                except: pass
                retval = self.assoc[sockname[1]].translate(val)
            elif message[0] == "v": # "v_v" global step scalar:value
                val = 0
                try:
                    val = float(message[1])
                except: pass                
                self.assoc[sockname[1]].stepScalar = val
                retval = None
            elif message[0] == "o": # "o_v" rotate:value
                val = 0
                try:
                    val = float(message[1])
                except: pass
                retval = self.assoc[sockname[1]].rotate(val)
            elif message[0] == "d": # "d_sonar" display:keyword
                val = 0
                try:
                    val = message[1]
                except: pass
                retval = self.assoc[sockname[1]].display[val] = 1
            elif message[0] == "e": # "e_amt" eat:keyword
                val = 0
                try:
                    val = float(message[1])
                except: pass
                retval = self.assoc[sockname[1]].eat(val)
            elif message[0] == "x": # "x_expression" expression
                try:
                    retval = eval(message[1])
                except: pass
            elif message[0] == "z": # "z_gripper_0_command" command
                dtype, index, command = None, None, None
                try:
                    dtype = message[1]
                    index = int(message[2])
                    command = message[3]
                except: pass
                device = self.assoc[sockname[1]].getIndex(dtype, index)
                if device:
                    retval = device.__class__.__dict__[command](device)
            elif message[0] == "g": # "g_sonar_0" geometry_sensor_id
                index = 0
                for d in self.assoc[sockname[1]].devices:
                    if d.type == message[1]:
                        if int(message[2]) == index:
                            if message[1] in ["sonar", "laser", "light", "bulb", "ir", "bumper"]:
                                retval = d.geometry, d.arc, d.maxRange
                            elif message[1] == "camera":
                                retval = d.width, d.height
                            elif message[1] == "line":
                                retval = d.geometry
                        index += 1
            elif message[0] == "r": # "r_sonar_0" groups_sensor_id; get group names
                index = 0
                for d in self.assoc[sockname[1]].devices:
                    if d.type == message[1]:
                        if int(message[2]) == index:
                            if message[1] in ["sonar", "laser", "light", "ir", "bumper", "line"]:
                                retval = d.groups
                        index += 1
            elif message[0] == "s": # "s_sonar_0" subscribe
                if message[1] in self.assoc[sockname[1]].display and self.assoc[sockname[1]].display[message[1]] != -1:
                    self.assoc[sockname[1]].display[message[1]] = 1
                self.properties.append("%s_%s" % (message[1], message[2]))
                self.assoc[sockname[1]].subscribed = 1
                retval = None
            elif message[0] in ["sonar", "laser", "line", "light", "camera", "gripper", "ir", "bumper"]: # sonar_0, light_0...
                index = 0
                for d in self.assoc[sockname[1]].devices:
                    if d.type == message[0]:
                        try:    i = int(message[1])
                        except: i = -1
                        if i == index:
                            retval = d.scan
                        index += 1
        if pickleIt:
            return pickle.dumps(retval)
        else:
            return retval

class TkSimulator(Tkinter.Toplevel, Simulator):
    def __init__(self, dimensions, offsets, scale, root = None, run = 0):
        #_tkCall(self._init_help, dimensions, offsets, scale, root, run)

    #def _init_help(self, dimensions, offsets, scale, root = None, run = 0):
        #if root == None:
        #    if myro.globvars.gui == None:
        #        myro.globvars.gui = Tkinter.Tk()
        #        myro.globvars.gui.withdraw()
        Tkinter.Toplevel.__init__(self, myro.globvars.gui) # , myro.globvars.gui)
        Simulator.__init__(self, dimensions, offsets, scale)
        self.root = myro.globvars.gui # root
        self.wm_title("Myro Simulator")
        self.protocol('WM_DELETE_WINDOW',self.destroy)
        self.frame = Tkinter.Frame(self)
        self.frame.pack(side = 'bottom', expand = "yes", anchor = "n",
                        fill = 'both')
        self.canvas = Tkinter.Canvas(self.frame, bg="white", width=self._width, height=self._height)
        self.canvas.pack(expand="yes", fill="both", side="top", anchor="n")
        self.addMouseBindings()
        self.mBar = Tkinter.Frame(self, relief=Tkinter.RAISED, borderwidth=2)
        self.mBar.pack(fill=Tkinter.X, expand="no")
        self.lastEventRobot = None
        self.menuButtons = {}
        menu = [
            ('File', [['Reset', self.reset],
                      ['Display world details', self.printDetails],
                      ['Exit', self.destroy]]),
            ('View',[
            ['wireframe', lambda: self.simToggle("wireframe")],
            ['trail', lambda: self.toggle("trail")],                     
            ['body', lambda: self.toggle("body")],                 
            ['boundingBox', lambda: self.toggle("boundingBox")],
            ['gripper', lambda: self.toggle("gripper")],
            ['camera', lambda: self.toggle("camera")],
            ['sonar', lambda: self.toggle("sonar")],
            ['ir', lambda: self.toggle("ir")],
            ['bumper', lambda: self.toggle("bumper")],
            ['light', lambda: self.toggle("light")],                     
            ['line', lambda: self.toggle("line")],                     
            ['lineTooFar', lambda: self.toggle("lineTooFar")],                     
            ['lightBlocked', lambda: self.toggle("lightBlocked")], 
            ]
             ),
            ('Options', [['lights visible above walls',
                          lambda: self.toggleOption("lightAboveWalls")],
                         ['draw mode', lambda: self.setMode('draw')],
                         ['view mode', lambda: self.setMode('view')]]),
            ('Clear',[['lights', lambda: self.simClear("lights")],
                      ['shapes', lambda: self.simClear("shapes")],
                      ['walls', lambda: self.simClear("walls")],
                     ]
             ),
            ]
        for entry in menu:
            self.mBar.tk_menuBar(self.makeMenu(self.mBar, entry[0], entry[1]))
        self.shapes = []
    def destroy(self):
        #print "DESTROY!"
        self.quit = 1
        self.done = 1
        time.sleep(2)
        sys.exit(1)
    def setMode(self, mode):
        self.mode = mode
    def toggleOption(self, key):
        if key == "lightAboveWalls":
            self.lightAboveWalls = not self.lightAboveWalls
        else:
            raise AttributeError, "invalid key: '%s'" % key
        self.redraw()
    def simToggle(self, key):
        self.display[key] = not self.display[key]
        self.redraw()
    def simClear(self, key):
        if key == "lights":
            self.lightsOrig = self.lights
            self.lights = []
        elif key == "shapes":
            self.shapesOrig = self.shapes
            self.shapes = []
        elif key == "walls":
            self.worldOrig = self.world
            self.world = []
        self.redraw()
    def toggle(self, key):
        for r in self.robots:
            if r.subscribed == 0: continue
            if r.display[key] == 1:
                r.display[key] = 0
            else:
                r.display[key] = 1
            r._last_pose = (-1, -1, -1)
        self.redraw()
    def reset(self):
        if len(self.lightsOrig) > 0:
            self.lights = self.lightsOrig
            self.lightsOrig = []
        if len(self.shapesOrig) > 0:
            self.shapes = self.shapesOrig
            self.shapesOrig = []
        if len(self.worldOrig) > 0:
            self.world = self.worldOrig
            self.worldOrig = []
        for r in self.robots:
            r._gx, r._gy, r._ga = r._xya
            r.energy = 10000.0
        for l in self.lights:
            l.x, l.y, l.brightness = l._xyb
        self.redraw()
    def makeMenu(self, bar, name, commands):
        """ Assumes self.menuButtons exists """
        menu = Tkinter.Menubutton(bar,text=name,underline=0)
        self.menuButtons[name] = menu
        menu.pack(side=Tkinter.LEFT,padx="2m", expand="no")
        menu.filemenu = Tkinter.Menu(menu)
        for cmd in commands:
            if cmd:
                menu.filemenu.add_command(label=cmd[0],command=cmd[1])
            else:
                menu.filemenu.add_separator()
        menu['menu'] = menu.filemenu
        return menu
    def dispatch_event(self, event, type):
        if self.lastEventRobot:
            return self.lastEventRobot.mouse_event(event, type, self.lastEventRobot)
        if self.mode == "draw":  # draw line to follow
            widget = event.widget
            x = widget.canvasx(event.x)
            y = widget.canvasy(event.y)
            if type == "down":
                x -= self.offset_x
                y -= self.offset_y
                x, y = map(lambda v: float(v) / self.scale, (x, -y))
                self._drawX = x
                self._drawY = y
            elif type == "up":
                x -= self.offset_x
                y -= self.offset_y
                x, y = map(lambda v: float(v) / self.scale, (x, -y))
                self.addShape("line", x, y, self._drawX, self._drawY, fill="black", width=.03)
                self.redraw()
        elif self.mode == "view":  # else let's get a robot
            widget = event.widget
            x = widget.canvasx(event.x)
            y = widget.canvasy(event.y)
            d = 5 # overlap, in canvas units
            items = widget.find_overlapping(x-d, y-d, x+d, y+d)
            for item in items:
                tags = self.canvas.gettags(item)
                for tag in tags:
                    if "robot-" in tag:
                        robot = self.robotsByName[tag[6:]]
                        self.lastEventRobot = robot
                        return robot.mouse_event(event, type, robot)
        else:
            raise AttributeError, "unknown mode: '%s'" % self.mode
    def addMouseBindings(self):
        self.canvas.bind("<B1-Motion>", func=lambda event=self:self.dispatch_event(event, "motion"))
        self.canvas.bind("<Button-1>",  func=lambda event=self:self.dispatch_event(event, "down"))
        self.canvas.bind("<ButtonRelease-1>", func=lambda event=self:self.dispatch_event(event, "up"))
        self.canvas.bind("<Control-B1-Motion>", func=lambda event=self:self.dispatch_event(event, "control-motion"))
        self.canvas.bind("<Control-Button-1>", func=lambda event=self:self.dispatch_event(event, "control-down"))
        self.canvas.bind("<Control-ButtonRelease-1>", func=lambda event=self:self.dispatch_event(event, "control-up"))
        self.canvas.bind("<ButtonRelease-2>", self.click_b2_up)
        self.canvas.bind("<ButtonRelease-3>", self.click_b3_up)
        self.canvas.bind("<Button-2>", self.click_b2_down)
        self.canvas.bind("<Button-3>", self.click_b3_down)
        self.canvas.bind("<B2-Motion>", self.click_b2_move)
        self.canvas.bind("<B3-Motion>", self.click_b3_move)
    def click_b2_down(self, event):
        self.click_start = event.x, event.y
    def click_b3_down(self, event):
        self.click_start = event.x, event.y
        self.click_b3_move(event)
    def click_b2_up(self, event):
        self.click_stop = event.x, event.y
        if self.click_stop == self.click_start:
            # center on this position:
            center = self.canvas.winfo_width()/2, self.canvas.winfo_height()/2
            x_diff = self.click_start[0] - self.click_stop[0]
            y_diff = self.click_start[1] - self.click_stop[1]
            self.offset_x -= (self.click_stop[0] - center[0])
            self.offset_y -= (self.click_stop[1] - center[1])
        else: # move this much
            x_diff = self.click_start[0] - self.click_stop[0]
            y_diff = self.click_start[1] - self.click_stop[1]
            self.offset_x -= x_diff
            self.offset_y -= y_diff
        self.redraw()
    def click_b3_up(self, event):
        """
        Button handler for B3 for scaling window
        """
        stop = event.x, event.y
        center = self.canvas.winfo_width()/2, self.canvas.winfo_height()/2
        radius_stop = Segment(center, stop).length()
        radius_start = Segment(center, self.click_start).length()
        self.scale *= radius_stop/radius_start
        self.offset_x = (radius_stop/radius_start) * self.offset_x + (1 - (radius_stop/radius_start)) * center[0]
        self.offset_y = (radius_stop/radius_start) * self.offset_y + (1 - (radius_stop/radius_start)) * center[1]
        self.redraw()
    def click_b2_move(self, event):
        self.remove('arrow')
        self.click_stop = event.x, event.y
        x1, y1 = self.click_start
        x2, y2 = self.click_stop
        self.canvas.create_line(x1, y1, x2, y2, tag="arrow", fill="purple")
    def click_b3_move(self, event):
        self.remove('arrow')
        stop = event.x, event.y
        center = self.canvas.winfo_width()/2, self.canvas.winfo_height()/2
        radius = Segment(center, stop).length()
        self.canvas.create_oval(center[0] - radius, center[1] - radius,
                                center[0] + radius, center[1] + radius,
                                tag="arrow", outline="purple")
    def resetPath(self, num):
        for point in range(len(self.trail[num])):
            self.trail[num][point] = None
    def resetPaths(self):
        for t in range(len(self.trail)):
            self.resetPath(t)
    def redraw(self):
        self.remove('all')
        # redraw shapes on floor:
        for shape in self.shapes:
            if shape[0] == "box":
                name, (ulx, uly, lrx, lry), ops = shape
                if "fill" in ops:
                    fill = ops["fill"]
                else:
                    fill = "black"
                outline = "black"
                if self.display["wireframe"]:
                    if fill != "white":
                        outline = fill
                    else:
                        outline = "black"
                    fill = ""
                self.canvas.create_rectangle(self.scale_x(ulx), self.scale_y(uly),
                                             self.scale_x(lrx), self.scale_y(lry),
                                             tag="line", fill=fill, outline=outline)
            elif shape[0] == "polygon":
                name, points, nargs = shape
                xys = [(self.scale_x(x), self.scale_y(y)) for (x, y) in points]
                self.canvas.create_polygon(xys, tag="line", **nargs)
        for light in self.lights:
            if light.type != "fixed": continue 
            x, y, brightness, color = light.x, light.y, light.brightness, light.color
            self.drawOval((x - brightness), (y - brightness),
                          (x + brightness), (y + brightness),
                          tag="line", fill=color, outline="orange")
        for shape in self.shapes:
            if shape[0] == "line":
                name, (x1, y1, x2, y2), nargs = shape
                if "width" in nargs.keys():
                    width = nargs["width"]/2.0
                    del nargs["width"]
                else:
                    width = .03/2.0 # minimum width of a line
                seg = Segment((x1, y1), (x2, y2))
                angle = seg.angle()
                a90 = angle + math.pi/4 # perpendicular and rotated for screen
                cos_a90 = math.cos(a90)
                sin_a90 = math.sin(a90)
                # Corners of "line":
                xys = ((self.scale_x(x1 + width * cos_a90 - width * sin_a90),
                        self.scale_y(y1 + width * sin_a90 + width * cos_a90)),
                       (self.scale_x(x1 + -width * cos_a90 - -width * sin_a90),
                        self.scale_y(y1 + -width * sin_a90 + -width * cos_a90)),
                       (self.scale_x(x2 + -width * cos_a90 - -width * sin_a90),
                        self.scale_y(y2 + -width * sin_a90 + -width * cos_a90)),
                       (self.scale_x(x2 + width * cos_a90 - width * sin_a90),
                        self.scale_y(y2 + width * sin_a90 + width * cos_a90)))                       
                self.canvas.create_polygon(xys, tag="line", **nargs)
                #For just a regular line:
                #self.canvas.create_line(self.scale_x(x1), self.scale_y(y1), self.scale_x(x2), self.scale_y(y2),
                #                        tag="line", **nargs)
            elif shape[0] == "oval":
                name, (x1, y1, x2, y2), nargs = shape
                x1, y1, x2, y2 = self.scale_x(x1), self.scale_y(y1), self.scale_x(x2), self.scale_y(y2)
                self.canvas.create_oval(x1, y1, x2, y2, tag="line", **nargs)
        if not self.display["wireframe"]:
            for segment in self.world:
                (x1, y1), (x2, y2) = segment.start, segment.end
                id = self.drawLine(x1, y1, x2, y2, fill="black", tag="line")
                segment.id = id
        i = 0
        for path in self.trail:
            if self.robots[i].subscribed and self.robots[i].display["trail"] == 1:
                if path[self.trailStart] != None:
                    lastX, lastY, lastA = path[self.trailStart]
                    #lastX, lastY = self.scale_x(lastX), self.scale_y(lastY)
                    color = self.robots[i].colorParts["trail"]
                    for p in range(self.trailStart, self.trailStart + self.maxTrailSize):
                        xya = path[p % self.maxTrailSize]
                        if xya == None: break
                        x, y = xya[0], xya[1]
                        self.drawLine(lastX, lastY, x, y, fill=color, tag="trail")
                        lastX, lastY = x, y
            i += 1
        for robot in self.robots:
            robot._last_pose = (-1, -1, -1)
        if not self.running:
            self.step(run=0)
    def printDetails(self):
        print "Window: size=(%d,%d), offset=(%d,%d), scale=%f" % (self.winfo_width(), self.winfo_height(), self.offset_x, self.offset_y, self.scale)
        for robot in self.robots:
            print "   %s: pose = (%.2f, %.2f, %.2f)" % (robot.name, robot._gx, robot._gy, robot._ga % (2 * math.pi))
        for shape in self.shapes:
            name, args, nargs = shape
            print "   self.addShape('%s', %s, %s)" % (name, args, nargs)
    def addBox(self, ulx, uly, lrx, lry, color="white", wallcolor="black"):
        Simulator.addBox(self, ulx, uly, lrx, lry, color, wallcolor)
        self.addShape("box", ulx, uly, lrx, lry, fill=color)
    def addWall(self, x1, y1, x2, y2, color="black"):
        seg = Segment((x1, y1), (x2, y2), partOf="wall")
        seg.color = color
        seg.type = "wall"
        id = self.drawLine(x1, y1, x2, y2, fill=color, tag="line")
        seg.id = id
        self.world.append( seg )
    def drawLine(self, x1, y1, x2, y2, fill, tag, **args):
        return self.canvas.create_line(self.scale_x(x1), self.scale_y(y1), self.scale_x(x2), self.scale_y(y2), tag=tag, fill=fill, **args)
    def drawOval(self, x1, y1, x2, y2, **args):
        return self.canvas.create_oval(self.scale_x(x1), self.scale_y(y1),
                                       self.scale_x(x2), self.scale_y(y2),
                                       **args)
    def drawPolygon(self, points, fill="", outline="black", tag="robot", **args):
        xy = map(lambda pt: (self.scale_x(pt[0]), self.scale_y(pt[1])), points)
        if self.display["wireframe"]:
            if fill != "white":
                outline = fill
            else:
                outline = "black"
            fill = ""
        return self.canvas.create_polygon(xy, tag=tag, fill=fill, outline=outline)
    def remove(self, thing):
        self.canvas.delete(thing)
    def step(self, run = 1):
        #print "stepping..."
        self.remove('robot')
        Simulator.step(self, run)
        self.update()
    def addTrail(self, pos, index, robot):
        Simulator.addTrail(self, pos, index, robot)
        if robot.display["trail"] == 1:
            xya = self.trail[pos][(index - 1) % self.maxTrailSize]
            if xya != None:
                self.drawLine(xya[0], xya[1], robot._gx, robot._gy, robot.color, "trail")
    def update(self):
        self.update_idletasks()

class SimRobot:
    def __init__(self, name, x, y, a, boundingBox = [], color = "red"):
        if " " in name:
            name = name.replace(" ", "_")
        self.name = name
        self.type = "robot"
        #self.lock = threading.Lock()
        # set them here manually: (afterwards, use setPose)
        self.proposePosition = 0 # used to check for obstacles before moving
        self.stepScalar = 1.0 # normally = 1.0
        self._gx = x
        self._gy = y
        self._ga = a
        self.subscribed = 0
        self.x, self.y, self.a = (0.0, 0.0, 0.0) # localize
        self.boundingBox = boundingBox # ((x1, x2), (y1, y2)) NOTE: Xs then Ys of bounding box
        self.boundingSeg = []
        if boundingBox != []:
            self.radius = max(max(map(abs, boundingBox[0])), max(map(abs, boundingBox[1]))) # meters
        else:
            self.radius = 0.0
        self.builtinDevices = []
        self.color = color
        self.colorParts = {"ir": "pink", "sonar": "gray", "bumper": "black", "trail": color, "line": "green"}
        self.devices = []
        self.simulator = None # will be set when added to simulator
        self.vx, self.vy, self.va = (0.0, 0.0, 0.0) # meters / second, rads / second
        self.friction = 1.0
        # -1: don't automatically turn display on when subscribing:
        self.display = {"body": 1, "boundingBox": 0, "gripper": -1, "camera": 0, "sonar": 0,
                        "light": -1, "lightBlocked": 0, "trail": -1, "ir": 0, "bumper": 1,
                        "line": 0, "lineTooFar": 0}
        self.stall = 0
        self.energy = 10000.0
        self.maxEnergyCostPerStep = 1.0
        # FIXME: add some noise to movement
        #self.noiseTranslate = 0.01 # percent of translational noise 
        #self.noiseRotate    = 0.01 # percent of translational noise 
        self._mouse = 0 # mouse down?
        self._mouse_xy = (0, 0) # last mouse click
        self._last_pose = (-1, -1, -1) # last robot pose drawn
        self.bulb = None
        self.gripper = None
        self._mouse_offset_from_center = [0,0]

    def additionalSegments(self, x, y, cos_a90, sin_a90, **dict):
        # dynamic segments
        retval = []
        if self.gripper:
            g = self.gripper
            x1, x2, x3, x4 = g.pose[0], g.pose[0] + g.armLength, g.pose[0], g.pose[0] + g.armLength
            y1, y2, y3, y4 = g.armPosition, g.armPosition, -g.armPosition,  -g.armPosition
            if g.robot.proposePosition and g.velocity != 0.0:
                armPosition, velocity = g.moveWhere()
                y1, y2, y3, y4 = armPosition, armPosition, -armPosition,  -armPosition
            xys = map(lambda nx, ny: (x + nx * cos_a90 - ny * sin_a90,
                                      y + nx * sin_a90 + ny * cos_a90),
                      (x1, x2, x3, x4), (y1, y2, y3, y4))
            w = [Segment(xys[0], xys[1], partOf="gripper"),
                 Segment(xys[2], xys[3], partOf="gripper")]
            for s in w:
                for key in dict:
                    s.__dict__[key] = dict[key]
                retval.append(s)
        return retval
    
    def addBoundingSeg(self, boundingSeg):
        if self.boundingSeg == []:
            self.boundingSeg = boundingSeg
        else:
            self.boundingSeg[0].extend(boundingSeg[0])
            self.boundingSeg[1].extend(boundingSeg[1])
        segradius = max(max(map(abs, boundingSeg[0])), max(map(abs, boundingSeg[1]))) # meters
        self.radius = max(self.radius, segradius)
        
    def localize(self, x = 0, y = 0, th = 0):
        self.x, self.y, self.a = (x, y, th)

    def setPose(self, x = None, y = None, a = None, handOfGod = 0):
        if x != None: # we never send just x; always comes with y
            if self._mouse != 1 and not handOfGod: # if the mouse isn't down:
                # first, figure out how much we moved in the global coords:
                a90 = -self._ga
                cos_a90 = math.cos(a90)
                sin_a90 = math.sin(a90)
                dx =  (x - self._gx) * cos_a90 - (y - self._gy) * sin_a90
                dy =  (x - self._gx) * sin_a90 + (y - self._gy) * cos_a90
                # then, move that much in the local coords:
                local90 = -self.a
                cos_local90 = math.cos(local90)
                sin_local90 = math.sin(local90)
                a90 = -self.a
                self.y += dx * cos_local90 - dy * sin_local90
                self.x += dx * sin_local90 + dy * cos_local90 
                # noise: --------------------------------------------------------------
                # FIXME: should be based on the total distance moved:
                # dist = Segment((x, y), (self._gx, self._gy)).length()
                # but distributed over x and y components, gaussian?
                # Velocity should maybe play a role, too
            # just update the global position
            self._gx = x
            self._gy = y
        if a != None:
            # if our angle changes, update localized position:
            if self._mouse != 1 and not handOfGod: # if mouse isn't down
                diff = a - self._ga
                self.a += diff 
                self.a = self.a % (2 * math.pi) # keep in the positive range
            # just update the global position
            self._ga = a % (2 * math.pi) # keep in the positive range
            # noise: --------------------------------------------------------------
            # FIXME: add gaussian(noiseRotate)
    def move(self, vx, va):
        self.vx = vx
        self.va = va
        return None

    def rotate(self, va):
        self.va = va
        return None

    def translate(self, vx):
        self.vx = vx
        return None

    def getPose(self):
        """ Returns global coordinates. """
        return (self._gx, self._gy, self._ga)
    
    def getIndex(self, dtype, i):
        index = 0
        for d in self.devices:
            if d.type == dtype:
                if i == index:
                    return d
                index += 1
        return None

    def updateDevices(self):
        # measure and draw the new device data:
        if self.subscribed == 0: return
        # do some computations and save for speed
        a90 = self._ga + PIOVER2
        cos_a90 = math.cos(a90)
        sin_a90 = math.sin(a90)
        for d in self.devices:
            if not d.active: continue
            if d.type == "sonar" or d.type == "ir" or d.type == "bumper":
                i = 0
                for x, y, a in d.geometry:
                    ga = (self._ga + a)
                    gx = self._gx + (x * cos_a90 - y * sin_a90)
                    gy = self._gy + (x * sin_a90 + y * cos_a90)
                    dist, hit, obj = self.simulator.castRay(self, gx, gy, -ga, d.maxRange)
                    if hit:
                        self.drawRay(d.type, gx, gy, hit[0], hit[1], self.colorParts[d.type])
                    else:
                        hx, hy = math.sin(-ga) * d.maxRange, math.cos(-ga) * d.maxRange
                        dist = d.maxRange
                        self.drawRay(d.type, gx, gy, gx + hx, gy + hy, self.colorParts[d.type])
                    if d.type == "bumper":
                        if dist < d.maxRange: d.scan[i] = 1
                        else:                 d.scan[i] = 0
                    else: 
                        d.scan[i] = dist
                    i += 1
            elif d.type == "bulb":
                pass # nothing to update... it is not a sensor
            elif d.type == "light":
                # for each light sensor:
                i = 0
                for (d_x, d_y, d_a) in d.geometry:
                    # compute total light on sensor, falling off as square of distance
                    # position of light sensor in global coords:
                    gx = self._gx + (d_x * cos_a90 - d_y * sin_a90)
                    gy = self._gy + (d_x * sin_a90 + d_y * cos_a90)
                    sum = 0.0
                    rgb = [0, 0, 0]
                    for light in self.simulator.lights: # for each light source:
                        # these can be type == "fixed" and type == "bulb"
                        if light.type == "fixed": 
                            x, y, brightness, light_rgb = light.x, light.y, light.brightness, light.rgb
                        else: # get position from robot:
                            if light.robot == self: continue # don't read the bulb if it is on self
                            ogx, ogy, oga, brightness, color = (light.robot._gx,
                                                                light.robot._gy,
                                                                light.robot._ga,
                                                                light.brightness, light.robot.color)
                            oa90 = oga + PIOVER2
                            x = ogx + (light.x * math.cos(oa90) - light.y * math.sin(oa90))
                            y = ogy + (light.x * math.sin(oa90) + light.y * math.cos(oa90))
                            light_rgb = colorMap[color]
                        seg = Segment((x,y), (gx, gy))
                        a = -seg.angle() + PIOVER2
                        # see if line between sensor and light is blocked by any boundaries (ignore other bb)
                        dist,hit,obj = self.simulator.castRay(self, x, y, a, seg.length() - .1,
                                                               ignoreRobot = "other", rayType = "light")
                        # compute distance of segment; value is sqrt of that?
                        if not hit: # no hit means it has a clear shot:
                            self.drawRay("light", x, y, gx, gy, "orange")
                            intensity = (1.0 / (seg.length() * seg.length())) 
                            sum += min(intensity, 1.0) * brightness * 1000.0
                            for c in [0, 1, 2]:
                                rgb[c] += light_rgb[c] * (1.0/ seg.length())
                        else:
                            self.drawRay("lightBlocked", x, y, hit[0], hit[1], "purple")
                    d.scan[i] = min(sum, d.maxRange)
                    for c in [0, 1, 2]:
                        d.rgb[i][c] = min(int(rgb[c]), 255)
                    i += 1
            elif d.type == "gripper":
                # cast a ray in two places, set scan = 1 if it is "broken"
                x = d.pose[0] + .07 # first beam distance from center of robot
                y = d.armPosition # distance between arms
                d.scan = [0] * (2 + 3) # two beams, 3 sensors (no lift)
                d.objs = []
                for i in range(2): # two beams
                    gx = self._gx + (x * cos_a90 - y * sin_a90)
                    gy = self._gy + (x * sin_a90 + y * cos_a90)
                    ogx = self._gx + (x * cos_a90 + y * sin_a90)
                    ogy = self._gy + (x * sin_a90 - y * cos_a90)
                    dist,hit,obj = self.simulator.castRay(self, gx, gy, -self._ga + PIOVER2, 2 * y,
                                                          rayType = "breakBeam")
                    if hit: 
                        d.scan[i] = 1
                        d.objs.append(obj) # for gripping
                        if self.display["gripper"] == 1: # breaker beams
                            self.drawRay("gripper", gx, gy, ogx, ogy, "orange")
                    elif self.display["gripper"] == 1:
                        self.drawRay("gripper", gx, gy, ogx, ogy, "purple")
                    x += .07  # distance between beams
                d.scan[2] = d.isClosed()
                d.scan[3] = d.isOpened()
                d.scan[4] = d.isMoving()
            elif d.type == "ptz": pass
            elif d.type == "line":
                d.scan = [0] * len(d.geometry)
                pos = 0
                for geom in d.geometry:
                    d_x, d_y, d_a = geom
                    x = self._gx + (d_x * cos_a90 - d_y * sin_a90)
                    y = self._gy + (d_x * sin_a90 + d_y * cos_a90)
                    for shape in self.simulator.shapes:
                        name, args, nargs = shape
                        if name == "line":
                            # compute closest distance between x,y and line
                            # first, the line segment we are considering:
                            x1, y1, x2, y2 = args
                            seg1 = Segment((x1, y1), (x2, y2))
                            b1 = seg1.yintercept
                            self.drawRay("lineTooFar", x1, y1, 0, b1, "blue") # extended
                            angle = seg1.angle()
                            ### perpendiculars to line segment:
                            r90 = angle + math.pi/4 # perpendicular and rotated for screen
                            cos_r90 = math.cos(r90)
                            sin_r90 = math.sin(r90)
                            width = 1.0
                            x3,y3,x4,y4 = (x1 + width * cos_r90 - width * sin_r90,
                                           y1 + width * sin_r90 + width * cos_r90,
                                           x1 + -width * cos_r90 - -width * sin_r90,
                                           y1 + -width * sin_r90 + -width * cos_r90)
                            self.drawRay("lineTooFar", x3, y3, x4, y4, "orange")
                            seg2 = Segment((x3,y3), (x4, y4))
                            b2 = seg2.yintercept
                            angle2 = seg2.angle()
                            ### perpendicular through robot:
                            #b3 = y - (angle2 * x)
                            # FIX: how do you get a line from slope and point?
                            # I'm using b2 because I can't compute b3!
                            seg3 = Segment((x, y), (0, b2))
                            self.drawRay("lineTooFar", x, y, 0, b2, "green")
                            intersect1 = seg1.intersects(seg3, otherBounded = 0)
                            if intersect1 != None:
                                x3, y3 = intersect1
                                seg4 = Segment((x, y), (x3, y3))
                                dist = seg4.length()
                                if "width" in nargs:
                                    width = nargs["width"]/2.0
                                else:
                                    width = .03/2.0 # radius, in meters
                                if dist <= width:
                                    d.scan[pos] = 1
                                    self.drawRay("line", x, y, x3, y3, self.colorParts[d.type])
                                    break
                                else:
                                     self.drawRay("lineTooFar", x, y, x3, y3, "red")
                    pos += 1
            elif d.type == "camera":
                x, y = self._gx, self._gy # camera location
                stepAngle = d.zoom / float(d.width - 1)
                a = d.startAngle
                d.scan = []
                for i in range(d.width):
                    # FIX: move camera to d.pose; currently assumes robot center
                    ga = (self._ga + a) 
                    dist,hit,obj = self.simulator.castRay(self, x, y, -ga,
                                                           ignoreRobot="self",
                                                           rayType = "camera")
                    if obj != None:
                        if i in [0, d.width - 1]:
                            self.drawRay("camera", x, y, hit[0], hit[1], "purple")
                        dist = (10 - dist)/10.0 # 10 meter range
                        if obj.type == "wall":
                            height = int(min(max((dist ** 2) * d.height/2.0, 1), d.height/2))
                        else:
                            height = int(min(max((dist ** 2) * d.height/4.0, 1), d.height/4))
                        d.scan.append((colorCode[obj.color], height))
                    else:
                        d.scan.append((None, None))
                    a -= stepAngle
            else:
                raise AttributeError, "unknown type of device: '%s'" % d.type

    def eat(self, amt):
        for light in self.simulator.lights:
            if light != "fixed": continue
            dist = Segment((self._gx, self._gy), (light.x, light.y)).length()
            radius = max(light.brightness, self.radius)
            if dist <= radius and amt/1000.0 <= light.brightness:
                light.brightness -= amt/1000.0
                self.energy += amt
                self.simulator.redraw()
                return amt
        return 0.0

    def step(self, timeslice = 100, movePucks = 1):
        """
        Move the robot self.velocity amount, if not blocked.
        """
        if self._mouse: return # don't do any of this if mouse is down
        self.proposePosition = 1
        gvx = self.ovx * self.stepScalar
        gvy = self.ovy * self.stepScalar
        vx = gvx * math.sin(-self._ga) + gvy * math.cos(-self._ga)
        vy = gvx * math.cos(-self._ga) - gvy * math.sin(-self._ga)
        va = self.ova
        # proposed positions:
        p_x = self._gx + vx * (timeslice / 1000.0) # miliseconds
        p_y = self._gy + vy * (timeslice / 1000.0) # miliseconds
        p_a = self._ga + va * (timeslice / 1000.0) # miliseconds
        pushedAPuck = 0
        # for each of the robot's bounding box segments:
        a90 = p_a + PIOVER2
        cos_a90 = math.cos(a90)
        sin_a90 = math.sin(a90)
        if self.subscribed or self.type == "puck":
            if vx != 0 or vy != 0 or va != 0:
                self.energy -= self.maxEnergyCostPerStep
            # let's check if that movement would be ok:
            segments = []
            if self.boundingBox != []:
                xys = map(lambda x, y: (p_x + x * cos_a90 - y * sin_a90,
                                        p_y + x * sin_a90 + y * cos_a90),
                          self.boundingBox[0], self.boundingBox[1])
                for i in range(len(xys)):
                    bb = Segment( xys[i], xys[i - 1])
                    segments.append(bb)
            if self.boundingSeg != []:
                xys = map(lambda x, y: (p_x + x * cos_a90 - y * sin_a90,
                                        p_y + x * sin_a90 + y * cos_a90),
                          self.boundingSeg[0], self.boundingSeg[1])
                for i in range(0, len(xys), 2):
                    bb = Segment( xys[i], xys[i + 1])
                    segments.append(bb)
            for s in self.additionalSegments(p_x, p_y, cos_a90, sin_a90):
                segments.append(s)
            for bb in segments:
                # check each segment of the robot's bounding segs for wall obstacles:
                for w in self.simulator.world:
                    if bb.intersects(w):
                        self.proposePosition = 0
                        if self.gripper and self.gripper.velocity != 0:
                            self.gripper.state = "stop"
                            self.gripper.velocity = 0
                        if self.ovx != 0 or self.ovy != 0 or self.ova != 0:
                            self.stall = 1
                        self.updateDevices()
                        self.draw()
                        return
                # check each segment of the robot's bounding box for other robots:
                for r in self.simulator.robots:
                    if r.name == self.name: continue # don't compare with your own!
                    r_a90 = r._ga + PIOVER2
                    cos_r_a90 = math.cos(r_a90)
                    sin_r_a90 = math.sin(r_a90)
                    r_segments = []
                    if r.boundingBox != []:
                        r_xys = map(lambda x, y: (r._gx + x * cos_r_a90 - y * sin_r_a90,
                                                  r._gy + x * sin_r_a90 + y * cos_r_a90),
                                    r.boundingBox[0], r.boundingBox[1])
                        for j in range(len(r_xys)):
                            r_seg = Segment(r_xys[j], r_xys[j - 1])
                            r_segments.append(r_seg)
                    if r.boundingSeg != []:
                        r_xys = map(lambda x, y: (r._gx + x * cos_r_a90 - y * sin_r_a90,
                                                  r._gy + x * sin_r_a90 + y * cos_r_a90),
                                    r.boundingSeg[0], r.boundingSeg[1])
                        for j in range(0, len(r_xys), 2):
                            r_seg = Segment(r_xys[j], r_xys[j + 1])
                            r_segments.append(r_seg)
                    for s in r.additionalSegments(r._gx, r._gy, cos_r_a90, sin_r_a90):
                        r_segments.append(s)
                    for r_seg in r_segments:
                        bbintersect = bb.intersects(r_seg)
                        if r.type == "puck": # other robot is a puck
                            if bbintersect:
                                # transfer some energy to puck
                                if movePucks:
                                    r._ga = self._ga + ((random.random() - .5) * 0.4) # send in random direction, 22 degree
                                    r.vx = self.vx * 0.9 # knock it away
                                    if r not in self.simulator.needToMove:
                                        self.simulator.needToMove.append(r)
                                    if self.type == "puck":
                                        self.vx = self.vx * 0.9 # loose some
                                pushedAPuck = 1
                        elif bbintersect:
                            if self.type == "puck":
                                self.vx = 0.0
                                self.vy = 0.0
                            self.proposePosition = 0
                            if self.gripper and self.gripper.velocity != 0:
                                self.gripper.state = "stop"
                                self.gripper.velocity = 0
                            if self.ovx != 0 or self.ovy != 0 or self.ova != 0:
                                self.stall = 1
                            self.updateDevices()
                            self.draw()
                            return
        if pushedAPuck:
            # can't move this yet!
            if movePucks and r not in self.simulator.needToMove:
                self.simulator.needToMove.append( self )
            else:
                if self.gripper and self.gripper.velocity != 0:
                    self.gripper.state = "stop"
                    self.gripper.velocity = 0
                if self.ovx != 0 or self.ovy != 0 or self.ova != 0:
                    self.stall = 1
                self.updateDevices()
                self.draw()
            return
        self.proposePosition = 0
        # ok! move the robot, if it wanted to move
        if self.gripper and self.gripper.velocity != 0:
            # handle moving paddles
            d = self.gripper
            d.armPosition, d.velocity = d.moveWhere()
            if d.armPosition == d.openPosition:
                if d.storage != [] and d.state == "deploy":
                    x = d.pose[0] + d.armLength/2
                    y = 0
                    rx, ry = (p_x + x * cos_a90 - y * sin_a90,
                              p_y + x * sin_a90 + y * cos_a90)
                    r = d.storage.pop()
                    r.setPose(rx, ry, 0.0)
                    d.state = "open"
        if self.friction != 1.0:
            if r.type == "puck":
  	         self.vx *= self.friction
  	         self.vy *= self.friction
  	         if 0.0 < self.vx < 0.1: self.vx = 0.0
  	         if 0.0 < self.vy < 0.1: self.vy = 0.0
  	         if 0.0 > self.vx > -0.1: self.vx = 0.0
  	         if 0.0 > self.vy > -0.1: self.vy = 0.0
  	    else:
                self.ovx *= self.friction
                self.ovy *= self.friction
                if 0.0 < self.ovx < 0.1: self.ovx = 0.0
                if 0.0 < self.ovy < 0.1: self.ovy = 0.0
                if 0.0 > self.ovx > -0.1: self.ovx = 0.0
                if 0.0 > self.ovy > -0.1: self.ovy = 0.0
        self.stall = 0
        self.setPose(p_x, p_y, p_a)
        self.updateDevices()
        self.draw()
    def draw(self): pass
    def drawRay(self, dtype, x1, y1, x2, y2, color):
        if self.display[dtype] == 1:
            self.simulator.drawLine(x1, y1, x2, y2, color, "robot")
    def addDevice(self, dev):
        self.devices.append(dev)
        if dev.type not in self.builtinDevices:
            self.builtinDevices.append(dev.type)
        if dev.type == "bulb":
            self.simulator.lights.append( dev )
            dev.robot = self
            self.bulb = dev
        elif dev.type == "camera":
            dev.robot = self
        elif dev.type == "gripper":
            dev.robot = self
            self.gripper = dev

class TkRobot(SimRobot):
    def __init__(self, *args, **kwargs):
        SimRobot.__init__(self, *args, **kwargs)
    def mouse_event(self, event, command, robot):
        x, y = event.x, event.y
        if command[:8] == "control-":
            self._mouse_xy = x, y
            cx, cy = self.simulator.scale_x(robot._gx), self.simulator.scale_y(robot._gy)
            if command == "control-up":
                self.simulator.remove('arrow')
                a = Segment((cx, cy), (x, y)).angle()
                robot.setPose(a = (-a - PIOVER2) % (2 * math.pi))
                self._mouse = 0
                self.simulator.lastEventRobot = None
                self.simulator.redraw()
            elif command in ["control-down", "control-motion"]:
                self._mouse = 1
                self.simulator.remove('arrow')
                self.simulator.canvas.create_line(cx, cy, x, y, tag="arrow", fill="purple")
        else:
            if command == "up":
                x -= self.simulator.offset_x
                y -= self.simulator.offset_y
                x, y = map(lambda v: float(v) / self.simulator.scale, (x, -y))
                robot.setPose(x - self._mouse_offset_from_center[0],
                              y - self._mouse_offset_from_center[1])
                self._mouse = 0
                self.simulator.lastEventRobot = None
                self.simulator.redraw()
            elif command == "down":
                self._mouse = 1
                self._mouse_xy = x, y
                cx = x - self.simulator.offset_x
                cy = y - self.simulator.offset_y
                cx, cy = map(lambda v: float(v) / self.simulator.scale, (cx, -cy))
                self._mouse_offset_from_center = cx - self._gx, cy - self._gy
                self.simulator.canvas.move("robot-%s" % robot.name, x - self._mouse_xy[0], y - self._mouse_xy[1])
            elif command == "motion":
                self._mouse = 1
                self.simulator.canvas.move("robot-%s" % robot.name, x - self._mouse_xy[0], y - self._mouse_xy[1])
                self._mouse_xy = x, y
                # now move it so others will see it it correct place as you drag it:
                x -= self.simulator.offset_x
                y -= self.simulator.offset_y
                x, y = map(lambda v: float(v) / self.simulator.scale, (x, -y))
                robot.setPose(x, y)
        return "break"
class Puck(SimRobot):
    def __init__(self, *args, **kwargs):
        SimRobot.__init__(self, *args, **kwargs)
        self.radius = 0.05
        self.friction = 0.90
        self.type = "puck"

class TkPuck(TkRobot):
    def __init__(self, *args, **kwargs):
        TkRobot.__init__(self, *args, **kwargs)
        self.radius = 0.05
        self.friction = 0.90
        self.type = "puck"
    def draw(self):
        """
        Draws the body of the robot. Not very efficient.
        """
        if  self._last_pose == (self._gx, self._gy, self._ga): return # hasn't moved
        self._last_pose = (self._gx, self._gy, self._ga)
        self.simulator.remove("robot-%s" % self.name)
        if self.display["body"] == 1:
            x1, y1, x2, y2 = (self._gx - self.radius), (self._gy - self.radius), (self._gx + self.radius), (self._gy + self.radius)
            self.simulator.drawOval(x1, y1, x2, y2, fill=self.color, tag="robot-%s" % self.name, outline="black")
        if self.display["boundingBox"] == 1 and self.boundingBox != []:
            # Body Polygon, by x and y lists:
            a90 = self._ga + PIOVER2 # angle is 90 degrees off for graphics
            cos_a90 = math.cos(a90)
            sin_a90 = math.sin(a90)
            xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                   self._gy + x * sin_a90 + y * cos_a90),
                     self.boundingBox[0], self.boundingBox[1])
            self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="", outline="purple")

Pioneer = SimRobot

class TkPioneer(TkRobot):
    def __init__(self, *args, **kwargs):
        TkRobot.__init__(self, *args, **kwargs)
        self.radius = 0.4

    def draw(self):
        """
        Draws the body of the robot. Not very efficient.
        """
        if self._last_pose == (self._gx, self._gy, self._ga) and (
            (self.gripper == None) or (self.gripper and self.gripper.velocity == 0)): return # hasn't moved
        self._last_pose = (self._gx, self._gy, self._ga)
        self.simulator.remove("robot-%s" % self.name)
        # Body Polygon, by x and y lists:
        sx = [.225, .15, -.15, -.225, -.225, -.15, .15, .225]
        sy = [.08, .175, .175, .08, -.08, -.175, -.175, -.08]
        s_x = self.simulator.scale_x
        s_y = self.simulator.scale_y
        a90 = self._ga + PIOVER2 # angle is 90 degrees off for graphics
        cos_a90 = math.cos(a90)
        sin_a90 = math.sin(a90)
        if self.display["body"] == 1:
            xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                   self._gy + x * sin_a90 + y * cos_a90),
                     sx, sy)
            self.simulator.drawPolygon(xy, fill=self.color, tag="robot-%s" % self.name, outline="black")
            bx = [ .14, .06, .06, .14] # front camera
            by = [-.06, -.06, .06, .06]
            xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                   self._gy + x * sin_a90 + y * cos_a90),
                     bx, by)
            self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="black")
            if self.bulb:
                x = (self._gx + self.bulb.x * cos_a90 - self.bulb.y * sin_a90)
                y = (self._gy + self.bulb.x * sin_a90 + self.bulb.y * cos_a90)
                radius = .05
                self.simulator.drawOval(x - radius, y - radius, x + radius, y + radius,
                                        tag="robot-%s" % self.name, fill=self.color, outline="black")
            if self.gripper:
                # draw grippers:
                # base:
                xy = [(self._gx + x * cos_a90 - y * sin_a90,
                       self._gy + x * sin_a90 + y * cos_a90) for (x,y) in
                      ((self.gripper.pose[0], self.gripper.openPosition),
                       (self.gripper.pose[0], -self.gripper.openPosition))]
                self.simulator.drawLine(xy[0][0], xy[0][1], xy[1][0], xy[1][1],
                                        tag="robot-%s" % self.name, fill="black")
                # left arm:
                xs = []
                ys = []
                xs.append(self.gripper.pose[0]);     ys.append(self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(self.gripper.armPosition - 0.01)
                xs.append(self.gripper.pose[0]);     ys.append(self.gripper.armPosition - 0.01)
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         xs, ys)
                self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="black", outline="black")
                # right arm:
                xs = []
                ys = []
                xs.append(self.gripper.pose[0]);     ys.append(-self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(-self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(-self.gripper.armPosition - 0.01)
                xs.append(self.gripper.pose[0]);     ys.append(-self.gripper.armPosition - 0.01)
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         xs, ys)
                self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="black", outline="black")
        if self.display["boundingBox"] == 1:
            if self.boundingBox != []:
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         self.boundingBox[0], self.boundingBox[1])
                self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="", outline="purple")
            if self.boundingSeg != []:
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         self.boundingSeg[0], self.boundingSeg[1])
                for i in range(0, len(xy), 2):
                    self.simulator.drawLine(xy[i][0], xy[i][1],
                                            xy[i + 1][0], xy[i + 1][1],
                                            tag="robot-%s" % self.name, fill="purple")
            additionalSegments = self.additionalSegments(self._gx, self._gy, cos_a90, sin_a90)
            if additionalSegments != []:
                for s in additionalSegments:
                    self.simulator.drawLine(s.start[0], s.start[1], s.end[0], s.end[1],
                                            tag="robot-%s" % self.name, fill="purple")

class TkBlimp(TkRobot):
    def __init__(self, *args, **kwargs):
        TkRobot.__init__(self, *args, **kwargs)
        self.radius = 0.44 # meters
        self.color = "purple"

    def draw(self):
        if self._last_pose == (self._gx, self._gy, self._ga): return
        self._last_pose = (self._gx, self._gy, self._ga)
        a90 = self._ga + PIOVER2 # angle is 90 degrees off for graphics
        cos_a90 = math.cos(a90)
        sin_a90 = math.sin(a90)
        self.simulator.remove("robot-%s" % self.name)
        self.simulator.drawOval(self._gx - self.radius, self._gy - self.radius,
                                self._gx + self.radius, self._gy + self.radius,
                                tag="robot-%s" % self.name, fill=self.color, outline="blue")
        x = (self._gx + self.radius * cos_a90 - 0 * sin_a90)
        y = (self._gy + self.radius * sin_a90 + 0 * cos_a90)
        self.simulator.drawLine(self._gx, self._gy, x, y,
                                tag="robot-%s" % self.name, fill="blue", width=3)

class RangeSensor:
    def __init__(self, name, geometry, arc, maxRange, noise = 0.0):
        self.type = name
        self.active = 1
        # geometry = (x, y, a) origin in meters and radians
        self.geometry = geometry
        self.arc = arc
        self.maxRange = maxRange
        self.noise = noise
        self.groups = {}
        self.scan = [0] * len(geometry) # for data
class Light:
    def __init__(self, x, y, brightness, color="yellow"):
        self.active = 1
        self.x = x
        self.y = y
        self.brightness = brightness
        self.color = color
        self._xyb = x, y, brightness # original settings for reset
        self.rgb = colorMap[color]
        self.type = "fixed"
class BulbDevice(Light):
    """
    Bulb will have color of robot.
    """
    def __init__(self, x, y):
        Light.__init__(self, x, y, 1.0)
        self.type = "bulb"
        self.active = 1
        self.geometry = (0, 0, 0)
class LightSensor:
    def __init__(self, geometry, noise = 0.0):
        self.type = "light"
        self.active = 1
        self.geometry = geometry
        self.arc = None
        self.maxRange = 1000.0
        self.noise = noise
        self.groups = {}
        self.scan = [0] * len(geometry) # for data
        self.rgb = [[0,0,0] for g in geometry]

class Gripper:
    def __init__(self):
        self.type = "gripper"
        self.active = 1
        self.scan = []
        self.objs = []
        self.armLength  = 0.200 # length of the paddles
        self.velocity   = 0.0   # moving?
        self.openPosition  = 0.12
        self.closePosition = 0.0
        self.pose = (0.225, 0, 0) # position of gripper on robot
        self.state = "open"
        self.armPosition   = self.openPosition
        self.breakBeam = []
        self.storage = []
    def close(self):
        self.state = "close"
        self.velocity = -0.01
        return None
    def deploy(self):
        self.state = "deploy"
        self.velocity = 0.01
        return None
    def store(self):
        self.state = "store"
        self.velocity = -0.01
        for segment in self.objs:
            segment.robot.setPose(-1000.0, -1000.0, 0.0)
            if segment.robot not in self.storage:
                self.storage.append( segment.robot )
    def open(self):
        self.state = "open"
        self.velocity = 0.01
        return None
    def stop(self):
        self.state = "stop"
        self.velocity = 0.0
        return None
    def moveWhere(self):
        armPosition = self.armPosition
        velocity = self.velocity
        if velocity > 0: # opening +
            armPosition += velocity
            if armPosition >= self.openPosition:
                armPosition = self.openPosition
                velocity = 0.0
        elif velocity < 0: # closing - 
            armPosition += velocity
            if armPosition <= self.closePosition:
                armPosition = self.closePosition
                velocity = 0.0
        return armPosition, velocity
    def isClosed(self):
        return self.velocity == 0 and self.armPosition == self.closePosition
    def isOpened(self):
        return self.velocity == 0 and self.armPosition == self.openPosition
    def isMoving(self):
        return self.velocity != 0

class PTZ:
    def __init__(self, camera):
        self.type = "ptz"
        self.camera = camera
        self.active = 1
    def setPose(self, p = None, t = None, z = None):
        if p != None:
            self.camera.pan = p * PIOVER180
        if z != None:
            self.camera.zoom = z * PIOVER180
        self.camera.startAngle = self.camera.pan + self.camera.zoom/2
        self.camera.stopAngle = self.camera.pan - self.camera.zoom/2
        return None
    def getPose(self):
        return self.camera.pan / PIOVER180, 0, self.camera.zoom / PIOVER180

class Camera:
    def __init__(self, width, height, pan, zoom, x, y, thr):
        self.type = "camera"
        self.active = 1
        self.scan = []
        self.width = width
        self.height = height
        self.pan = pan * PIOVER180
        self.tilt = 0
        self.zoom = zoom * PIOVER180
        self.startAngle = self.pan + self.zoom/2
        self.stopAngle = self.pan - self.zoom/2
        self.pose = (x, y, thr)
        self.color = [[0,0,0] for i in range(self.width)]
        self.range = [0 for i in range(self.width)]

class PioneerFrontSonars(RangeSensor):
    def __init__(self):
        RangeSensor.__init__(self,
            "sonar", geometry = (( 0.10, 0.175, 90 * PIOVER180),
                                 ( 0.17, 0.15, 65 * PIOVER180),
                                 ( 0.20, 0.11, 40 * PIOVER180),
                                 ( 0.225, 0.05, 15 * PIOVER180),
                                 ( 0.225,-0.05,-15 * PIOVER180),
                                 ( 0.20,-0.11,-40 * PIOVER180),
                                 ( 0.17,-0.15,-65 * PIOVER180),
                                 ( 0.10,-0.175,-90 * PIOVER180)),
            arc = 5 * PIOVER180, maxRange = 8.0, noise = 0.0)
        self.groups = {'all': range(8),
                       'front': (3, 4),
                       'front-left' : (1,2,3),
                       'front-right' : (4, 5, 6),
                       'front-all' : (1,2, 3, 4, 5, 6),
                       'left' : (0,), 
                       'right' : (7,), 
                       'left-front' : (1,2), 
                       'right-front' : (5,6, ),
                       'left-back' : [],
                       'right-back' : [],
                       'back-right' : [],
                       'back-left' : [], 
                       'back' : [],
                       'back-all' : []}
        
class Pioneer16Sonars(RangeSensor):
    def __init__(self):
        RangeSensor.__init__(self,
            "sonar", geometry = (( 0.10, 0.175, 90 * PIOVER180),
                                 ( 0.17, 0.15, 65 * PIOVER180),
                                 ( 0.20, 0.11, 40 * PIOVER180),
                                 ( 0.225, 0.05, 15 * PIOVER180),
                                 ( 0.225,-0.05,-15 * PIOVER180),
                                 ( 0.20,-0.11,-40 * PIOVER180),
                                 ( 0.17,-0.15,-65 * PIOVER180),
                                 ( 0.10,-0.175,-90 * PIOVER180),
                                 ( -0.10,-0.175,-90 * PIOVER180),
                                 ( -0.17,-0.15, (180 + 65) * PIOVER180),
                                 ( -0.20,-0.11, (180 + 40) * PIOVER180),
                                 ( -0.225,-0.05,(180 + 15) * PIOVER180),
                                 ( -0.225, 0.05,(180 - 15) * PIOVER180),
                                 ( -0.20, 0.11, (180 - 40) * PIOVER180),
                                 ( -0.17, 0.15, (180 - 65) * PIOVER180),
                                 ( -0.10, 0.175,(180 - 90) * PIOVER180)),
            arc = 5 * PIOVER180, maxRange = 8.0, noise = 0.0)
        self.groups = {'all': range(16),
                       'front': (3, 4),
                       'front-left' : (1,2,3),
                       'front-right' : (4, 5, 6),
                       'front-all' : (1,2, 3, 4, 5, 6),
                       'left' : (0, 15), 
                       'right' : (7, 8), 
                       'left-front' : (0,), 
                       'right-front' : (7, ),
                       'left-back' : (15, ),
                       'right-back' : (8, ),
                       'back-right' : (9, 10, 11),
                       'back-left' : (12, 13, 14), 
                       'back' : (11, 12),
                       'back-all' : ( 9, 10, 11, 12, 13, 14)}
        
class PioneerFrontLightSensors(LightSensor):
    def __init__(self):
        # make sure outside of bb!
        LightSensor.__init__(self, ((.225,  .175, 0), (.225, -.175, 0)),
                             noise=0.0) 
        self.groups = {"front-all": (0, 1),
                       "all": (0, 1),
                       "front": (0, 1),
                       "front-left": (0, ),
                       "front-right": (1, ),
                       'left' : (0,), 
                       'right' : (1,), 
                       'left-front' : (0,), 
                       'right-front' : (1, ),
                       'left-back' : [],
                       'right-back' : [],
                       'back-right' : [],
                       'back-left' : [], 
                       'back' : [],
                       'back-all' : []}

class Pioneer4FrontLightSensors(LightSensor):
    def __init__(self):
        # make sure outside of bb!
        LightSensor.__init__(self, (
            (.225,  .175, 0),
            (.225,  .0875, 0),
            (.225, -.0875, 0),
            (.225, -.175, 0),
            ),
                             noise=0.0) 
        self.groups = {"front-all": (0, 1, 2, 3),
                       "all": (0, 1, 2, 3),
                       "front": (1, 2),
                       "front-left": (0, ),
                       "front-right": (3, ),
                       'left' : (0, 1), 
                       'right' : (2, 3), 
                       'left-front' : (0,), 
                       'right-front' : (3, ),
                       'left-back' : [],
                       'right-back' : [],
                       'back-right' : [],
                       'back-left' : [], 
                       'back' : [],
                       'back-all' : []}

class TkMyro(TkRobot):
    def __init__(self, *args, **kwargs):
        TkRobot.__init__(self, *args, **kwargs)
        self.radius = 0.09

    def draw(self):
        """
        Draws the body of the robot. Not very efficient.
        """
        if self._last_pose == (self._gx, self._gy, self._ga) and (
            (self.gripper == None) or (self.gripper and self.gripper.velocity == 0)): return # hasn't moved
        self._last_pose = (self._gx, self._gy, self._ga)
        self.simulator.remove("robot-%s" % self.name)
        # Body Polygon, by x and y lists:
        #sx = [ .09, .09,-.09,-.09]
        #sy = [ .08,-.08,-.08, .08] 
        sx = [ 0.05, 0.05, 0.07, 0.07, 0.09, 0.09, 0.07, 0.07, 0.05, 0.05,
               -0.05, -0.05, -0.07, -0.08, -0.09, -0.09, -0.08, -0.07, -0.05, -0.05]
        sy = [ 0.06, 0.08, 0.07, 0.06, 0.06, -0.06, -0.06, -0.07, -0.08, -0.06,
               -0.06, -0.08, -0.07, -0.06, -0.05, 0.05, 0.06, 0.07, 0.08, 0.06]  
        s_x = self.simulator.scale_x
        s_y = self.simulator.scale_y
        a90 = self._ga + PIOVER2 # angle is 90 degrees off for graphics
        cos_a90 = math.cos(a90)
        sin_a90 = math.sin(a90)
        if self.display["body"] == 1:
            xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                   self._gy + x * sin_a90 + y * cos_a90),
                     sx, sy)
            self.simulator.drawPolygon(xy, fill=self.color, tag="robot-%s" % self.name, outline="black")
            self.simulator.drawOval(self._gx - .007, self._gy - .007,
                                    self._gx + .007, self._gy + .007,
                                    fill="black", outline="black", tag="robot-%s" % self.name)
            # --------------------------------------------------------------------------
            # Parts: wheel, wheel, light, light
            bx = [[ .04, .04, -.04, -.04], [ .04, .04, -.04, -.04], [.06, .08, .08, .06],   [.06, .08, .08, .06]]
            by = [[ .08, .07, .07, .08], [ -.08, -.07, -.07, -.08], [.02, .03, .03, .02], [-.02, -.03, -.03, -.02]]
            colors = ["black", "black", "yellow", "yellow"]
            for i in range(len(bx)):
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         bx[i], by[i])
                self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill=colors[i])
            # --------------------------------------------------------------------------
            if self.bulb:
                x = (self._gx + self.bulb.x * cos_a90 - self.bulb.y * sin_a90)
                y = (self._gy + self.bulb.x * sin_a90 + self.bulb.y * cos_a90)
                radius = .04
                self.simulator.drawOval(x - radius, y - radius, x + radius, y + radius,
                                        tag="robot-%s" % self.name, fill=self.color, outline="black")
            if self.gripper:
                # draw grippers:
                # base:
                xy = [(self._gx + x * cos_a90 - y * sin_a90,
                       self._gy + x * sin_a90 + y * cos_a90) for (x,y) in
                      ((self.gripper.pose[0], self.gripper.openPosition),
                       (self.gripper.pose[0], -self.gripper.openPosition))]
                self.simulator.drawLine(xy[0][0], xy[0][1], xy[1][0], xy[1][1],
                                        tag="robot-%s" % self.name, fill="black")
                # left arm:
                xs = []
                ys = []
                xs.append(self.gripper.pose[0]);     ys.append(self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(self.gripper.armPosition - 0.01)
                xs.append(self.gripper.pose[0]);     ys.append(self.gripper.armPosition - 0.01)
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         xs, ys)
                self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="black", outline="black")
                # right arm:
                xs = []
                ys = []
                xs.append(self.gripper.pose[0]);     ys.append(-self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(-self.gripper.armPosition + 0.01)
                xs.append(self.gripper.pose[0] + self.gripper.armLength); ys.append(-self.gripper.armPosition - 0.01)
                xs.append(self.gripper.pose[0]);     ys.append(-self.gripper.armPosition - 0.01)
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         xs, ys)
                self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="black", outline="black")
        if self.display["boundingBox"] == 1:
            if self.boundingBox != []:
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         self.boundingBox[0], self.boundingBox[1])
                self.simulator.drawPolygon(xy, tag="robot-%s" % self.name, fill="", outline="purple")
            if self.boundingSeg != []:
                xy = map(lambda x, y: (self._gx + x * cos_a90 - y * sin_a90,
                                       self._gy + x * sin_a90 + y * cos_a90),
                         self.boundingSeg[0], self.boundingSeg[1])
                for i in range(0, len(xy), 2):
                    self.simulator.drawLine(xy[i][0], xy[i][1],
                                            xy[i + 1][0], xy[i + 1][1],
                                            tag="robot-%s" % self.name, fill="purple")
            additionalSegments = self.additionalSegments(self._gx, self._gy, cos_a90, sin_a90)
            if additionalSegments != []:
                for s in additionalSegments:
                    self.simulator.drawLine(s.start[0], s.start[1], s.end[0], s.end[1],
                                            tag="robot-%s" % self.name, fill="purple")

class MyroIR(RangeSensor):
    def __init__(self):
        RangeSensor.__init__(self,
                             "ir", geometry = (( 0.09, 0.05, 0),
                                               ( 0.09,-0.05, 0)),
                             arc = 5 * PIOVER180, maxRange = 0.35, noise = 0.0)
        self.groups = {'all': range(2),
                       'front': (0, 1),
                       'front-left' : (0, ),
                       'front-right' : (1, ),
                       'front-all' : (0, 1,),
                       'left' : (0,), 
                       'right' : (1,), 
                       'left-front' : (0, ), 
                       'right-front' : (1, ),
                       'left-back' : [],
                       'right-back' : [],
                       'back-right' : [],
                       'back-left' : [], 
                       'back' : [],
                       'back-all' : []}
class MyroBumper(RangeSensor):
    def __init__(self):
        RangeSensor.__init__(self,
                             "bumper", geometry = (( 0.20, 0.0, 80 * PIOVER180),
                                                   ( 0.20, 0.0,-80 * PIOVER180)),
                             arc = 5 * PIOVER180, maxRange = 0.20, noise = 0.0)
        self.groups = {'all': range(2),
                       'front': (0, 1),
                       'front-left' : (0, ),
                       'front-right' : (1, ),
                       'front-all' : (0, 1,),
                       'left' : (0,), 
                       'right' : (1,), 
                       'left-front' : (0, ), 
                       'right-front' : (1, ),
                       'left-back' : [],
                       'right-back' : [],
                       'back-right' : [],
                       'back-left' : [], 
                       'back' : [],
                       'back-all' : []}
        
class MyroLightSensors(LightSensor):
    def __init__(self):
        LightSensor.__init__(self,
                             ((.06,-0.02, 20 * PIOVER180),
                              (.07,  0.0, 0),
                              (.07,  0.02, 20 * PIOVER180),
                              ),
                             noise=0.0) 
        self.groups = {"front-all": (0, 1, 2),
                       "all": (0, 1, 2),
                       "front": (0, 1, 2),
                       "front-left": (0, ),
                       "front-right": (1, ),
                       'center-front': (2,),
                       'front-center': (2,),
                       'left' : (0,), 
                       'right' : (1,), 
                       'left-front' : (0,), 
                       'right-front' : (1, ),
                       'left-back' : [],
                       'right-back' : [],
                       'back-right' : [],
                       'back-left' : [], 
                       'back' : [],
                       'back-all' : []}

class MyroLineSensors:
    def __init__(self, geometry = [(0.05,0.005,0), (0.05,-0.005,0)], noise = 0.0):
        self.type = "line"
        self.active = 1
        self.geometry = geometry
        self.arc = None
        self.maxRange = 10.0
        self.noise = noise
        self.groups = {"all": range(len(geometry))}
        self.scan = [0] * len(geometry) # for data
        self.rgb = [[0,0,0] for g in geometry]

if __name__ == "__main__":
    globalspath, filename = os.path.split(myro.globvars.__file__)
    myro.globvars.myropath, directory = os.path.split(globalspath)
    simulator = _tkCall(INIT, os.path.join(myro.globvars.myropath, "worlds", "MyroWorld"))
    for port in [60000]:
        print "Simulator starting listener on port", port, "..."
        t = Thread(simulator, port)
        _tkCall(t.start)
    ###print "here"
    u = Updater(simulator)
    _tkCall(u.start)
    _tkCall(simulator.mainloop)
    ##print "Done!"
