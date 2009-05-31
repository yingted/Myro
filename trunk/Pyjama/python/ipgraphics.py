#
# ipgraphics.py
# graphics module for IronPython
# Mark F. Russo
# 7/18/2008
# Doug Blank
# 5/24/2009
#
# Original graphics module by John Zelle of Wartburg College
# http://mcsp.wartburg.edu/zelle/python/

import clr
clr.AddReference("Microsoft.Scripting.Core")
clr.AddReference("System")
clr.AddReference("System.Windows.Forms")
clr.AddReference("System.Drawing")
clr.AddReference("IronPython")

from System import Array
from System.Windows.Forms import *
from System import Drawing
from System.Threading import Thread, ThreadStart, AutoResetEvent
from Microsoft import Func
import math, sys

try:
    import pyjama
except:
    print "Running outside of Pyjama..."
    class pyjama:
        TopLevelControl = None
        Threaded = False

# Map color strings to internal color objects.
# This can be extended with many additional colors.
_color_map = {
    'black'         : Drawing.Color.Black,
    'lightgray'     : Drawing.Color.LightGray,
    'gray'          : Drawing.Color.Gray,
    'darkgray'      : Drawing.Color.DarkGray,
    'darkgrey'      : Drawing.Color.DarkGray,
    'slategray'     : Drawing.Color.SlateGray,
    'white'         : Drawing.Color.White,
    'red'           : Drawing.Color.Red,
    'green'         : Drawing.Color.Green,
    'green2'        : Drawing.Color.FromArgb(255,0,238,0),
    'green3'        : Drawing.Color.FromArgb(255,0,205,0),
    'blue'          : Drawing.Color.Blue,
    'yellow'        : Drawing.Color.Yellow,
    'yellow2'       : Drawing.Color.FromArgb(255,238,238,0),
    'cyan'          : Drawing.Color.Cyan,
    'peachpuff'     : Drawing.Color.PeachPuff,
    'transparent'   : Drawing.Color.Transparent,
    ''              : Drawing.Color.Transparent
}

colors = _color_map.keys()

# Font face map.
# This can be extended.
_font_face_map = {
    'helvetica'   : 'Helvetica',
    'arial'       : 'Arial',
    'courier'     : 'Courier',
    'times roman' : 'Times New Roman'
}

# Font style map.
_font_style_map = {
    'normal'        : Drawing.FontStyle.Regular,
    'bold'          : Drawing.FontStyle.Bold,
    'italic'        : Drawing.FontStyle.Italic,
    'bold italic'   : Drawing.FontStyle.Bold | Drawing.FontStyle.Italic, 
    'strikeout'     : Drawing.FontStyle.Strikeout,
    'underline'     : Drawing.FontStyle.Underline
}

# ----------------------------------------
import exceptions

class GraphicsError(exceptions.Exception):
    """Generic error class for graphics module exceptions."""
    pass

OBJ_ALREADY_DRAWN = "Object currently drawn"
UNSUPPORTED_METHOD = "Object doesn't support operation"
BAD_OPTION = "Illegal option value"
DEAD_THREAD = "Graphics thread quit unexpectedly"

# -----------------------------------------
# Create another thread on which to run GraphWin-related activities.
# Patterned after winforms.py example in IP distribution.

def _ip_thread():
    try:
        global _are
        if not pyjama.TopLevelControl:
            # Create the dummy control, and show then hide it to get
            # Windows Forms to initialize it.
            pyjama.TopLevelControl = Form(Size = Drawing.Size(0,0))  
            pyjama.TopLevelControl.Show()
            pyjama.TopLevelControl.Hide()
        # Signal that the thread running _ip_thread is ready for
        # the main thread to send input to it.
        _are.Set()                              
        Application.Run()
    finally:
        print "Cannot start ipgraphics thread"

def _ip_shutdown():
    # Close all GraphWin windows and gracefully shutdown main form
    # which terminates main thread.
    # Copy GraphWin reference to a new list first to prevent errors that 
    # occur while directly enumerating over items in Forms collection.
    frms = [f for f in Application.OpenForms if isinstance(f, GraphWin)]
    for f in frms: f.close()
    pyjama.TopLevelControl.Invoke(Func[object](Application.Exit))

# Start the thread.
if not pyjama.Threaded:
    _are = AutoResetEvent(False)
    Thread(ThreadStart(_ip_thread)).Start()
    _are.WaitOne()
    _ip_thread()
    # Interpeter exit should clean up and terminate thread.
    sys.exitfunc = _ip_shutdown
else: # else thread is running
    if not pyjama.TopLevelControl: # but no GUI, then we'll just start one here:
        print "Starting my own GUI Listener..."
        # Create the dummy control, and show then hide it to get 
        # Windows Forms to initialize it.
        pyjama.TopLevelControl = Form(Size = Drawing.Size(0,0))  
        pyjama.TopLevelControl.Show()
        pyjama.TopLevelControl.Hide()

# ----------------------------------------
class Window(Form):

    # - - - - - - - - - - - - - - - - -
    def __init__(self, title="Graphics Window",
                 width=200, height=200, autoflush=True, smooth=False):
        def __init_help():
            Form.__init__(self)
            self.Text = title
            # Add to the size of the window to account for borders
            dwidth  = self.Width  - self.ClientSize.Width
            dheight = self.Height - self.ClientSize.Height
            self.Size = Drawing.Size(width+dwidth, height+dheight)
            self.TopMost = True
            # Turn on double buffering
            try:
                self.SetStyle(ControlStyles.UserPaint, True)
                self.SetStyle(ControlStyles.DoubleBuffer, True)
                self.SetStyle(ControlStyles.ResizeRedraw, True)
                self.SetStyle(ControlStyles.AllPaintingInWmPaint, True)
                self.UpdateStyles()
            except:
                print "Mono bug: skipping double-buffer settings"

            # items list maintians a list of graphic items
            # to be drawn on the canvas.
            self.items = []
            
            self.height = height
            self.width = width
            self.closed = False
            self.autoflush = autoflush
            self.smooth = smooth
            self.trans = None
            
            self.mouseX = None
            self.mouseY = None
            self._mouseCallback = None
            
            self.Paint += self._onPaint
            self.Click += self._onClick
            self.FormClosing += self._onFormClosing
            
            self.BackColor = Drawing.Color.FromArgb(255,236,233,216)
            self.Show()
            
            # Refresh set to autoflush.
            if autoflush: self.Invalidate()

        pyjama.TopLevelControl.Invoke(Func[object](__init_help))

    # - - - - - - - - - - - - - - - - -
    def _onPaint(self, sender, e):
        # Will draw in antialias mode if smooth is True.
        if self.smooth:
            e.Graphics.SmoothingMode = Drawing.Drawing2D.SmoothingMode.AntiAlias
            
        # This is where the actual drawing occurs.
        for s in self.items: s._draw(e.Graphics)
    
    # - - - - - - - - - - - - - - - - -
    def _onFormClosing(self, sender, e):
        # Called by form when it is closing.
        self.close()

    # - - - - - - - - - - - - - - - - -
    def __checkOpen(self):
        if self.closed:
            raise GraphicsError, "window is closed"

     # - - - - - - - - - - - - - - - - -
    def append(self, itm):
        """appends a shape to this GraphWin"""
        self.items.append(itm)

    # - - - - - - - - - - - - - - - - -
    def remove(self, itm):
        """removes a shape from this GraphWin"""
        self.items.remove(itm)
    
    # - - - - - - - - - - - - - - - - -
    def setBackground(self, color):
        """Set background color of the window"""
        self.__checkOpen()
        clr = _color_map[color]
        def tmp(): self.BackColor = clr
        pyjama.TopLevelControl.Invoke(Func[object](tmp))

    # - - - - - - - - - - - - - - - - -
    def setCoords(self, x1, y1, x2, y2):
        """Set coordinates of window to run from (x1,y1) in the
        lower-left corner to (x2,y2) in the upper-right corner."""
        self.trans = Transform(self.width, self.height, x1, y1, x2, y2)

    # - - - - - - - - - - - - - - - - -
    def close(self):
        """Close this GraphWin window"""
        if self.closed: return
        def tmp(): self.__close_help()
        pyjama.TopLevelControl.Invoke(Func[object](self.__close_help))

    # - - - - - - - - - - - - - - - - -
    def __close_help(self):
        """Close the window"""
        self.closed = True
        self.Close()
    
    # - - - - - - - - - - - - - - - - -
    def isClosed(self):
        """Return True of this GraphWin is closed"""
        return self.closed

    # - - - - - - - - - - - - - - - - -
    def __autoflush(self):
        # Only called from remote thread.
        if self.autoflush:
            pyjama.TopLevelControl.Invoke(Func[object](self.Invalidate))

    # - - - - - - - - - - - - - - - - -
    def plot(self, x, y, color="black"):
        """Set pixel (x,y) to the given color"""
        self.__checkOpen()
        xs,ys = self.toScreen(x,y)
        pt = Point(xs, ys)
        pt.setOutline(color)
        pt.draw(self)
        self.__autoflush()

    # - - - - - - - - - - - - - - - - -
    def plotPixel(self, x, y, color="black"):
        """Set pixel raw (independent of window coordinates) pixel
        (x,y) to color"""
        pt = Point(x, y)
        pt.setOutline(color)
        pt.draw(self)
        self.__autoflush()

    # - - - - - - - - - - - - - - - - -
    def flush(self):
        """Flush drawing to the window"""
        self.__checkOpen()
        pyjama.TopLevelControl.Invoke(Func[object](self.Invalidate))

    # - - - - - - - - - - - - - - - - -
    def update(self):
        self.flush()

    # - - - - - - - - - - - - - - - - -
    def getMouse(self):
        """Wait for mouse click and return Point object representing
        the click"""
        # Loop runs on local thread. All calls run on remote thread.
        self.update()
        while self.mouseX == None or self.mouseY == None:
            if self.isClosed(): raise GraphicsError, "getMouse in closed window"
            Thread.Sleep(100) # give up thread
        x, y = self.toWorld(self.mouseX, self.mouseY)
        self.mouseX = None
        self.mouseY = None
        return Point(x,y)

    # - - - - - - - - - - - - - - - - -
    def checkMouse(self):
        """Return last mouse click or None if mouse has
        not been clicked since last call"""
        if self.isClosed():
            raise GraphicsError, "checkMouse in closed window"
        if self.mouseX != None and self.mouseY != None:
            x,y = self.toWorld(self.mouseX, self.mouseY)
            self.mouseX = None
            self.mouseY = None
            return Point(x,y)
        else:
            return None

    # - - - - - - - - - - - - - - - - -
    def getHeight(self):
        """Return the height of the window"""
        return self.height
    
    # - - - - - - - - - - - - - - - - -
    def getWidth(self):
        """Return the width of the window"""
        return self.width
    
    # - - - - - - - - - - - - - - - - -
    def toScreen(self, x, y):
        """Convert x,y to screen coordinates"""
        trans = self.trans
        if trans:
            return self.trans.screen(x,y)
        else:
            return int(x), int(y)
    
    # - - - - - - - - - - - - - - - - -
    def toWorld(self, x, y):
        """Convert x,y to world coordinates"""
        trans = self.trans
        if trans:
            return self.trans.world(x,y)
        else:
            return float(x), float(y)
    
    # - - - - - - - - - - - - - - - - -
    def _onClick(self, sender, e):
        # Save the location of all mouse clicks.
        # Only called from remote thread.
        self.mouseX = e.X
        self.mouseY = e.Y
        if self._mouseCallback:
            self._mouseCallback(Point(e.X, e.Y))

# -----------------------------------------------
class Transform(object):

    """Internal class for 2-D coordinate transformations"""

    # - - - - - - - - - - - - - - - - -
    def __init__(self, w, h, xlow, ylow, xhigh, yhigh):
        # w, h are width and height of window
        # (xlow,ylow) coordinates of lower-left [raw (0,h-1)]
        # (xhigh,yhigh) coordinates of upper-right [raw (w-1,0)]
        xspan = (xhigh-xlow)
        yspan = (yhigh-ylow)
        self.xbase = xlow
        self.ybase = yhigh
        self.xscale = xspan/float(w-1)
        self.yscale = yspan/float(h-1)
    
    # - - - - - - - - - - - - - - - - -
    def screen(self,x,y):
        # Returns x,y in screen (actually window) coordinates
        xs = (x-self.xbase) / self.xscale
        ys = (self.ybase-y) / self.yscale
        return int(xs+0.5),int(ys+0.5)
    
    # - - - - - - - - - - - - - - - - -
    def world(self,xs,ys):
        # Returns xs,ys in world coordinates
        x = xs*self.xscale + self.xbase
        y = self.ybase - ys*self.yscale
        return x,y

# ----------------------------------------
class GraphicsObject(object):

    """Generic base class for all of the drawable objects"""
    
    # A subclass of GraphicsObject should override _draw and
    #   and _move methods.
    
    # - - - - - - - - - - - - - - - - -
    def __init__(self):
        # When an object is drawn, canvas is set to the GraphWin(canvas)
        #    object where it is drawn.
        self.canvas = None
        self.pen = None
        self.brush = None

        # Keep copies of properties
        self.fill_color    = None
        self.outline_color = None
        self.outline_width = 1

        # Default properties are stored as
        # properties of pen and brush objects
        self.setOutline("black")
        self.setFill("transparent")

    # - - - - - - - - - - - - - - - - -
    def setFill(self, color):
        """Set interior color to color"""
        def tmp(): 
            if self.fill_color == color: return
            if self.brush: self.brush.Dispose()
            clr = _color_map[color]
            self.brush = Drawing.SolidBrush(clr)
            self.fill_color = color
        pyjama.TopLevelControl.Invoke(Func[object](tmp))
    
    # - - - - - - - - - - - - - - - - -
    def setOutline(self, color):
        """Set outline color to color"""
        def tmp(): 
            if self.outline_color == color: return
            if self.pen: self.pen.Dispose()
            clr = _color_map[color]
            self.pen = Drawing.Pen(clr, self.outline_width)
            self.outline_color = color
        pyjama.TopLevelControl.Invoke(Func[object](tmp))
        
    # - - - - - - - - - - - - - - - - -
    def setWidth(self, width):
        """Set line weight to width"""
        def tmp(): 
            if self.outline_width == width: return
            if self.pen: self.pen.Dispose()
            clr = _color_map[self.outline_color]
            self.pen = Drawing.Pen(clr, width)
            self.outline_width = width
        pyjama.TopLevelControl.Invoke(Func[object](tmp))

    # - - - - - - - - - - - - - - - - -
    def draw(self, graphwin):

        """Draw the object in graphwin, which should be a GraphWin
        object.  A GraphicsObject may only be drawn into one
        window. Raises an error if attempt made to draw an object that
        is already visible."""
        
        if self.canvas and not self.canvas.isClosed():
            raise GraphicsError, OBJ_ALREADY_DRAWN
        if graphwin.isClosed():
            raise GraphicsError, "Can't draw to closed window"
        self.canvas = graphwin
        self.canvas.append(self)
        if graphwin.autoflush:
            pyjama.TopLevelControl.Invoke(Func[object](self.canvas.Invalidate))

    # - - - - - - - - - - - - - - - - -
    def undraw(self):

        """Undraw the object (i.e. hide it). Returns silently if the
        object is not currently drawn."""

        if not self.canvas: return
        if not self.canvas.isClosed():
            self.canvas.remove(self)
            if self.canvas.autoflush:
                pyjama.TopLevelControl.Invoke(Func[object](self.canvas.Invalidate))
        self.canvas = None

    # - - - - - - - - - - - - - - - - -
    def move(self, dx, dy):

        """move object dx units in x direction and dy units in y
        direction"""
        self._move(dx,dy)
        canvas = self.canvas
        if canvas and not canvas.isClosed():
            if canvas.autoflush:
                pyjama.TopLevelControl.Invoke(Func[object](self.canvas.Invalidate))

    # - - - - - - - - - - - - - - - - -
    def _draw(self, canvas, options):
        """draws appropriate figure on canvas with options provided
        Returns Tk id of item drawn"""
        pass # must override in subclass

    # - - - - - - - - - - - - - - - - -
    def _move(self, dx, dy):
        """updates internal state of object to move it dx,dy units"""
        pass # must override in subclass

# -----------------------------------------------
class Point(GraphicsObject):

    # - - - - - - - - - - - - - - - - -
    def __init__(self, x, y):
        GraphicsObject.__init__(self)
        self.setOutline = self.setFill  # Uses rectangle fill to draw points
        self.setOutline("black")
        self.x = x
        self.y = y

    def __repr__(self):
        return "<Point at (%d,%d)>" % (self.x, self.y)
    
    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):
        # Use a rectangle fill to draw points
        x,y = self.canvas.toScreen(self.x,self.y)
        g.FillRectangle(self.brush, x, y, 1, 1)
    
    # - - - - - - - - - - - - - - - - -
    def _move(self, dx, dy):
        self.x = self.x + dx
        self.y = self.y + dy
        
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = Point(self.x,self.y)
        other.pen = self.pen.Clone()
        other.brush = self.brush.Clone()
        return other
    
    # - - - - - - - - - - - - - - - - -
    def getX(self):
        return self.x
    
    # - - - - - - - - - - - - - - - - -
    def getY(self):
        return self.y
    
# -----------------------------------------------
class _BBox(GraphicsObject):
    # Internal base class for objects represented by bounding box
    # (opposite corners) Line segment is a degenerate case.
    
    # - - - - - - - - - - - - - - - - -
    def __init__(self, p1, p2):
        GraphicsObject.__init__(self)
        self.p1 = p1.clone()
        self.p2 = p2.clone()

    # - - - - - - - - - - - - - - - - -
    def _move(self, dx, dy):
        self.p1.x = self.p1.x + dx
        self.p1.y = self.p1.y + dy
        self.p2.x = self.p2.x + dx
        self.p2.y = self.p2.y + dy
    
    # - - - - - - - - - - - - - - - - -
    def getP1(self):
        return self.p1.clone()
    
    # - - - - - - - - - - - - - - - - -
    def getP2(self):
        return self.p2.clone()
    
    # - - - - - - - - - - - - - - - - -
    def getCenter(self):
        p1 = self.p1
        p2 = self.p2
        return Point((p1.x+p2.x)/2.0, (p1.y+p2.y)/2.0)
    
# -----------------------------------------------
class Rectangle(_BBox):
    
    # - - - - - - - - - - - - - - - - -
    def __init__(self, p1, p2):
        _BBox.__init__(self, p1, p2)
    
    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):
        p1 = self.p1
        p2 = self.p2
        x1,y1 = self.canvas.toScreen(p1.x,p1.y)
        x2,y2 = self.canvas.toScreen(p2.x,p2.y)
        if x1 > x2: x2, x1 = x1, x2
        if y1 > y2: y2, y1 = y1, y2
        width, height = x2-x1, y2-y1
        g.DrawRectangle(self.pen, x1, y1, width, height)
        g.FillRectangle(self.brush, x1, y1, width, height)
        
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = Rectangle(self.p1, self.p2)
        other.pen = self.pen.Clone()
        other.brush = self.brush.Clone()
        return other
    
# -----------------------------------------------
class Oval(_BBox):
    
    # - - - - - - - - - - - - - - - - -
    def __init__(self, p1, p2):
        _BBox.__init__(self, p1, p2)
        
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = Oval(self.p1, self.p2)
        other.pen = self.pen.Clone()
        other.brush = self.brush.Clone()
        return other

    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):
        p1 = self.p1
        p2 = self.p2
        x1,y1 = self.canvas.toScreen(p1.x,p1.y)
        x2,y2 = self.canvas.toScreen(p2.x,p2.y)
        if x1 > x2: x2, x1 = x1, x2
        if y1 > y2: y2, y1 = y1, y2
        width, height = x2-x1, y2-y1
        g.DrawEllipse(self.pen, x1, y1, width, height)
        g.FillEllipse(self.brush, x1, y1, width, height)
    
# -----------------------------------------------
class Circle(Oval):
    
    # - - - - - - - - - - - - - - - - -
    def __init__(self, center, radius):
        p1 = Point(center.x-radius, center.y-radius)
        p2 = Point(center.x+radius, center.y+radius)
        Oval.__init__(self, p1, p2)
        self.radius = radius
    
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = Circle(self.getCenter(), self.radius)
        other.pen = self.pen.Clone()
        other.brush = self.brush.Clone()
        return other
    
    # - - - - - - - - - - - - - - - - -
    def getRadius(self):
        return self.radius

# -----------------------------------------------
class Line(_BBox):

    # - - - - - - - - - - - - - - - - -
    def __init__(self, p1, p2):
        _BBox.__init__(self, p1, p2)
        #self.setOutline = self.setFill
        self._arrow = "none"
        self.setFill("black")

    def __repr__(self):
        return "<Line at (%d,%d),(%d,%d)>" % (self.p1.x, 
                                              self.p1.y, 
                                              self.p2.x, 
                                              self.p2.y)
        
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = Line(self.p1, self.p2)
        other.pen = self.pen.Clone()
        other.brush = self.brush.Clone()
        return other
    
    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):
        p1 = self.p1
        p2 = self.p2
        x1,y1 = self.canvas.toScreen(p1.x,p1.y)
        x2,y2 = self.canvas.toScreen(p2.x,p2.y)
        g.DrawLine(self.pen, x1, y1, x2, y2)

        # Draw arrows
        if self._arrow in ["first", "both"]:
            self._draw_arrowhead(g, x2,y2,x1,y1)
        if self._arrow in ["last", "both"]:
            self._draw_arrowhead(g, x1,y1,x2,y2)
    
    # - - - - - - - - - - - - - - - - -
    def _draw_arrowhead(self, g, x1, y1, x2, y2):
        arrlen = 10.0                   # set the length of the arrow
        angle = 0.4                     # set the half-angle made by the arrow point
        
        xdiff = x2-x1                   # calculate unit vector
        ydiff = y2-y1
        arclen = math.sqrt( xdiff*xdiff + ydiff*ydiff )
        xunit = xdiff/arclen
        yunit = ydiff/arclen

        cospa = math.cos( angle )       # rotate the scaled unit vector by a half-angle in both directions
        sinpa = math.sin( angle )       # and tanslate back to the arrow head point
        arrowx2 = arrlen * (xunit*cospa + yunit*sinpa) + x1
        arrowy2 = arrlen * (yunit*cospa - xunit*sinpa) + y1
        cosma = math.cos(-angle )
        sinma = math.sin(-angle )
        arrowx3 = arrlen * (xunit*cosma + yunit*sinma) + x1
        arrowy3 = arrlen * (yunit*cosma - xunit*sinma) + y1
        
        # --- draw the arrow head
        pp = [Drawing.Point(x,y) for x,y in [(x1,y1),(arrowx2,arrowy2),(arrowx3,arrowy3)]]
        pts = Array[Drawing.Point](pp)
        g.DrawPolygon(self.pen, pts)
        g.FillPolygon(self.brush, pts)

    # - - - - - - - - - - - - - - - - -
    def setArrow(self, option):
        if not option in ["first","last","both","none"]:
            raise GraphicsError, BAD_OPTION
        self._arrow = option

# -----------------------------------------------
class Polygon(GraphicsObject):
    
    # - - - - - - - - - - - - - - - - -
    def __init__(self, *points):
        # if points passed as a list, extract them
        if len(points) == 1 and type(points[0] == type([])):
            points = points[0]
        self.points = map(Point.clone, points)
        GraphicsObject.__init__(self)
    
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = apply(Polygon, self.points)
        other.pen = self.pen.Clone()
        other.brush = self.brush.Clone()
        return other
    
    # - - - - - - - - - - - - - - - - -
    def getPoints(self):
        return map(Point.clone, self.points)

    # - - - - - - - - - - - - - - - - -
    def _move(self, dx, dy):
        for p in self.points:
            p.move(dx,dy)
   
    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):
        canvas = self.canvas
        pts = []
        for p in self.points:
            x,y = canvas.toScreen(p.x,p.y)
            pts.append( Drawing.Point(x,y) )
        pts = Array[Drawing.Point](pts)
        g.DrawPolygon(self.pen, pts)
        g.FillPolygon(self.brush, pts)

# -----------------------------------------------
class Text(GraphicsObject):
    
    # - - - - - - - - - - - - - - - - -
    def __init__(self, p, text):
        GraphicsObject.__init__(self)
        self.setText( text )
        self.anchor = p.clone()
        self.setFill( "black" )
        self.setOutline = self.setFill
        self.font = Drawing.Font("Helvetica", 12)
    
    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):
        canvas = self.canvas
        p = self.anchor
        x,y = canvas.toScreen(p.x,p.y)
        frmt = Drawing.StringFormat()
        frmt.Alignment = Drawing.StringAlignment.Center
        frmt.LineAlignment = Drawing.StringAlignment.Center
        g.DrawString(self._text, self.font, self.brush, x, y, frmt)
    
    # - - - - - - - - - - - - - - - - -
    def _move(self, dx, dy):
        self.anchor.move(dx,dy)
    
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = Text(self.anchor, self.config['text'])
        other.pen = self.pen.Clone()
        other.brush = self.brush.Clone()
        other.font = self.font.Clone()
        return other
    
    # - - - - - - - - - - - - - - - - -
    def setText(self,text):
        #self._text = text
        def tmp(): self._text = text
        pyjama.TopLevelControl.Invoke(Func[object](tmp))
    
    # - - - - - - - - - - - - - - - - -
    def getText(self):
        return self._text

    # - - - - - - - - - - - - - - - - -
    def getAnchor(self):
        return self.anchor.clone()
    
    # - - - - - - - - - - - - - - - - -
    def setFace(self, face):
        try:
            fface = _font_face_map[face]
        except:
            raise GraphicsError, BAD_OPTION
        fsize = self.font.Size
        fstyle = self.font.Style
        self.font.Dispose()
        self.font = Drawing.Font(fface, fsize, fstyle)

    # - - - - - - - - - - - - - - - - -
    def setSize(self, size):
        if 5 <= size <= 36:
            fsize = float(size)
        else:
            raise GraphicsError, BAD_OPTION
        fface = self.font.FontFamily
        fstyle = self.font.Style
        self.font.Dispose()
        self.font = Drawing.Font(fface, fsize, fstyle)
        
    # - - - - - - - - - - - - - - - - -
    def setStyle(self, style):
        try:
            fstyle = _font_style_map[style]
        except:
            raise GraphicsError, BAD_OPTION
        fface = self.font.FontFamily
        fsize = self.font.Size
        self.font.Dispose()
        self.font = Drawing.Font(fface, fsize, fstyle)

    # - - - - - - - - - - - - - - - - -
    def setTextColor(self, color):
        self.setFill(color)

# -----------------------------------------------
class Entry(GraphicsObject):

    # - - - - - - - - - - - - - - - - -
    def __init__(self, p, width):
        def tmp():
            # Create the underlying TextBox, but keep it hidden
            tb = TextBox()
            tb.Visible = False
            return tb
        iar = pyjama.TopLevelControl.BeginInvoke(Func[object](tmp))
        self.entry = pyjama.TopLevelControl.EndInvoke(iar)
        GraphicsObject.__init__(self)
        self.font = Drawing.Font("Helvetica", 12)
        self.anchor = p.clone()
        self.width = width
        self.setFill("lightgray")
        self.setTextColor("black")

    # - - - - - - - - - - - - - - - - -
    def draw(self, graphwin):

        """Draw the object in graphwin, which should be a GraphWin
        object.  A GraphicsObject may only be drawn into one
        window. Raises an error if attempt made to draw an object that
        is already visible."""
        
        if self.canvas and not self.canvas.isClosed():
            raise GraphicsError, OBJ_ALREADY_DRAWN
        if graphwin.isClosed():
            raise GraphicsError, "Can't draw to closed window"
        self.canvas = graphwin
        
        # Add to controls and draw
        self.canvas.append(self)
        def tmp(): 
            self.canvas.Controls.Add(self.entry)
            if self.canvas.autoflush:
                self.canvas.Invalidate()
        pyjama.TopLevelControl.Invoke(Func[object](tmp))
        

    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):

        # Set widget colors, font and size
        self.setFill(self.fill_color)
        self.entry.Font = self.font
        self.setTextColor(self.color)
        sizef = g.MeasureString("X"*self.width, self.font)
        self.entry.Size = Drawing.Size.Round(sizef)

        # Position the Widget ane make visible
        p = self.anchor
        x,y = self.canvas.toScreen(p.x,p.y)
        x,y = x - 0.5*sizef.Width, y - 0.5*sizef.Height
        self.entry.Location = Drawing.Point(x,y)
        self.entry.Visible = True

    # - - - - - - - - - - - - - - - - -
    def undraw(self):

        """Undraw the Entry Widget (i.e. hide it). Returns silently if the
        object is not currently drawn."""

        if not self.canvas: return
        if not self.canvas.isClosed():
            self.canvas.remove(self)
            def tmp():
                self.canvas.Controls.Remove(self.entry)
                if self.canvas.autoflush: self.canvas.Invalidate()
            pyjama.TopLevelControl.Invoke(Func[object](tmp))
        self.canvas = None

    # - - - - - - - - - - - - - - - - -
    def getText(self):
        def tmp(): return self.entry.Text
        iar = pyjama.TopLevelControl.BeginInvoke(Func[object](tmp))
        return pyjama.TopLevelControl.EndInvoke(iar)

    # - - - - - - - - - - - - - - - - -
    def _move(self, dx, dy):
        self.anchor.move(dx,dy)
        
    # - - - - - - - - - - - - - - - - -
    def getAnchor(self):
        return self.anchor.clone()

    # - - - - - - - - - - - - - - - - -
    def clone(self):
        other = Entry(self.anchor, self.width)
        other.entry.Text = self.entry.Text
        other.setFill(self.fill)
        return other
    
    # - - - - - - - - - - - - - - - - -
    def setText(self, t):
        def tmp(): self.entry.Text = t
        pyjama.TopLevelControl.Invoke(Func[object](tmp))
        
    # - - - - - - - - - - - - - - - - -
    def setFill(self, color):
        """Set interior color to color"""
        def tmp():
            if self.fill_color == color: return
            clr = _color_map[color]
            if clr == Drawing.Color.Transparent:
                clr = Drawing.Color.White
            self.entry.BackColor = clr
            self.fill_color = color
        pyjama.TopLevelControl.Invoke(Func[object](tmp))
    
    # - - - - - - - - - - - - - - - - -
    def setFace(self, face):
        try:
            fface = _font_face_map[face]
        except:
            raise GraphicsError, BAD_OPTION
        fsize = self.font.Size
        fstyle = self.font.Style
        self.font.Dispose()
        self.font = Drawing.Font(fface, fsize, fstyle)
        
    # - - - - - - - - - - - - - - - - -
    def setSize(self, size):
        if 5 <= size <= 36:
            fsize = float(size)
        else:
            raise GraphicsError, BAD_OPTION
        fface = self.font.FontFamily
        fstyle = self.font.Style
        self.font.Dispose()
        self.font = Drawing.Font(fface, fsize, fstyle)

    # - - - - - - - - - - - - - - - - -
    def setStyle(self, style):
        try:
            fstyle = _font_style_map[style]
        except:
            raise GraphicsError, BAD_OPTION
        fface = self.font.FontFamily
        fsize = self.font.Size
        self.font.Dispose()
        self.font = Drawing.Font(fface, fsize, fstyle)
        
    # - - - - - - - - - - - - - - - - -
    def setTextColor(self, color):
        self.color=color
        clr = _color_map[color]
        def tmp(): self.entry.ForeColor = clr
        pyjama.TopLevelControl.Invoke(Func[object](tmp))

# -----------------------------------------------
class Image(GraphicsObject):
    
    # - - - - - - - - - - - - - - - - -
    #def __init__(self, p, pixmap):
    def __init__(self, arg1, arg2=None):
        GraphicsObject.__init__(self)

        # Handle all constructor argument type combinations
        typ1, typ2 = type(arg1), type(arg2)

        # Image(Point, filepath)
        if typ1 == Point and typ2 == str:
            self.img = Drawing.Bitmap(arg2, True)
            w, h = self.img.Width, self.img.Height
            self.anchor = Point(arg1.getX()-w/2, arg1.getY()-h/2)
        # Image(Point, ImageToClone)
        elif typ1 == Point and typ2 == Image:
            self.img = arg2.img
            w, h = self.img.Width, self.img.Height
            self.anchor = Point(arg1.getX()-w/2, arg1.getY()-h/2)
        # Image(filepath)
        elif typ1 == str and typ2 == type(None):
            self.anchor = Point(0,0)
            self.img = Drawing.Bitmap(arg1, True)
        # Image(width, height)
        elif typ1 == int and typ2 == int:
            self.anchor = Point(0,0)
            self.img = Drawing.Bitmap(arg1, arg2)

    # - - - - - - - - - - - - - - - - -
    def _draw(self, g):
        canvas = self.canvas
        p = self.anchor
        x,y = canvas.toScreen(p.x,p.y)
        g.DrawImage(self.img, Drawing.Point(x,y))
    
    # - - - - - - - - - - - - - - - - -
    def _move(self, dx, dy):
        self.anchor.move(dx,dy)

    # - - - - - - - - - - - - - - - - -
    def getAnchor(self):
        return self.anchor.clone()
    
    # - - - - - - - - - - - - - - - - -
    def clone(self):
        imgCopy = Pixmap(self.img.Clone())
        other = Image(self.anchor, imgCopy)
        return other
    
    # - - - - - - - - - - - - - - - - -
    def getWidth(self):
        """Returns the width of the image in pixels"""
        def tmp(): return self.img.Width
        iar = pyjama.TopLevelControl.BeginInvoke(Func[object](tmp))
        return pyjama.TopLevelControl.EndInvoke(iar)

    # - - - - - - - - - - - - - - - - -
    def getHeight(self):
        """Returns the height of the image in pixels"""
        def tmp(): return self.img.Height
        iar = pyjama.TopLevelControl.BeginInvoke(Func[object](tmp))
        return pyjama.TopLevelControl.EndInvoke(iar)
    
    # - - - - - - - - - - - - - - - - -
    def getPixel(self, x, y):
        """Returns a list [r,g,b] with the RGB color values for pixel (x,y)
        r,g,b are in range(256)

        """
        def tmp():
            clr = self.img.GetPixel(x,y)
            return [clr.R, clr.G, clr.B]
        iar = pyjama.TopLevelControl.BeginInvoke(Func[object](tmp))
        return pyjama.TopLevelControl.EndInvoke(iar)

    # - - - - - - - - - - - - - - - - -
    def setPixel(self, x, y, (r,g,b)):
        """Sets pixel (x,y) to the color given by RGB values r, g, and b.
        r,g,b should be in range(256)
        """
        clr = color_rgb(r,g,b)
        def tmp(): self.img.SetPixel(x, y, clr)
        pyjama.TopLevelControl.Invoke(Func[object](tmp))
    
    # - - - - - - - - - - - - - - - - -
    def save(self, filename):
        """Saves the pixmap image to filename.
        The format for the save image is determined from the filname extension.
        """
        path, name = os.path.split(filename)
        ext = name.split(".")[-1]
        ext = ext.lower()
        if   ext == 'emf':  frmt = Drawing.Imaging.ImageFormat.Emf
        elif ext == 'gif':  frmt = Drawing.Imaging.ImageFormat.Gif
        elif ext == 'ico':  frmt = Drawing.Imaging.ImageFormat.Icon
        elif ext == 'jpg':  frmt = Drawing.Imaging.ImageFormat.Jpeg
        elif ext == 'jpeg': frmt = Drawing.Imaging.ImageFormat.Jpeg
        elif ext == 'png':  frmt = Drawing.Imaging.ImageFormat.Png
        elif ext == 'tif':  frmt = Drawing.Imaging.ImageFormat.Tiff
        elif ext == 'tiff': frmt = Drawing.Imaging.ImageFormat.Tiff
        elif ext == 'wmf':  frmt = Drawing.Imaging.ImageFormat.Wmf
        else:               frmt = Drawing.Imaging.ImageFormat.Bmp
        def tmp(): self.img.Save(filename, frmt)
        pyjama.TopLevelControl.Invoke(Func[object](tmp))

# -----------------------------------------------
def color_rgb(r,g,b,a=255):
    """r,g,b are intensities of red, green, and blue in range(256)
    a is the transparency of the color.
    Returns color object for the resulting color"""
    clr = Drawing.Color.FromArgb(a,r,g,b)
    return clr

# -----------------------------------------------
# For compatibility.
# The Pixmap and Bitmap objects are now merged:
Pixmap = Image
# Window is a more general name:
GraphWin = Window

# -----------------------------------------------
def test():
    win = GraphWin()
    win.setCoords(0,0,10,10)
    t = Text(Point(5,5), "Centered Text")
    t.draw(win)
    p = Polygon(Point(1,1), Point(5,3), Point(2,7))
    p.draw(win)
    e = Entry(Point(5,6), 10)
    e.draw(win)
    print win.getMouse()
    p.setFill("red")
    p.setOutline("blue")
    p.setWidth(2)
    s = ""
    for pt in p.getPoints():
        s = s + "(%0.1f,%0.1f) " % (pt.getX(), pt.getY())
    t.setText(e.getText())
    e.setFill("green")
    e.setText("Spam!")
    e.move(2,0)
    print win.getMouse()
    p.move(2,3)
    s = ""
    for pt in p.getPoints():
        s = s + "(%0.1f,%0.1f) " % (pt.getX(), pt.getY())
    t.setText(s)
    print win.getMouse()
    p.undraw()
    e.undraw()
    t.setStyle("bold")
    print win.getMouse()
    t.setStyle("normal")
    print win.getMouse()
    t.setStyle("italic")
    print win.getMouse()
    t.setStyle("bold italic")
    print win.getMouse()
    t.setSize(14)
    print win.getMouse()
    t.setFace("arial")
    t.setSize(20)
    return win

# -----------------------------------------------
if __name__ == "__main__":
    test()
