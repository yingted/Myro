"""
Based on John Zelle's Graphics Library.
"""

import clr
clr.AddReference("gtk-sharp")
clr.AddReference("gdk-sharp")
clr.AddReference("gnome-sharp")
clr.AddReference("System")
#clr.AddReference("glib-sharp")
#import GLib
import Gtk
import Gdk
import Gnome
import System
Gtk.Application.Init()

class Color(object):
    """
    Color object
    """
    def __init__(self, red=0, green=0, blue=0):
        self.red = red
        self.green = green
        self.blue = blue

    def getRed(self): return self.red
    def getGreen(self): return self.green
    def getBlue(self): return self.blue
        
black = Color(0,0,0)
white = Color(255,255,255)

class GraphWin(object):

    def __init__(self, title="Graphics Window", width=200, height=200, 
                 autoflush=False):
        self._window = Gtk.Window(title)
        # FIXME: add a canvas
        self._canvas = Gnome.Canvas()
        #self._drawingArea = Gtk.DrawingArea()
        #self._window.DeleteEvent += self.OnWinDelete
        #self._drawingArea.Realized += self.OnRealized
        #self._drawingArea.ExposeEvent += self.OnExposed
        #self._gc = Gdk.GC(self._drawingArea)
        self._hbox = Gtk.HBox()
        self._window.Add(self._canvas)
        self._window.Resize(width, height)
        self._window.ShowAll()
        self._autoflush = autoflush

    def OnWinDelete(self, widget, args):
        pass
        
    def OnRealized(self, widget, args):
        pass
        
    def OnExposed(self, widget, args):
        pass
	        
    def plot(self, x, y, color):
        """
        Draws the pixel at $(x,y)$ in the window. Color is optional, 
        black is the default. Note: pixel-level operations are very 
        inefficient and this method should be avoided.
        """ 

    def plotPixel(self, x, y, color):
        """
        Draws the pixel at the ``raw'' position $(x,y)$ ignoring any 
        coordinate transformations set up by setCoords. Note: pixel-
        level operations are very inefficient and this method should 
        be avoided.
        """ 

    def setBackground(self, color):
        """
        Sets the window background to the given color. The initial 
        background is gray. See Section 5.8.5 for information on 
        specifying colors.
        """ 

    def close(self):
        """
        Closes the on-screen window. Once a window is closed, further 
        operations on the window will raise a GraphicsError exception.
        """ 

    def isClosed(self):
        """
        Returns a Boolean indicating if the window has been closed either
        by an explicit call to close or a click on its close box.
        """
             
    def getMouse(self):
        """
        Pauses for the user to click in the window and returns where the 
        mouse was clicked as a Point object. Raises GraphicsError if the 
        window is closed while getMouse is in progress. 
        """

    def setCoords(self, xll, yll, xur, yur):
        """
        Sets the coordinate system of the window. The lower left corner 
        is $(xll, yll)$ and the upper right corner is $(xur, yur)$. All 
        subsequent drawing will be done with respect to the altered 
        coordinate system (except for plotPixel). 
        """
        
    def update(self):
        """
        Causes any pending window operations to be performed. Normally, 
        this will happen automatically during idle periods. Explicit update() 
        calls may be useful for animations.
        """

class BaseGraphic(object):            
    def setFill(self, color):
        """Sets the interior of the object to the given color."""
        self.fillColor = color 
        
    def setOutline(self, color):
        """Sets the outline of the object to the given color.""" 
	self.outlineColor = color

    def setWidth(self, pixels):
        """
        Sets the width of the outline of the object to this 
        many pixels. (Does not work for Point.)
        """
        self.width = pixels 

    def draw(self, aGraphWin):
        """
        Draws the object into the given GraphWin. An object may only 
        be drawn in one window at a time.
        """
        self.window = aGraphWin 
        # FIXME: add it to the canvas
        # win._window.Add(image) works fine

    def undraw(self):
        """
        Undraws the object from a graphics window. Returns silently if
        object is not drawn.""" 

    def move(self, dx,dy):
        """
        Moves the object dx units in the $x$ direction and dy units in
        the $y$ direction. If the object is currently drawn, its image
        is adjusted to the new position.
        """ 

    def clone(self):
        """
        Returns a duplicate of the object. Clones are always created
        in an undrawn state. Other than that, they are identical to
        the cloned object.
        """ 

class Point(BaseGraphic):
    def __init__(self,x,y):
        """Constructs a point having the given coordinates."""
        self.x = x
        self.y = y
         
    def getX(self):
        """Returns the $x$ coordinate of a point."""
        return self.x
         
    def getY(self):
        """Returns the $y$ coordinate of a point."""
        return self.y
        
    def clone(self):
        return Point(self.x, self.y)
        
class Line(BaseGraphic):

    def __init__(self, point1, point2):
        """Constructs a line segment from point1 to point2. """
        self.point1 = point1
        self.point2 = point2
        self.arrowType = 'none'
        self._graphwin = None
        self._line = None 

    def draw(self, aGraphWin):
    	self._graphwin = aGraphWin
    	self._line = Gnome.CanvasLine(self._graphwin._canvas.Root())
        self._line.Points = Gnome.CanvasPoints(System.Array[System.Double]([self.point1.x, self.point1.y, 
                                                                            self.point2.x, self.point2.y]))

    def setArrow(self, arrowType):
        """
        Sets the arrowhead status of a line. Arrows may be drawn at
        either the first point, the last point, or both. Possible
        values of string are 'first', 'last', 'both', and 'none'. The
        default setting is 'none'. 
        """
        self.arrowType = arrowType

    def getCenter(self):
        """Returns a clone of the midpoint of the line segment. """
        return Point(int((self.point1.x + self.point2.x)/2),
                     int((self.point1.y + self.point2.y)/2))

    def clone(self):
        line = Line(self.point1.x, self.point1.y,
                    self.point2.x, self.point2.y)
        line.arrowType = self.arrowType
        return line

    def getP1(self):
        """Returns a clone of the corresponding endpoint of the segment. """
        return self.point1.clone()

    def getP2(self):
        """Returns a clone of the corresponding endpoint of the segment. """
        return self.point2.clone()

class Circle(BaseGraphic):
    def __init__(self, centerPoint, radius):
        """Constructs a circle with given center point and radius. """

    def getCenter(self):
        """Returns a clone of the center point of the circle. """

    def getRadius(self):
        """Returns the radius of the circle. """

    def getP1(self):
        """
        Returns a clone of the corresponding corner of the circle's
        bounding box. These are opposite corner points of a square
        that circumscribes the circle. 
        """

    def getP2(self):
        """
        Returns a clone of the corresponding corner of the circle's
        bounding box. These are opposite corner points of a square
        that circumscribes the circle. 
        """

class Rectangle(BaseGraphic):
    def __init__(self, point1, point2):
        """
        Constructs a rectangle having opposite corners at point1 and
        point2.
        """

    def getCenter(self):
        """Returns a clone of the center point of the rectangle. """

    def getP1(self):
        """
        Returns a clone of corner points originally used to construct
        the rectangle.
        """

    def getP2(self):
        """
        Returns a clone of corner points originally used to construct
        the rectangle. 
        """


class Oval(BaseGraphic):

    def __init__(self, point1, point2):
        """
        Constructs an oval in the bounding box determined by point1
        and point2.
        """

    def getCenter(self):
        """
        Returns a clone of the point at the center of the oval. 
        """

    def getP1(self):
        """
        Returns a clone of the corresponding point used to construct
        the oval.
        """

    def getP2(self):
        """
        Returns a clone of the corresponding point used to construct
        the oval.
        """


class Polygon():

    def __init__(self, *points):
        """
        Constructs a polygon having the given points as vertices. Also
        accepts a single parameter that is a list of the vertices.
        """

    def getPoints(self):
        """
        Returns a list containing clones of the points used to
        construct the polygon.
        """


class Text(BaseGraphic):

    def __init__(self, anchorPoint, string):
        """
        Constructs a text object that displays the given string
        centered at anchorPoint. The text is displayed
        horizontally. """

    def setText(self, string):
        """Sets the text of the object to string. """

    def getText(self):
        """Returns the current string. """

    def getAnchor(self):
        """Returns a clone of the anchor point. """

    def setFace(self, family):
        """
        Changes the font face to the given family. Possible values
        are: 'helvetica', 'courier', 'times roman', and 'arial'.
        """

    def setSize(self, point):
        """
        Changes the font size to the given point size. Sizes from 5 to
        36 points are legal.
        """

    def setStyle(self, style):
        """
        Changes font to the given style. Possible values are 'normal',
        'bold', 'italic', and 'bold italic'.
        """

    def setTextColor(self, color):
        """
        Sets the color of the text to color. Note: setFill has the
        same effect. 
        """

class Entry(BaseGraphic):

    def __init__(self, centerPoint, width):
        """
        Constructs an Entry having the given center point and
        width. The width is specified in number of characters of text
        that can be displayed. 
        """

    def getAnchor(self):
        """Returns a clone of the point where the entry box is centered. """

    def getText(self):
        """Returns the string of text that is currently in the entry box. """

    def setText(self, string):
        """Sets the text in the entry box to the given string. """

    def setFace(self, family):
        """Changes the font face to the given family. Possible values are: 'helvetica', 'courier', 'times roman', and 'arial'. """

    def setSize(self, point):
        """Changes the font size to the given point size. Sizes from 5 to 36 points are legal. """

    def setStyle(self, style):
        """Changes font to the given style. Possible values are: 'normal', 'bold', 'italic', and 'bold italic'. """

    def setTextColor(self, color):
        """Sets the color of the text to color"""

class Image(BaseGraphic):

    def __init__(self, centerPoint, image):
        """
        image is either the name of an image file, or a Pixmap 
        object (see next section). Constructs an image from contents 
        of the given file or pixmap, centered at the given center 
        point. Note: if image is a Pixmap, subsequent changes to the 
        Pixmap will be reflected in the drawn Image. """
        self.image = image
        self.centerPoint = centerPoint

    def getAnchor(self):
        """Returns a clone of the point where the image is centered. """

    def draw(self, aGraphWin):
        self._graphwin = aGraphWin
        self._cpixbuf = Gnome.CanvasPixbuf(self._graphwin._canvas.Root())
        self._cpixbuf.Pixbuf = self.image._pixbuf
        self._cpixbuf.X = self.centerPoint.x
        self._cpixbuf.Y = self.centerPoint.y

class Pixmap(BaseGraphic):

    def __init__(self, *args):
        """
        Constructs a Pixmap from the image file, filename, given 
        height and width. See Image for supported file types. 
        """
        if len(args) == 0: # default size
            # False is HasAlpha
            self._pixbuf = Gdk.Pixbuf(Gdk.Colorspace.Rgb, False, 8, 200, 200)
        elif len(args) == 1: # string, filename
            self._pixbuf = Gdk.Pixbuf(args[0])
        elif len(args) == 2: # width, height
            self._pixbuf = Gdk.Pixbuf(Gdk.Colorspace.Rgb, False, 8, 
                                     args[0], args[1])
        self.gtk_image = Gtk.Image(self._pixbuf)
        self.gdk_image = Gdk.Image(Gdk.ImageType.Normal, 
                                   self.gtk_image.Visual, 
                                   self._pixbuf.Width, 
                                   self._pixbuf.Height) 
                                   
    def draw(self, aGraphWin):
        raise AttributeError("can't draw a pixmap; make an Image")

    def getWidth(self):
        """ Returns the width of the image in pixels. """
        return self._pixbuf.Width

    def getHeight(self):
        """ Returns the height of the image in pixels. """
        return self._pixbuf.Height
    
    def getPixel(self, x,y):
        """
        Returns a triple (r,g,b) of the red, green, and blue 
        intensities of the pixel at (x,y). Intensity values are 
        in range(256). 
        """
        uint = long(self.gdk_image.GetPixel(x,y))
        red = int(uint / (2 ** 16))
        green = int((uint - (red * (2 ** 16))) / (2 ** 8))
        blue =  int(uint - (red * (2 ** 16)) - (green * (2 ** 8)))
        return (red, green, blue)

    def setPixel(self, x, y, color):
        """
        Color is a triple (r,g,b) representing a color for the pixel. 
        Sets pixel at (x,y) to the given color. 
        """
        red = color.getRed()     * (2 ** 16)
        green = color.getGreen() * (2  ** 8)
        blue =  color.getBlue()
        self.gdk_image.PutPixel(x,y, long(red + green + blue))
    
    def save(self, filename):
        """
        Saves the image in a file having the given name. The format 
        for the file is determined by the extension on the filename 
        (e.g. .ppm or .gif). 
        """
        # FIXME: get type from filename
        self._pixbuf.Save(filename, "jpeg") # or "png"
    
    def clone(self):
        """Returns a copy of the Pixmap. """
        # FIXME: copy this object, and lower levels
        copy = Pixmap()
        copy.pixbuf = self.pixbuf.Copy() # or Clone()
        return copy

