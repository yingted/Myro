// Based on John Zelle's Graphics Library.

using Gtk;
using Gdk;
using Gnome;

// Gtk.Application.Init()
//public Color black = new Color(0,0,0);
//public static Color white = Color(255,255,255);

namespace Graphics {

class Color {

    int red, green, blue;

    public Color(int red, int green, int blue) {
        this.red = red;
        this.green = green;
        this.blue = blue;
    }

    public int getRed() { return this.red; }
    public int getGreen() { return this.green; }
    public int getBlue() { return this.blue; }
}

class GraphWin {

    public Gtk.Window _window;
    public Gnome.Canvas _canvas;
    bool _autoflush;

    GraphWin(string title, int width, int height, bool autoflush) {
        this._window = new Gtk.Window(title);
        this._canvas = new Gnome.Canvas();
        this._window.Add(this._canvas);
        this._window.Resize(width, height);
        this._window.ShowAll();
        this._autoflush = autoflush;
    }

    public void plot(int x, int y, Color color) {
        /*
        Draws the pixel at $(x,y)$ in the window. Color is optional, 
        black is the default. Note: pixel-level operations are very 
        inefficient and this method should be avoided.
        */ 
    }

    public void plotPixel(int x, int y, Color color) {
        /*
	Draws the pixel at the ``raw'' position $(x,y)$ ignoring any 
        coordinate transformations set up by setCoords. Note: pixel-
        level operations are very inefficient and this method should 
        be avoided.
        */ 
    }

    public void setBackground(Color color) {
        /*
        Sets the window background to the given color. The initial 
        background is gray. See Section 5.8.5 for information on 
        specifying colors.
        */ 
    }

    public void close() {
        /*
        Closes the on-screen window. Once a window is closed, further 
        operations on the window will raise a GraphicsError exception.
        */ 
    }

    public void isClosed() {
        /*
        Returns a Boolean indicating if the window has been closed either
        by an explicit call to close or a click on its close box.
        */
    }
             
    public void getMouse() {
        /*
        Pauses for the user to click in the window and returns where the 
        mouse was clicked as a Point object. Raises GraphicsError if the 
        window is closed while getMouse is in progress. 
        */
    }

    public void setCoords(int xll, int yll, int xur, int yur) {
        /*
        Sets the coordinate system of the window. The lower left corner 
        is $(xll, yll)$ and the upper right corner is $(xur, yur)$. All 
        subsequent drawing will be done with respect to the altered 
        coordinate system (except for plotPixel). 
        */
    }
        
    public void update() {
        /*
        Causes any pending window operations to be performed. Normally, 
        this will happen automatically during idle periods. Explicit update() 
        calls may be useful for animations.
        */
    }
}

class BaseGraphic {            

    Color _fillColor;
    Color _outlineColor;
    int _width;
    public GraphWin _graphwin;

    public void setFill(Color color) {
        /*Sets the interior of the object to the given color.*/
        this._fillColor = color;
    }
        
    public void setOutline(Color color) {
        /*Sets the outline of the object to the given color.*/ 
	this._outlineColor = color;
    }

    public void setWidth(int pixels) {
        /*
        Sets the width of the outline of the object to this 
        many pixels. (Does not work for Point.)
        */
        this._width = pixels;
    }

    public void draw(GraphWin aGraphWin) {
        /*
        Draws the object into the given GraphWin. An object may only 
        be drawn in one window at a time.
        */
        this._graphwin = aGraphWin;
        // FIXME { add it to the canvas
        // win._window.Add(image) works fine
    }

    public void undraw() {
        /*
        Undraws the object from a graphics window. Returns silently if
        object is not drawn.*/ 
    }

    public void move(int dx, int dy) {
        /*
        Moves the object dx units in the $x$ direction and dy units in
        the $y$ direction. If the object is currently drawn, its image
        is adjusted to the new position.
        */ 
    }

    public void clone() {
        /*
        Returns a duplicate of the object. Clones are always created
        in an undrawn state. Other than that, they are identical to
        the cloned object.
        */ 
    }
}

class Point: BaseGraphic {

    public int x, y;

    public Point(int x, int y) {
        /*Constructs a point having the given coordinates.*/
        this.x = x;
        this.y = y;
    }
         
    public int getX() {
        /*Returns the $x$ coordinate of a point.*/
        return this.x;
    }
         
    public int getY() {
        /*Returns the $y$ coordinate of a point.*/
        return this.y;
    }
        
    public Point clone() {
        return new Point(this.x, this.y);
    }
}
        
class Line: BaseGraphic {

    Point point1, point2;
    Gnome.CanvasLine _line;
    string arrowType;

    public Line(Point point1, Point point2) {
        /*Constructs a line segment from point1 to point2. */
        this.point1 = point1;
        this.point2 = point2;
        this.arrowType = "none";
        this._graphwin = null;
        this._line = null;
    }

    public void draw(GraphWin aGraphWin) {
    	this._graphwin = aGraphWin;
    	this._line = new Gnome.CanvasLine(this._graphwin._canvas.Root());
	int [] points = new int[] {this.point1.x, this.point1.y, 
			 this.point2.x, this.point2.y};
        //this._line.Points = new Gnome.CanvasPoints( points );
    }

    public void setArrow(string arrowType) {
        /*
        Sets the arrowhead status of a line. Arrows may be drawn at
        either the first point, the last point, or both. Possible
        values of string are 'first', 'last', 'both', and 'none'. The
        default setting is 'none'. 
        */
        this.arrowType = arrowType;
    }

    public Point getCenter() {
        /*Returns a clone of the midpoint of the line segment. */
        return new Point((this.point1.x + this.point2.x)/2,
			 (this.point1.y + this.point2.y)/2);
    }
    public Line clone() {
        Line line = new Line(this.point1, this.point2);
	// FIXME: need to copy
        line.arrowType = this.arrowType;
	return line;
    }

    public Point getP1() {
        /*Returns a clone of the corresponding endpoint of the segment. */
	return this.point1.clone();
    }

    public Point getP2() {
        /*Returns a clone of the corresponding endpoint of the segment. */
	return this.point2.clone();
    }
}

class Circle: BaseGraphic {
    public Circle(Point centerPoint, int radius) {
        /*Constructs a circle with given center point and radius. */
    }

    public void getCenter() {
        /*Returns a clone of the center point of the circle. */
    }

    public void getRadius() {
        /*Returns the radius of the circle. */
    }

    public void getP1() {
        /*
        Returns a clone of the corresponding corner of the circle's
        bounding box. These are opposite corner points of a square
        that circumscribes the circle. 
        */
    }

    public void getP2() {
        /*
        Returns a clone of the corresponding corner of the circle's
        bounding box. These are opposite corner points of a square
        that circumscribes the circle. 
        */
    }
}

class Rectangle : BaseGraphic {
    public Rectangle(Point point1, Point point2) {
        /*
        Constructs a rectangle having opposite corners at point1 and
        point2.
        */
    }

    public void getCenter() {
        /*Returns a clone of the center point of the rectangle. */
    }

    public void getP1() {
        /*
        Returns a clone of corner points originally used to construct
        the rectangle.
        */
    }

    public void getP2() {
        /*
        Returns a clone of corner points originally used to construct
        the rectangle. 
        */
    }
}


class Oval: BaseGraphic {

    public Oval(Point point1, Point point2) {
        /*
        Constructs an oval in the bounding box determined by point1
        and point2.
        */
    }

    public void getCenter() {
        /*
        Returns a clone of the point at the center of the oval. 
        */
    }

    public void getP1() {
        /*
        Returns a clone of the corresponding point used to construct
        the oval.
        */
    }

    public void getP2() {
        /*
        Returns a clone of the corresponding point used to construct
        the oval.
        */
    }
}


class Polygon: BaseGraphic {

    public Polygon(Point points) {
        /*
        Constructs a polygon having the given points as vertices. Also
        accepts a single parameter that is a list of the vertices.
        */
    }

    public void getPoints() {
        /*
        Returns a list containing clones of the points used to
        construct the polygon.
        */
    }
}

class Text : BaseGraphic {

    public Text(Point anchorPoint, string s) {
        /*
        Constructs a text object that displays the given string
        centered at anchorPoint. The text is displayed
        horizontally. */
    }

    public void setText(string s) {
        /*Sets the text of the object to string. */
    }

    public void getText() {
        /*Returns the current string. */
    }

    public void getAnchor() {
        /*Returns a clone of the anchor point. */
    }

    public void setFace(string family) {
        /*
        Changes the font face to the given family. Possible values
        are { 'helvetica', 'courier', 'times roman', and 'arial'.
        */
    }

    public void setSize(int point) {
        /*
        Changes the font size to the given point size. Sizes from 5 to
        36 points are legal.
        */
    }

    public void setStyle(string style) {
        /*
        Changes font to the given style. Possible values are 'normal',
        'bold', 'italic', and 'bold italic'.
        */
    }

    public void setTextColor(Color color) {
        /*
        Sets the color of the text to color. Note { setFill has the
        same effect. 
        */
    }
}

class Entry : BaseGraphic {

    public Entry(Point centerPoint, int width) {
        /*
        Constructs an Entry having the given center point and
        width. The width is specified in number of characters of text
        that can be displayed. 
        */
    }

    public void getAnchor() {
        /*Returns a clone of the point where the entry box is centered. */
    }

    public void getText() {
        /*Returns the string of text that is currently in the entry box. */
    }

    public void setText(string s) {
        /*Sets the text in the entry box to the given string. */
    }

    public void setFace(string family) {
        /*Changes the font face to the given family. Possible values are { 'helvetica', 'courier', 'times roman', and 'arial'. */
    }

    public void setSize(int point) {
        /*Changes the font size to the given point size. Sizes from 5 to 36 points are legal. */
    }

    public void setStyle(string style) {
        /*Changes font to the given style. Possible values are { 'normal', 'bold', 'italic', and 'bold italic'. */
    }

    public void setTextColor(Color color) {
        /*Sets the color of the text to color*/
    }
}

class Image : BaseGraphic {

    Pixmap image;
    Point centerPoint;
    Gnome.CanvasPixbuf _cpixbuf;

    public Image(Point centerPoint, Pixmap image) {
        /*
        image is either the name of an image file, or a Pixmap 
        object (see next section). Constructs an image from contents 
        of the given file or pixmap, centered at the given center 
        point. Note { if image is a Pixmap, subsequent changes to the 
        Pixmap will be reflected in the drawn Image. */
        this.image = image;
        this.centerPoint = centerPoint;
    }

    public void getAnchor() {
        /*Returns a clone of the point where the image is centered. */
    }

    public void draw(GraphWin aGraphWin) {
        this._graphwin = aGraphWin;
        this._cpixbuf = new Gnome.CanvasPixbuf(this._graphwin._canvas.Root());
        this._cpixbuf.Pixbuf = this.image._pixbuf;
        this._cpixbuf.X = this.centerPoint.x;
        this._cpixbuf.Y = this.centerPoint.y;
    }
}

class Pixmap : BaseGraphic {

    Gdk.Image gdk_image;
    Gtk.Image gtk_image;
    public Gdk.Pixbuf _pixbuf;

    public Pixmap() {
        /*
        Constructs a Pixmap from the image file, filename, given 
        height and width. See Image for supported file types. 
        */
	// False is HasAlpha
	this._pixbuf = new Gdk.Pixbuf(Gdk.Colorspace.Rgb, false, 8, 200, 200);
	init();
    }

    public Pixmap(string filename) {
	this._pixbuf = new Gdk.Pixbuf(filename);
	init();
    }

    public Pixmap(int w, int h) {
	this._pixbuf = new Gdk.Pixbuf(Gdk.Colorspace.Rgb, false, 8, w, h);
	init();
    }

    public void init() {
	this.gtk_image = new Gtk.Image(this._pixbuf);
        this.gdk_image = new Gdk.Image(Gdk.ImageType.Normal, 
				       this.gtk_image.Visual, 
				       this._pixbuf.Width, 
				       this._pixbuf.Height);
    }               

    public void draw(GraphWin aGraphWin) {
        //raise AttributeError("can't draw a pixmap; make an Image");
    }

    public int getWidth() {
        /* Returns the width of the image in pixels. */
        return this._pixbuf.Width;
    }

    public int getHeight() {
        /* Returns the height of the image in pixels. */
        return this._pixbuf.Height;
    }
    
    public Color getPixel(int x, int y) {
        /*
	  Returns a triple (r,g,b) of the red, green, and blue 
	  intensities of the pixel at (x,y). Intensity values are 
	  in range(256). 
        */
        uint val = gdk_image.GetPixel(x,y);
        int red = (int)(val / (2 ^ 16));
        int green = (int) ((val - (red * (2 ^ 16))) / (2 ^ 8));
        int blue =  (int) (val - (red * (2 ^ 16)) - (green * (2 ^ 8)));
        return new Color(red, green, blue);
    }

    public void setPixel(int x, int y, Color color) {
        /*
        Color is a triple (r,g,b) representing a color for the pixel. 
        Sets pixel at (x,y) to the given color. 
        */
        int red = color.getRed()     * (2 ^ 16);
        int green = color.getGreen() * (2  ^ 8);
        int blue =  color.getBlue();
        this.gdk_image.PutPixel(x,y, (uint)(red + green + blue));
    }

    public void save(string filename) {
        /*
        Saves the image in a file having the given name. The format 
        for the file is determined by the extension on the filename 
        (e.g. .ppm or .gif). 
        */
        // FIXME { get type from filename
	this._pixbuf.Save(filename, "jpeg"); // or "png"
    }
		
    public Pixmap clone() {
        /*Returns a copy of the Pixmap. */
	// FIXME: copy this object, and lower levels
	Pixmap copy = new Pixmap();
        copy._pixbuf = this._pixbuf.Copy(); // or Clone()
        return copy;
    }
}
}