/*********************************************************************
 *
 * Copyright (c) 2008 Douglas S. Blank
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *********************************************************************/

// Based, in part, on John Zelle's Python Graphics Library.

using System;
using System.Collections;

using Gtk;
using Gdk;
using Gnome;

// This should come from the DLR, not Python:
using IronPython.Runtime;
// uses: List, ICodeFormattable
using Microsoft.Scripting.Runtime;
// uses: __repr__

public class Graphics {

  public static GraphWin _defaultWindow = null;
  public static Color black     = new Color(0,     0,   0);
  public static Color white     = new Color(255, 255, 255);
  public static Color blue      = new Color(  0,   0, 255);
  public static Color red       = new Color(255,   0,   0);
  public static Color green     = new Color(  0, 255,   0);
  public static Color gray      = new Color(128, 128, 128);
  public static Color darkGray  = new Color( 64,  64,  64);
  public static Color lightGray = new Color(192, 192, 192);
  public static Color yellow    = new Color(255, 255,   0);
  public static Color pink      = new Color(255, 175, 175);
  public static Color magenta   = new Color(255,   0, 255);
  public static Color cyan      = new Color(  0, 255, 255);
  
    public Graphics() {
	// Gtk.Application.Init()
    }
    
    public static void show() {
	if (_defaultWindow != null) {
	    _defaultWindow.update();
	}
    }
    
    public static void show(Image image) {
	if (_defaultWindow == null) {
	    _defaultWindow = new GraphWin();
	    image.draw(_defaultWindow);
	} else {
	    try {
		image.draw(_defaultWindow);
	    } catch {
		_defaultWindow = new GraphWin();
		image.draw(_defaultWindow);
	    }
	}
    }
    
    public static IEnumerator getPixels(Pixmap pixmap) {
	return pixmap.getPixels();
    }
    
    public static Pixel getPixel(Pixmap pixmap, int x, int y) {
	return pixmap.getPixel(x, y);
    }

    public static Color getColor(Pixel pixel) {
	return pixel.getColor();
    }
    public static Color getColor(Pixmap pixmap, int x, int y) {
	return pixmap.getColor(x, y);
    }
    public static int getGray(Pixel pixel) {
	return pixel.getGray();
    }
    public static int getGray(Color color) {
	return color.getGray();
    }
    public static int getGray(Pixmap pixmap, int x, int y) {
	return pixmap.getGray(x, y);
    }
    public static int getGray(List list) {
	return (((int)list[0]) + ((int)list[1]) + ((int)list[2])) / 3;
    }
    public static List getRGB(Pixel pixel) {
	return pixel.getRGB();
    }
    public static List getRGB(Color color) {
	return color.getRGB();
    }
    public static List getRGB(Pixmap pixmap, int x, int y) {
	return pixmap.getRGB(x, y);
    }

    public static void setColor(Color color1, Color color2) {
	color1.setColor(color2);
    }
    public static void setColor(Color color, List list) {
	color.setColor(list);
    }
    public static void setColor(Color color, int gray) {
	color.setColor(gray);
    }
    public static void setRGB(Color color1, Color color2) {
	color1.setRGB(color2);
    }
    public static void setRGB(Color color, List list) {
	color.setRGB(list);
    }
    public static void setRGB(Color color, int gray) {
	color.setRGB(gray);
    }
    
    public static Color makeColor(int red, int green, int blue) {
	return new Color(red, green, blue);
    }

    public class Color : ICodeFormattable {
	
	int red, green, blue;
	
	public Color(int red, int green, int blue) {
	    this.red = red;
	    this.green = green;
	    this.blue = blue;
	}
	
	public int getRed() { return this.red; }
	public int getGreen() { return this.green; }
	public int getBlue() { return this.blue; }
	public int getGray() { 
	    return (this.red + this.green + this.blue) / 3;
	}
	public List getRGB() { 
	    List list = new List();
	    list.append(this.red);
	    list.append(this.green);
	    list.append(this.blue);
	    return list;
	}
	
	public void setRed(int red) { this.red = red; }
	public void setGreen(int green) { this.green = green; }
	public void setBlue(int blue) { this.blue = blue; }
	public void setGray(int gray) { 
	    this.red = gray;
	    this.green = gray;
	    this.blue = gray;
	}
	
	public void setColor(Color color) {
	    red = color.getRed();
	    green = color.getGreen();
	    blue = color.getBlue();
	}
	
	public void setColor(List list) {
	    red = (int) list[0];
	    green = (int) list[2];
	    blue = (int) list[3];
	}
	
	public void setColor(int gray) {
	    red = gray;
	    green = gray;
	    blue = gray;
	}
	
	public void setRGB(List list) {
	    red = (int) list[0];
	    green = (int) list[1];
	    blue = (int) list[2];
	}
	
	public void setRGB(Color color) {
	    red = color.getRed();
	    green = color.getGreen();
	    blue = color.getBlue();
	}
	
	public void setRGB(int gray) {
	    red = gray;
	    green = gray;
	    blue = gray;
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Color (r={0},g={1},b={2})>",
				 red, green, blue);
        }
    }

    public static GraphWin makeGraphWin() {
	return new GraphWin();
    }
    
    public class GraphWin : ICodeFormattable {
	
	  public Gtk.Window window;
	public Gnome.Canvas canvas;
	Color background;
	
	public GraphWin(int width, int height): 
	    this("Graphics Window", width, height) {
	}
	public GraphWin(): this("Graphics Window", 200, 200) {
	}
	public GraphWin(string title): this(title, 200, 200) {
	}
	public GraphWin(string title, int width, int height) {
	    this.window = new Gtk.Window(title);
	    this.canvas = new Gnome.Canvas();
	    // expand, fill, padding
	    canvas.SetSizeRequest(width, height);
	    canvas.SetScrollRegion(0.0, 0.0, 
				   (double) width, (double) height);
	    window.Resize(width, height);
	    window.Add(canvas);
	    window.ShowAll();
	}

	public void plot(int x, int y) {
	    this.plot(x, y, black);
	}
	
	public void plot(int x, int y, Color color) {
	    /*
	      Draws the pixel at $(x,y)$ in the window. Color is
	      optional, black is the default. Note: pixel-level
	      operations are very inefficient and this method should
	      be avoided.
	    */ 
	}
	
	public void plotPixel(int x, int y, Color color) {
	    /*
	      Draws the pixel at the ``raw'' position $(x,y)$ ignoring
	      any coordinate transformations set up by
	      setCoords. Note: pixel- level operations are very
	      inefficient and this method should be avoided.
	    */ 
	}
	
	public void setBackground(Color color) {
	    /*
	      Sets the window background to the given color. The
	      initial background is gray. See Section 5.8.5 for
	      information on specifying colors.
	    */ 
	}
	
	public void close() {
	    /*
	      Closes the on-screen window. Once a window is closed,
	      further operations on the window will raise a
	      GraphicsError exception.
	    */ 
	}
	
	public void isClosed() {
	    /*
	      Returns a Boolean indicating if the window has been
	      closed either by an explicit call to close or a click on
	      its close box.
	    */
	}
	
	public void getMouse() {
	    /*
	      Pauses for the user to click in the window and returns
	      where the mouse was clicked as a Point object. Raises
	      GraphicsError if the window is closed while getMouse is
	      in progress.
	    */
	}
	
	public void setCoords(int xll, int yll, int xur, int yur) {
	    /*
	      Sets the coordinate system of the window. The lower left
	      corner is $(xll, yll)$ and the upper right corner is
	      $(xur, yur)$. All subsequent drawing will be done with
	      respect to the altered coordinate system (except for
	      plotPixel).
	    */
	}
	
	public void update() {
	    /*
	      Causes any pending window operations to be
	      performed. Normally, this will happen automatically
	      during idle periods. Explicit update() calls may be
	      useful for animations.
	    */
	    this.window.QueueDraw();
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<GraphWin object>");
        }
    }
    
    public class BaseGraphic {
	
	Color fillColor;
	Color outlineColor;
	int width;
	public GraphWin graphwin;
	
	public void setFill(Color color) {
	    /*Sets the interior of the object to the given color.*/
	    this.fillColor = color;
	}
	
	public void setOutline(Color color) {
	    /*Sets the outline of the object to the given color.*/ 
	    this.outlineColor = color;
	}
	
	public void setWidth(int pixels) {
	    /*
	      Sets the width of the outline of the object to this 
	      many pixels. (Does not work for Point.)
	    */
	    this.width = pixels;
	}
	
	public void draw(GraphWin aGraphWin) {
	    /*
	      Draws the object into the given GraphWin. An object may only 
	      be drawn in one window at a time.
	    */
	    this.graphwin = aGraphWin;
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
    
    public static Point makePoint(int x, int y) {
	return new Point(x, y);
    }

    public class Point: BaseGraphic, ICodeFormattable {
	
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

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Point at ({0},{1})>", x, y);
        }
    }
    
    public static Line makeLine(Point p1, Point p2) {
	return new Line(p1, p2);
    }

    public class Line: BaseGraphic, ICodeFormattable {
	
	Point point1, point2;
	Gnome.CanvasLine line;
	Gnome.CanvasPoints points;
	string arrowType;
	
	public Line(Point point1, Point point2) {
	    /*Constructs a line segment from point1 to point2. */
	    this.point1 = point1;
	    this.point2 = point2;
	    this.arrowType = "none";
	    this.graphwin = null;
	    this.line = null;
	}
	
	public void draw(GraphWin aGraphWin) {
	    this.graphwin = aGraphWin;
	    this.line = new Gnome.CanvasLine(this.graphwin.canvas.Root());
	    points = new Gnome.CanvasPoints(new double[]{
		    point1.x, point1.y, point2.x, point2.y});
	    this.line.Points = points;
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

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Line at ({0},{1}) to ({2},{3})>",
				 point1.x, point1.y, point2.x, point2.y);
        }
    }
    
    public class Circle: BaseGraphic, ICodeFormattable {
	Point point1, point2;
	Point centerPoint;
	int radius;

	public Circle(Point centerPoint, int radius) {
	    /*Constructs a circle with given center point and radius. */
	    this.centerPoint = centerPoint;
	    this.radius = radius;
	}
	
	public Point getCenter() {
	    /*Returns a clone of the center point of the circle. */
	    return centerPoint.clone();
	}
	
	public int getRadius() {
	    /*Returns the radius of the circle. */
	    return radius;
	}
	
	public Point getP1() {
	    /*
	      Returns a clone of the corresponding corner of the circle's
	      bounding box. These are opposite corner points of a square
	      that circumscribes the circle. 
	    */
	    return point1.clone();
	}
	
	public Point getP2() {
	    /*
	      Returns a clone of the corresponding corner of the circle's
	      bounding box. These are opposite corner points of a square
	      that circumscribes the circle. 
	    */
	    return point2.clone();
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Circle at ({0},{1}) radius={2}>",
				 centerPoint.x, centerPoint.y, radius);
        }
    }
    
    public class Rectangle : BaseGraphic, ICodeFormattable {
	Point point1, point2;
	public Rectangle(Point point1, Point point2) {
	    /*
	      Constructs a rectangle having opposite corners at point1 and
	      point2.
	    */
	    this.point1 = point1;
	    this.point2 = point2;
	}
	
	public Point getCenter() {
	    /*Returns a clone of the center point of the rectangle. */
	    return new Point((this.point1.x + this.point2.x)/2,
			     (this.point1.y + this.point2.y)/2);
	}
	
	public Point getP1() {
	    /*
	      Returns a clone of corner points originally used to construct
	      the rectangle.
	    */
	    return point1.clone();
	}
	
	public Point getP2() {
	    /*
	      Returns a clone of corner points originally used to construct
	      the rectangle. 
	    */
	    return point2.clone();
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Rectangle at ({0},{1}) to ({2},{3})>",
				 point1.x, point1.y, point2.x, point2.y);
        }
    }
    
    
    public class Oval: BaseGraphic, ICodeFormattable {

	Point point1, point2;
	
	public Oval(Point point1, Point point2) {
	    /*
	      Constructs an oval in the bounding box determined by point1
	      and point2.
	    */
	}
	
	public Point getCenter() {
	    /*
	      Returns a clone of the point at the center of the oval. 
	    */
	    return new Point((this.point1.x + this.point2.x)/2,
			     (this.point1.y + this.point2.y)/2);
	}
	
	public Point getP1() {
	    /*
	      Returns a clone of the corresponding point used to construct
	      the oval.
	    */
	    return this.point1.clone();
	}
	
	public Point getP2() {
	    /*
	      Returns a clone of the corresponding point used to construct
	      the oval.
	    */
	    return this.point2.clone();
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Oval at ({0},{1}) to ({2},{3})>", 
				 point1.x, point1.y, point2.x, point2.y);
        }
    }
    
    
    public class Polygon: BaseGraphic, ICodeFormattable {
	
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

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Polygon object>");
        }
    }
    
    public class Text : BaseGraphic, ICodeFormattable {
	
	Point anchorPoint;
	String str;

	public Text(Point anchorPoint, string s) {
	    /*
	      Constructs a text object that displays the given string
	      centered at anchorPoint. The text is displayed
	      horizontally. */
	    this.anchorPoint = anchorPoint; // Share!
	    this.str = s;
	}
	
	public void setText(string s) {
	    /*Sets the text of the object to string. */
	    str = s;
	}
	
	public string getText() {
	    /*Returns the current string. */
	    return str;
	}
	
	public Point getAnchor() {
	    /*Returns a clone of the anchor point. */
	    return anchorPoint; // Share!
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

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Text object>");
        }
    }
    
    public class Entry : BaseGraphic, ICodeFormattable {
	
	Point centerPoint;
	int width;

	public Entry(Point centerPoint, int width) {
	    /*
	      Constructs an Entry having the given center point and
	      width. The width is specified in number of characters of text
	      that can be displayed. 
	    */
	    this.centerPoint = centerPoint;
	    this.width = width;
	}
	
	public Point getCenter() {
	    /*Returns a clone of the point where the entry box is
	     * centered. */
	    return centerPoint.clone();
	}
	
	public void getText() {
	    /*Returns the string of text that is currently in the
	     * entry box. */
	}
	
	public void setText(string s) {
	    /*Sets the text in the entry box to the given string. */
	}
	
	public void setFace(string family) {
	    /*Changes the font face to the given family. Possible
	     * values are { 'helvetica', 'courier', 'times roman', and
	     * 'arial'. */
	}
	
	public void setSize(int point) {
	    /*Changes the font size to the given point size. Sizes
	     * from 5 to 36 points are legal. */
	}
	
	public void setStyle(string style) {
	    /*Changes font to the given style. Possible values are {
	     * 'normal', 'bold', 'italic', and 'bold italic'. */
	}
	
	public void setTextColor(Color color) {
	    /*Sets the color of the text to color*/
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Entry at ({0},{1}) width={2}>",
				 centerPoint.x, centerPoint.y, width);
        }
    }
    
    public static Image makeImage(Point centerPoint, Pixmap pixmap) {
	return new Image(centerPoint, pixmap);
    }

    public class Image : BaseGraphic, ICodeFormattable {
	
	Pixmap image;
	Point centerPoint;
	Gnome.CanvasPixbuf cpixbuf;
	
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
	
	public Point getCenter() {
	    /*Returns a clone of the point where the image is centered. */
	    return centerPoint.clone();
	}
	
	public void draw(GraphWin aGraphWin) {
	    this.graphwin = aGraphWin;
	    this.cpixbuf = new Gnome.CanvasPixbuf(this.graphwin.canvas.Root());
	    this.cpixbuf.Pixbuf = this.image.pixbuf;
	    this.cpixbuf.X = this.centerPoint.x;
	    this.cpixbuf.Y = this.centerPoint.y;
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Image object>");
        }
    }
    
    public static Pixel makePixel(int x, int y, Pixmap pixmap) {
	return new Pixel(x, y, pixmap);
    }

    public class Pixel : ICodeFormattable {
	
	int x, y;
	Pixmap pixmap;
	
	public Pixel(int x, int y, Pixmap pixmap) {
	    this.x = x;
	    this.y = y;
	    this.pixmap = pixmap;
	}
	
	public void setColor(Color color) {
	    pixmap.setPixel(x, y, color);
	}
	
	public void setColor(List list) {
	    pixmap.setPixel(x, y, list);
	}
	
	public void setColor(int gray) {
	    pixmap.setPixel(x, y, gray);
	}
	
	public void setRGB(List list) {
	    pixmap.setPixel(x, y, list);
	}
	
	public void setRGB(Color color) {
	    pixmap.setPixel(x, y, color);
	    
	}
	
	public void setRGB(int gray) {
	    pixmap.setPixel(x, y, gray);
	    
	}
	
	public Color getColor() {
	    return pixmap.getColor(x, y);
	}
	
	public List getRGB() {
	    return pixmap.getRGB(x, y);
	}
	
	public int getRed() {
	    return pixmap.getRed(x, y);
	}
	
	public int getGreen() {
	    return pixmap.getGreen(x, y);
	}
	
	public int getBlue() {
	    return pixmap.getBlue(x, y);
	}
	
	public int getGray() {
	    return pixmap.getGray(x, y);
	}
	
	public void setRed(int red) {
	    pixmap.setRed(x, y, red);
	}
	
	public void setGreen(int green) {
	    pixmap.setGreen(x, y, green);
	}
	
	public void setBlue(int blue) {
	    pixmap.setBlue(x, y, blue);
	}
	
	public void setGray(int gray) {
	    pixmap.setGray(x, y, gray);
	}
	
	public string __str__() {
	    return String.Format("<Pixel at ({0},{1})>", x, y);
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Pixel at ({0},{1})>", x, y);
        }
    }
    
    public static Pixmap makePixmap() {
	return new Pixmap();
    }

    public class Pixmap : BaseGraphic, ICodeFormattable {
	
	internal Gdk.Pixbuf pixbuf;
	int bytesPerPixel;
	
	public Pixmap() {
	    /*
	      Constructs a Pixmap from the image file, filename, given 
	      height and width. See Image for supported file types. 
	    */
	    // false is HasAlpha
	    this.pixbuf = new Gdk.Pixbuf(Gdk.Colorspace.Rgb,false,8,200,200);
	    bytesPerPixel = this.pixbuf.HasAlpha ? 4 : 3;
	    clear();
	}
	
	public Pixmap(string filename) {
	    this.pixbuf = new Gdk.Pixbuf(filename);
	    bytesPerPixel = this.pixbuf.HasAlpha ? 4 : 3;
	}
	
	public Pixmap(int w, int h) {
	    this.pixbuf = new Gdk.Pixbuf(Gdk.Colorspace.Rgb, false, 8, w, h);
	    bytesPerPixel = this.pixbuf.HasAlpha ? 4 : 3;
	    clear();
	}
	
	public void clear() {
	    clear(white);
	}
	
	public void clear(Color color) {
	    unsafe {
		int rowstride = pixbuf.Rowstride;
		int width = pixbuf.Width;
		int height = pixbuf.Height;
		byte *line = (byte *) pixbuf.Pixels;
		
		int r = color.getRed();
		int g = color.getGreen();
		int b = color.getBlue();
		
		for (int y = 0; y < height; y++){
		    for (int x = 0; x < width; x++){
			byte *rgb = &line[x * bytesPerPixel];
			*rgb++ = (byte) r;
			*rgb++ = (byte) g;
			*rgb++ = (byte) b;
		    }
		    line += rowstride;
		}
	    }
	}
	
	public void draw(GraphWin aGraphWin) {
	    //raise AttributeError("can't draw a pixmap; make an Image");
	}
	
	public int getWidth() {
	    /* Returns the width of the image in pixels. */
	    return this.pixbuf.Width;
	}
	
	public int getHeight() {
	    /* Returns the height of the image in pixels. */
	    return this.pixbuf.Height;
	}
	
	public IEnumerator getPixels() {
	    for (int y = 0; y < pixbuf.Height; y++) {
		for (int x = 0; x < pixbuf.Width; x++) {
		    yield return new Pixel(x, y, this);
		}
	    }
	}
	
	public Pixel getPixel(int x, int y) {
	    /*
	      Returns a triple (r,g,b) of the red, green, and blue 
	      intensities of the pixel at (x,y). Intensity values are 
	      in range(256). 
	    */
	    return new Pixel(x, y, this);
	}
	
	
	public Color getColor(int x, int y) {
	    /*
	      Returns a triple (r,g,b) of the red, green, and blue 
	      intensities of the pixel at (x,y). Intensity values are 
	      in range(256). 
	    */
	    byte r, g, b;
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		r = pixels[x * bytesPerPixel + y * this.pixbuf.Rowstride + 0];
		g = pixels[x * bytesPerPixel + y * this.pixbuf.Rowstride + 1];
		b = pixels[x * bytesPerPixel + y * this.pixbuf.Rowstride + 2];
	    }
	    return new Color((int)r, (int)g, (int)b);
	}
	
	public List getRGB(int x, int y) {
	    /*
	      Returns a triple (r,g,b) of the red, green, and blue 
	      intensities of the pixel at (x,y). Intensity values are 
	      in range(256). 
	    */
	    byte r, g, b;
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		r = pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
		g = pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
		b = pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
	    }
	    List list = new List();
	    list.append((int)r);
	    list.append((int)g);
	    list.append((int)b);
	    return list;
	}
	
	public void setPixel(int x, int y, int gray) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		byte *r = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
		byte *g = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
		byte *b = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
		*r = (byte) gray;
		*g = (byte) gray;
		*b = (byte) gray;
	    }
	}
	
	public void setPixel(int x, int y, List list) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		byte *r = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
		byte *g = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
		byte *b = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
		*r = (byte) list[0];
		*g = (byte) list[1];
		*b = (byte) list[2];
	    }
	}
	
	public void setPixel(int x, int y, Color color) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		byte *r = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
		byte *g = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
		byte *b = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
		*r = (byte) color.getRed();
		*g = (byte) color.getGreen();
		*b = (byte) color.getBlue();
	    }
	}
	
	public void setRed(int x, int y, int red) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		byte *r = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
		*r = (byte) red;
	    }
	}
	
	public void setGreen(int x, int y, int green) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		byte *g = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
		*g = (byte) green;
	    }
	}
	
	public void setBlue(int x, int y, int blue) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		byte *b = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
		*b = (byte) blue;
	    }
	}
	
	public void setGray(int x, int y, int gray) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		byte *r = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
		byte *g = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
		byte *b = &pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
		*r = (byte) gray;
		*g = (byte) gray;
		*b = (byte) gray;
	    }
	}
	
	public int getRed(int x, int y) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    int r;
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		r = (int) pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
	    }
	    return r;
	}
	
	public int getGreen(int x, int y) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    int g;
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		g = (int) pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
	    }
	    return g;
	}
	
	public int getBlue(int x, int y) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    int b;
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		b = (int) pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
	    }
	    return b;
	}
	
	public int getGray(int x, int y) {
	    /*
	      Color is a triple (r,g,b) representing a color for the pixel. 
	      Sets pixel at (x,y) to the given color. 
	    */
	    int r;
	    int g;
	    int b;
	    unsafe {
		byte *pixels = (byte *)this.pixbuf.Pixels;
		r = (int) pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 0];
		g = (int) pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 1];
		b = (int) pixels[x * bytesPerPixel + y * pixbuf.Rowstride + 2];
	    }
	    return ((r + g + b)/3);
	}
	
	public void save(string filename) {
	    /*
	      Saves the image in a file having the given name. The format 
	      for the file is determined by the extension on the filename 
	      (e.g. .ppm or .gif). 
	    */
	    // FIXME { get type from filename
	    this.pixbuf.Save(filename, "jpeg"); // or "png", "gif"
	}
	
	public Pixmap clone() {
	    /*Returns a copy of the Pixmap. */
	    Pixmap copy = new Pixmap();
	    copy.pixbuf = this.pixbuf.Copy(); // or Clone()
	    return copy;
	}

	public virtual string __repr__(CodeContext context) {
	    return String.Format("<Pixmap ({0},{1})>", 
				 pixbuf.Width, pixbuf.Height);
        }
    }
}