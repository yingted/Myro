/* Graphics Module for IronPython/IronRuby
 * Translated graphics module by Mark F. Russo and Doug Blank
 * Translated by Michelle Beard 07/01/2010
 * Inspired graphics module by John Zelle of Wartburg College
 * http://mcsp.wartburg.edu/zelle/pythoh
*/

using System;
using System.Windows.Forms;
using System.Drawing;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using System.IO;

public class PJGraphics
{
    private static AutoResetEvent _are = null;
    private static Form _tlc = null;

    public static void _ip_thread() {
	try {
	    if (_tlc == null) {
		// Create the dummy control, and show then hide it to get
		// Windows Forms to initialize it.
		_tlc = new Form(); //new System.Drawing.Size(0,0));
		_tlc.Show();
		_tlc.Hide();
	    }
	    // Signal that the thread running _ip_thread is ready for
	    // the main thread to send input to it.
	    _are.Set();
	    Application.Run();
	} catch (Exception e) {
	    System.Console.WriteLine("Cannot start pjgraphics thread");
	}
    }

    public static void init() {
	_are = new AutoResetEvent(false);
	Thread thread = new Thread(new ThreadStart(_ip_thread));
	thread.ApartmentState = ApartmentState.STA;
	thread.IsBackground = true;
	thread.Start();
	
	_are.WaitOne();
	//_ip_thread();
	// Interpeter exit should clean up and terminate thread.
	//sys.exitfunc = _ip_shutdown;
    }

    #region MapColors
    /// <summary>
    /// Map color strings to internal color objects.
    /// This can be extended with many additional colors.
    /// </summary>
    public class MapColors
    {
        Dictionary<string, Color> _color_map = new Dictionary<string, Color>()
            {
                {"black", Color.Black},
                {"lightgray", Color.LightGray},
                {"gray", Color.Gray},
                {"darkgray", Color.DarkGray},
                {"darkgrey", Color.DarkGray},
                {"slategray", Color.SlateGray},
                {"white", Color.White},
                {"red", Color.Red},
                {"green", Color.Green},
                {"green2", Color.FromArgb(255, 0, 238, 0)},
                {"green3", Color.FromArgb(255, 0, 205, 0)},
                {"blue", Color.Blue},
                {"yellow", Color.Yellow},
                {"yellow2", Color.FromArgb(255, 238, 238, 0)},
                {"cyan", Color.Cyan},
                {"peachpuff", Color.PeachPuff},
                {"transparent", Color.Transparent},
                {"", Color.Transparent}
            };

        Dictionary<string, string> _font_face_map = new Dictionary<string, string>()
        {
            {"helvetica", "Helvetica"},
            {"arial", "Arial"},
            {"courier", "Courier"},
            {"times roman", "Times New Roman"}
        };

        Dictionary<string, FontStyle> _font_style_map = new Dictionary<string, FontStyle>()
        {
            {"normal", FontStyle.Regular},
            {"bold", FontStyle.Bold},
            {"italic", FontStyle.Italic},
            {"bold italic", FontStyle.Bold | FontStyle.Italic},
            {"strikeout", FontStyle.Strikeout},
            {"underline", FontStyle.Underline}
        };

        public MapColors()
        {

        }

        public Dictionary<string, Color> color_map()
        {
            return _color_map;
        }

        public Dictionary<string, string> font_face_map()
        {
            return _font_face_map;
        }

        public Dictionary<string, FontStyle> font_style_map()
        {
            return _font_style_map;
        }
    }
    #endregion

    #region Graphics Error
    /// <summary>
    /// Generic error class for graphics module exceptions.
    /// </summary>
    class GraphicsError : System.Exception
    {
        string BAD_OPTION = "Illegal option value";
        string OBJ_ALREADY_DRAWN = "Object currently drawn";
        string UNSUPPORTED_METHOD = "Object doesn't support operation";
        string DEAD_THREAD = "Graphics thread quit unexpectedly";
    }
    #endregion  

    #region GraphWin
    /// <summary>
    /// Window class
    /// </summary>
    public class GraphWin : Form
    {

        string _title = "Graphics Window";
        int _height = 200;
        int _width = 200;
        bool _autoflush;
        public bool _smooth;
        bool _closed;
        object trans;
        ArrayList items;
        int? mouseX;
        int? mouseY;
        bool? _mouseCallBack;
        
        public int height
        {
            get { return _height; }
            set { _height = value; }
        }

        public int width
        {
            get { return _width; }
            set { _width = value; }
        }

        public string title
        {
            get { return _title; }
            set { _title = value; }
        }

        public bool autoflush
        {
            get { return _autoflush; }
            set { _autoflush = value; }
        }

        private bool smooth
        {
            get { return _smooth; }
        }
       
        public GraphWin()
        {

            InitializeComponent();
            _tlc.Invoke(null);
        }

        public GraphWin(string title)
        {
            this._title = title;
            InitializeComponent();
        }

        public GraphWin(string title, int height)
        {
            this._title = title;
            this._height = height;
            InitializeComponent();
        }

        public GraphWin(string title, int height, int width)
        {
            this._title = title;
            this._height = height;
            this._width = width;
            InitializeComponent();
        }

        public GraphWin(int height, int width)
        {
            this._height = height;
            this._width = width;
            InitializeComponent();
        }

        public void InitializeComponent()
        {
            this._smooth = false;
            this._autoflush = true;
            this.items = new ArrayList();
            this.Text = this.title;

            // Add to the size of the window to account for borders
            int dwidth = this._width - this.ClientSize.Width;
            int dheight = this._height - this.ClientSize.Height;

            this.Size = new System.Drawing.Size(this._width + dwidth, this._height + dheight);
            this.TopMost = true;

            //Turn on Double Buffereing
            try
            {
                this.SetStyle(ControlStyles.DoubleBuffer |
                              ControlStyles.UserPaint |
                              ControlStyles.AllPaintingInWmPaint |
                              ControlStyles.UserPaint,
                true);
                this.UpdateStyles();
            }
            catch
            {
                MessageBox.Show("Mono bug: skipping double-buffer settings");
            }

            this._closed = false;
            this._smooth = smooth;

            this.mouseX = null;
            this.mouseY = null;
            this._mouseCallBack = null;

            this.Paint += this._onPaint;
            //this.Click += this._onClick;
            this.MouseClick += new MouseEventHandler(Window_MouseClick);
            this.FormClosing += this._onFormClosing;

            this.BackColor = System.Drawing.Color.FromArgb(255, 236, 233, 216);
            this.Show();
            // Refresh set to autoflush
            if (_autoflush) this.Invalidate();
        }

        void Window_MouseClick(object sender, MouseEventArgs e)
        {
            /// Save the location of all mouse clicks.
            /// Only called from remote thread.
            this.mouseX = e.X;
            this.mouseY = e.Y;
            //if (this._mouseCallBack)
              //  this._mouseCallBack(new Point(e.X, e.Y));
        }

        private void _onPaint(object sender, PaintEventArgs e)
        {
            if (this.smooth)
                e.Graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;

            foreach (GraphicsObject s in this.items)
            {
                s._draw(e.Graphics);
            }
        }

        private void _onFormClosing(object sender, EventArgs e)
        {
            this.close();
        }

        private void _checkOpen()
        {
            if (this._closed)
                throw new GraphicsError();
        }

        public void append(GraphicsObject itm)
        {
            this.items.Add(itm);
        }

        public void remove(GraphicsObject itm)
        {
            this.items.Remove(itm);
        }

        public void setBackground(string color)
        {
            /// Set background color of window
            this._checkOpen();
            MapColors map = new MapColors();
            this.BackColor = map.color_map()[color];
            this.__autoflush();
        }

        public void setCoords(int x1, int y1, int x2, int y2)
        {
            /// Set coordinates of window to run from (x1, y1) in the
            /// lower-left corner to (x2, y2) in the upper-right corner.
            trans = new Transform(this.width, this.height, x1, y1, x2, y2);
        }

        public void close()
        {
            /// Close this GraphWin window
            if (this._closed) return;
            this._close_help();
        }

        private void _close_help()
        {
            /// Close the window
            this._closed = true;
            this.Close();
        }

        internal bool isClosed()
        {
            /// Return True of this GraphWin is closed
            return this._closed;
        }

        public void plot(int x, int y, string clr)
        {
            /// Set pixel (x, y) to the given color
            this._checkOpen();
            double[] screen = this.toScreen(x, y);
            Point pt = new Point(screen[0], screen[1]);
            pt.setOutline(clr);
            pt.draw(this);
            this.__autoflush();

        }

        public void plotPixel(int x, int y, string clr)
        {
            /// Set pixel raw (independent of window coordinates)
            /// pixel (x, y) to color
            Point pt = new Point(x, y);
            pt.setOutline(clr);
            pt.draw(this);
            this.__autoflush();
        }
        
        private void __autoflush()
        {
            if (this._autoflush)
                this.Invalidate();
            this.Invalidate();
        }

        internal void flush()
        {
            this._checkOpen();
        }

        internal void update()
        {
            this.flush();
        }

        public int getHeight()
        {
            return this.height;
        }

        public int getWidth()
        {
            return this.width;
        }

        public Point getMouse()
        {
            /// Wait for mouse click and return Point object
            /// representing the click
            this.update();
            while (this.mouseX == null || this.mouseY == null)
            {
                if (this.isClosed())
                    Console.WriteLine("Graphics Error");
                Thread.Sleep(100);
            }
            float[] coords = this.toWorld(this.mouseX, this.mouseY);
            this.mouseX = null;
            this.mouseY = null;
            return new Point(coords[0], coords[1]);
        }

        public Point checkMouse()
        {
            if (this.isClosed())
                //Console.WriteLine("Check mouse in closed window");
                throw new GraphicsError();
            if (this.mouseX != null & this.mouseY != null)
            {
                float[] coords = this.toWorld(this.mouseX, this.mouseY);
                this.mouseX = null;
                this.mouseY = null;
                return new Point(coords[0], coords[1]);
            }
            else
                return null;
        }

        internal double[] toScreen(double x, double y)
        {
            /// Convert x,y to screen coordinates
            double[] val = new double[] { x, y };
            return val;
        }

        protected internal float[] toWorld(int? x, int? y)
        {
            /// Convert x,y to world coordinates
            float[] val = new float[2] { (float)x, (float)y };
            return val;
        }
    }
    #endregion

    #region Transform
    /// <summary>
    /// Internal class for 2-D coordinate transformations
    /// </summary>
    class Transform : Object
    {
        int xbase;
        int ybase;
        double xscale;
        double yscale;

        public Transform(int w, int h, int xlow, int ylow, int xhigh, int yhigh)
        {
            int xspan = (xhigh - xlow);
            int yspan = (yhigh - ylow);
            this.xbase = xlow;
            this.ybase = ylow;
            this.xscale = xspan / (w - 1);
            this.yscale = yspan / (h - 1);
        }

        private int[] screen(int x, int y)
        {
            double xs = (x - this.xbase) / this.xscale;
            double ys = (y - this.ybase) / this.yscale;
            int[] value = new int[2] { (int)(xs + 0.5), (int)(ys + 0.5) };
            return value;
        }

        private double[] world(int xs, int ys)
        {
            double x = xs * this.xscale + this.xbase;
            double y = ys * this.yscale + this.ybase;
            double[] value = new double[2] { x, y };
            return value;
        }
    }
    #endregion

    #region GraphicsObject
    /// <summary>
    /// Generic base class for all the drawable objects
    /// </summary>
    public class GraphicsObject : Object
    {
        string _fill_color;
        string outline_color;
        int outline_width = 1;
        Brush _brush;
        Pen _pen;
        GraphWin _canvas; //probably can't draw here for some reason
        Font _font;

        public string fill_color
        {
            get { return _fill_color; }
            set { _fill_color = value; }
        }

        public Pen pen
        {
            get { return _pen; }
            set { _pen = value; }
        }

        public Brush brush
        {
            get { return _brush; }
            set { _brush = value; }
        }

        public Font font
        {
            get { return _font; }
            set { _font = value; }
        }

        public GraphWin canvas
        {
            get { return _canvas; }
            set { _canvas = value; }
        }

        public GraphicsObject()
        {
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            // When an object is drawn, canvas is set to the GraphWin (canvas)
            // object where it is drawn.
            this.canvas = null;
            this._pen = null;
            this._brush = null;

            // Keep copies of properties
            this.fill_color = null;
            this.outline_color = null;
            this.outline_width = 1;

            this.setOutline("black");
            this.setFill("transparent");
        }

        public virtual void setFill(string clr)
        {
             /// Set interior color to color
            if (this.fill_color == clr) return;
            MapColors map_colors = new MapColors();
            this._brush = new SolidBrush(map_colors.color_map()[clr]);
            this.fill_color = clr;
        }

        public virtual void setOutline(string clr)
        {
            /// Set outline color to color
            if (this.outline_color == clr) return;
            MapColors map_colors = new MapColors();
            Color color = map_colors.color_map()[clr];
            this._pen = new Pen(color, this.outline_width);
            this.outline_color = clr;
        }

        public virtual void setWidth(int width)
        {
            /// Set line weight to width
            if (this.outline_width == width) return;
            MapColors map_colors = new MapColors();
            Color clr = map_colors.color_map()[this.outline_color];
            this._pen = new Pen(clr, width);
        }

        public virtual void draw(GraphWin graphWin)
        {
            /// Draw the object in graphwin, which should be GraphWin
            /// object. A GraphicsObject may only be drawn into one 
            /// window. Raise an error if attempt made to draw an object that 
            /// is all ready visible.
            //if (this.canvas.isClosed())

            this.canvas = graphWin;
            this.canvas.append(this);
            this.canvas.Invalidate();
        }

        public virtual void undraw()
        {
            /// Undraw the object (i.e. hide it). Returns silently if
            /// the object is not currently drawn.
            if (!this.canvas.isClosed())
                this.canvas.remove(this);

        }

        public void move(int dx, int dy)
        {
            /// Move object dx units in x direction and dy units 
            /// in y direction
            this._move(dx, dy);
        }

        public virtual void _draw(Graphics canvas)
        {
            //
        }

        public virtual void _move(int dx, int dy)
        {
            //
        }
    }
    #endregion

    #region Point
    /// <summary>
    /// Point Class
    /// </summary>
    public class Point : GraphicsObject
    {
        private double _x;
        private double _y;

        public double x
        {
            get { return _x; }
            set { _x = value; }
        }

        public double y
        {
            get { return _y; }
            set { _y = value; }
        }

        public Point(double x, double y) : base()
        /// Create a point on a canvas
        {
            this.setOutline("black");
            this._x = x;
            this._y = y;
        }

        public override string ToString()
        {
            return (String.Format("<Point at ({0},{1})>", this.x, this.y));
        }

        public override void _draw(Graphics g)
        {
            /// Use a rectangle fill to draw points
            double[] coords = this.canvas.toScreen(this.x, this.y);
            g.FillRectangle(this.brush, (int)coords[0], (int)coords[1], 1, 1);
        }   

        public override void _move(int dx, int dy)
        {
            this.x += dx;
            this.y += dy;
        }

        public Point clone()
        {
            //Point other = new Point(this._x, this._y);
            //other.pen = (Pen)this.pen.Clone();
            //other.brush = (Brush)this.brush.Clone();
            //return other;
            return new Point(this.x, this.y);
        }

        // Why do this when the user can just type p.x or p.y? 
        public double getX()
        {
            return this.x;
        }

        public double getY()
        {
            return this.y;
        }
    }
    #endregion

    #region BBox
    /// <summary>
    /// Internal base class for objects represented by
    /// bounding box (opposite corners.
    /// Line segement is a degenerate case.
    /// </summary>
    public class _BBox : GraphicsObject
    {
        Point _p1;
        Point _p2;

        public Point p1
        {
            get { return _p1; }
            set { _p1 = value; }
        }

        public Point p2
        {
            get { return _p2; }
            set { _p2 = value; }
        }

        public _BBox(Point p1, Point p2) : base()
        {
            GraphicsObject g = new GraphicsObject();
            this._p1 = p1.clone();
            this._p2 = p2.clone();
        }

        public override void _move(int dx, int dy)
        {
            this._p1.x += dx;
            this._p1.y += dy;
            this._p2.x += dx;
            this._p2.y += dy;
        }

        public Point getP1()
        {
            return this._p1.clone();
        }

        public Point getP2()
        {
            return this._p2.clone();
        }

        public Point getCenter()
        {
            return new Point((float)(_p1.x + _p2.x) / 2, (float)(_p1.y + _p2.y) / 2);
        }
    }
    #endregion

    #region Rectangle
    /// <summary>
    /// Rectangle class
    /// </summary>
    public class Rectangle : _BBox
    {
        
        public Rectangle(Point p1, Point p2) 
            : base(p1, p2)
        {
            this.p1 = p1;
            this.p2 = p2;
        }
               
        public override void _draw(Graphics g)
        {
            p1 = this.p1;
            p2 = this.p2;
            double[] c1 = this.canvas.toScreen(p1.x, p1.y);
            double[] c2 = this.canvas.toScreen(p2.x, p2.y);

            // Need to write swap function here
            if (c1[0] > c2[0])
            {
                double temp = c2[0];
                c2[0] = c1[0];
                c1[0] = temp;
            }

            if (c1[1] > c2[1])
            {
                double temp = c2[1];
                c2[1] = c1[1];
                c1[1] = temp;
            }

            int width = (int)c2[0] - (int)c1[0];
            int height = (int)c2[1] - (int)c1[0];
            g.DrawRectangle(this.pen, (int)c1[0], (int)c1[1], width, height);
            g.FillRectangle(this.brush, (int)c1[0], (int)c1[1], width, height);
        }

        public override string ToString()
        {
            return (String.Format("<Rectangle at ({0},{1})>", this.p1, this.p2));
        }

        public Rectangle clone()
        {
            Rectangle other = new Rectangle(this.p1, this.p2);
            other.pen = (Pen)this.pen.Clone();
            other.brush = (Brush)this.brush.Clone();
            return other;
        }
    }
    #endregion

    #region Oval
    /// <summary>
    /// Oval class
    /// </summary>
    public class Oval : _BBox
    {
        public Oval(Point p1, Point p2)
            : base(p1, p2)
        {
            this.p1 = p1;
            this.p2 = p2;
        }

        public Oval clone()
        {
            Oval other = new Oval(this.p1, this.p2);
            other.pen = (Pen) this.pen.Clone();
            other.brush = (Brush) this.brush.Clone();
            return other;
        }

        public override string ToString()
        {
            return String.Format("<Oval at ({0}, {1})>", this.p1, this.p2);
        }

        public override void _draw(Graphics g)
        {
            p1 = this.p1;
            p2 = this.p2;
            double[] c1 = this.canvas.toScreen(p1.x, p1.y);
            double[] c2 = this.canvas.toScreen(p2.x, p2.y);
            if (c1[0] > c2[0])
            {
                double temp = c2[0];
                c2[0] = c1[0];
                c1[0] = temp;
            }

            if (c1[1] > c2[1])
            {
                double temp = c2[1];
                c2[1] = c1[1];
                c1[1] = temp;
            }
            
            int width = (int)c2[0] - (int)c1[0];
            int height = (int)c2[1] - (int)c1[0];
            g.DrawEllipse(this.pen, (int)c1[0], (int)c1[1], width, height);
            g.FillEllipse(this.brush, (int)c1[0], (int)c1[1], width, height);
        }
    }
    #endregion
   
    #region Circle
    /// <summary>
    /// Circle class
    /// </summary>
    public class Circle : Oval
    {
        double _radius;

        public Circle(Point center, double radius) : 
            base (
            new Point(center.x - radius, center.y - radius), 
            new Point(center.x + radius, center.y + radius)
            )

        {
            this._radius = radius;        
        }

        public new Circle clone()
        {
            Circle other = new Circle(this.getCenter(), this._radius);
            other.pen = (Pen)this.pen.Clone();
            other.brush = (Brush)this.brush.Clone();
            return other;
        }

        public double getRadius()
        {
            return this._radius;
        }

    }
    #endregion
    
    #region Line
    /// <summary>
    /// Line class
    /// </summary>
    public class Line : _BBox
    {
        string _arrow;

        public Line(Point p1, Point p2)
            : base(p1, p2)
        {
            this._arrow = "none";
            this.setFill("black");
        }

        public override string ToString()
        {
            return (String.Format("<Line at ({0},{1}), ({2}, {3})>", this.p1.x, this.p1.y, this.p2.x, this.p2.y));
        }

        public Line clone()
        {
            Line other = new Line(this.p1, this.p2);
            this.pen = (Pen)this.pen.Clone();
            this.brush = (Brush)this.brush.Clone();
            return other;
        }

        public override void _draw(Graphics g)
        {
            p1 = this.p1;
            p2 = this.p2;
            double[] f = this.canvas.toScreen(p1.x, p1.y);
            double[] s = this.canvas.toScreen(p2.x, p2.y);
            g.DrawLine(this.pen, (float) f[0], (float) f[1], (float) s[0], (float) s[1]);

            // Draw arrows
            if (this._arrow == "first" || this._arrow == "both")
                this._draw_arrowhead(g, (int)s[0], (int)s[1], (int)f[0], (int)f[1]);
            if (this._arrow == "last" || this._arrow == "both")
                this._draw_arrowhead(g, (int)f[0], (int)f[1], (int)s[0], (int)s[1]);
        }

        private void _draw_arrowhead(Graphics g, int x1, int y1, int x2, int y2)
        {
            // Set length of arrow
            double arrlen = 10.0;
            // Set the half-angle made by the arrow point
            double angle = 0.4;
            // Calculate unit vector
            double xdiff = x2 - x1;
            double ydiff = y2 - y1;
            double arclen = Math.Sqrt(xdiff * xdiff + ydiff * ydiff);
            double xunit = xdiff / arclen;
            double yunit = ydiff / arclen;
            // Rotate the scaled unit vecotr by a half-angle in both directions
            double cospa = Math.Cos(angle);
            // Translate back to the arrow head point
            double sinpa = Math.Sin(angle);
            double arrowx2 = arrlen * (xunit * cospa + yunit * sinpa) + x1;
            double arrowy2 = arrlen * (yunit * cospa - xunit * sinpa) + y1;
            double cosma = Math.Cos(-angle);
            double sinma = Math.Sin(-angle);
            double arrowx3 = arrlen * (xunit * cospa + yunit * sinma) + x1;
            double arrowy3 = arrlen * (yunit * cosma - xunit * sinma) + y1;
            
            // Draw the arrow head
            System.Drawing.Point[] pts = new System.Drawing.Point[3] { new System.Drawing.Point(x1, y1), new System.Drawing.Point((int)arrowx2, (int)arrowy2), new System.Drawing.Point((int)arrowx3, (int)arrowy3) };
            g.DrawPolygon(this.pen, pts);
            g.FillPolygon(this.brush, pts);
        }

        public void setArrow(string option)
        {
            string[] options =  new string[4] {"first", "last", "both", "none" };
            int retVal = Array.BinarySearch(options, option);
            if (retVal == 0)
                Console.WriteLine("Option not found!");
            this._arrow = option;
        }
       
    }
    #endregion
    
    #region Polygon
    /// <summary>
    /// Polygon class
    /// </summary>
    public class Polygon : GraphicsObject
    {
        Point[] _Points;

        public Point[] Points
        {
            get { return _Points; }
            set { _Points = value; }
        }

        public Polygon(params Point[] points)
        {
            this._Points = points;
            for (int i = 0; i < points.Length; i++)
                this._Points[i] = points[i].clone();
        }
   
        public Polygon clone()
        {
            Polygon other = new Polygon(this._Points);
            other.pen = (Pen)this.pen.Clone();
            other.brush = (Brush)this.brush.Clone();
            return other;
        }

        public Point[] getPoints()
        {
            Point[] pClones = new Point[this._Points.Length];
            for (int i = 0; i < this._Points.Length; i++)
                pClones[i] = this.Points[i].clone();
            return pClones;
        }

        public override void _move(int dx, int dy)
        {
            foreach (Point p in this._Points)
                p.move(dx, dy);
        }

        public override void _draw(Graphics g)
        {
            double[] value;
            System.Drawing.Point[] pts = new System.Drawing.Point[this._Points.Length];
            for (int i = 0; i < this._Points.Length; i++)
            {
                value = canvas.toScreen((int)_Points[i].x, (int)_Points[i].y);
                pts[i] = new System.Drawing.Point((int)this._Points[i].x, (int)this._Points[i].y);
            }
            g.DrawPolygon(this.pen, pts);
            g.FillPolygon(this.brush, pts);
        }
    }
    #endregion

    #region Text
    /// <summary>
    /// Text class
    /// </summary>
    public class Text : GraphicsObject
    {
        Font font;
        Point anchor;
        string _text;
        Point _p;

        public Point p
        {
            get { return _p; }
            set { _p = value; }
        }

        public Text(Point p, string text) : base()
        {
            this._p = p;
            this._text = text;
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            this.setFill("black");
            this.setOutline("black");
            this.font = new Font("Helvetica", 12);
            this.anchor = p.clone();
        }

        public override void _draw(Graphics g)
        {
            this.p = this.anchor;
            double[] coords = this.canvas.toScreen((int)p.x, (int)p.y);
            // change to double
            StringFormat frmt = new StringFormat();
            frmt.Alignment = StringAlignment.Center;
            frmt.LineAlignment = StringAlignment.Center;
            g.DrawString(this._text, this.font, this.brush, (float)coords[0], (float)coords[1], frmt);
        }

        public override void _move(int dx, int dy)
        {
            this.anchor.move(dx, dy);
        }

        public Text clone()
        {
            Text other = new Text(this.anchor, this._text); // fix me
            other.pen = (Pen)this.pen.Clone();
            this.brush = (Brush)this.brush.Clone();
            this.font = (Font)this.font.Clone();
            return other;
        }
        public void setText(string text)
        {
            this._text = text;
        }

        public string getText()
        {
            return this._text;
        }

        public Point getAnchor()
        {
            return this.anchor.clone();
        }

        public void setFace(string face)
        {
            MapColors fap = new MapColors();
            string fface;
            try
            {
                fface = fap.font_face_map()[face];
            }
            catch
            {
                fface = "Courier";
                throw;
            }
            int fsize = (int)this.font.Size;
            FontStyle fstyle = this.font.Style;
            this.font = new Font(fface, fsize, fstyle);
        }

        public void setSize(int size)
        {
            float fsize;
            if (5 <= size & size <= 36)
                fsize = (float)size;
            else
                return;
            FontFamily fface = this.font.FontFamily;
            FontStyle fstyle = this.font.Style;
            this.font = new Font(fface, fsize, fstyle);
        }

        public void setStyle(string style)
        {
            FontStyle fstyle;
            MapColors fap = new MapColors();
            try
            {
                fstyle = fap.font_style_map()[style];
            }
            catch { fstyle = FontStyle.Regular; }
            FontFamily fface = this.font.FontFamily;
            float fsize = this.font.Size;
            this.font.Dispose();
            this.font = new Font(fface, fsize, fstyle);
        }

        public void setTextColor(string color)
        {
            this.setFill(color);
        }
    }
    #endregion

    #region Entry
    /// <summary>
    /// Entry class. 
    /// </summary>
    public class Entry : GraphicsObject
    {
        TextBox entry = new TextBox();
        Point anchor;
        int width;
        string color;
        Point _p;

        public Point p
        {
            get { return _p; }
            set { _p = value; }
        }

        public Entry(Point p1, int width) : base()
        {
            //this.entry = new TextBox();
            this.p = p1.clone();
            this.anchor = p1.clone();
            this.width = width;
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            //this.entry = invoke();
            this.font = new Font("Helvetica", 12);
            this.setFill("lightgray");
            this.setTextColor("black");
        }

        private TextBox invoke()
        {
            TextBox tb = new TextBox();
            tb.Visible = false;
            return tb;
        }

        public override void draw(GraphWin graphWin)
        {
            ///Draw the object in graphWin, which should be a GraphWin
            ///object. A GraphicsObject may only be drawn into one window.
            ///Raises an error if attempt made to draw an object that is allready
            ///visible.
            if (this.canvas !=null)
                Console.WriteLine("Object all ready drawn!");
                
            if (graphWin.isClosed())
                Console.WriteLine("Can't draw to closed window!");
            this.canvas = graphWin;

            // Add to controls and draw
            this.canvas.append(this);
            this.canvas.Controls.Add(this.entry);
            
            if (this.canvas.autoflush)
                this.canvas.Invalidate();
        }

        public override void _draw(Graphics g)
        {
            /// Set widget colors, font and size
            this.setFill(this.fill_color);
            this.entry.Font = this.font;
            this.setTextColor(this.color);
            string measure = new string('X', this.width);
            SizeF sizef = g.MeasureString(measure, this.font);
            this.entry.Size = Size.Round(sizef);
            
            /// Position the Widgit and make visible
            p = this.anchor;
            this.entry.Location = new System.Drawing.Point((int) p.x, (int) p.y);
            double[] c = this.canvas.toScreen(p.x, p.y);
            c[0] = c[0] - 0.5 * sizef.Width;
            c[1] = c[1] - 0.5 * sizef.Height;
            this.entry.Visible = true;
        }

        public override void undraw()
        {
            /// Undraw the Entry Widget. Returns silently if the 
            /// object is not currently drawn.
            if (!this.canvas.isClosed())
            {
                this.canvas.remove(this);
                this.canvas.Controls.Remove(this.entry);
                if (this.canvas.autoflush)
                    this.canvas.Invalidate();
            }
            this.canvas = null;
        }

        public string getText()
        {
            return this.entry.Text;
        }

        public override void _move(int dx, int dy)
        {
            this.anchor.move(dx, dy);
        }

        public Point getAnchor()
        {
            return this.anchor.clone();
        }

        public Entry clone()
        {
            Entry other = new Entry(this.anchor, this.width);
            other.entry.Text = this.entry.Text;
            return other;
        }

        public void setText(string text)
        {
            this.entry.Text = text;
        }

        public override void setFill(string color)
        {
            /// Set interior color to color
            if (this.fill_color == color)
                return;
            MapColors map = new MapColors();
            Color clr = map.color_map()[color];
            if (clr == Color.Transparent)
                this.entry.BackColor = Color.White;            
            else this.entry.BackColor = clr;
            this.fill_color = color;   
        }

        public void setFace(string face)
        {
            MapColors map = new MapColors();
            string fface = map.font_face_map()[face];
            float fsize = this.font.Size;
            FontStyle fstyle = this.font.Style;
            this.font.Dispose();
            this.font = new Font(fface, fsize, fstyle);
        }

        public void setStyle(string style)
        {
            MapColors map = new MapColors();
            FontStyle fstyle = map.font_style_map()[style];
            FontFamily fface = this.font.FontFamily;
            float fsize = this.font.Size;
            this.font.Dispose();
            this.font = new Font(fface, fsize, fstyle);
        }

        public void setTextColor(string color)
        {
            this.color = color;
            MapColors map = new MapColors();
            Color clr = map.color_map()[color];
            this.entry.ForeColor = clr;
        }
    }
    #endregion

    #region Image
    /// <summary>
    /// Image class
    /// </summary>
    public class Image : GraphicsObject
    {
        Point anchor;
        double w;
        double h;
        Bitmap img;

        /// <summary>
        /// Image(Point, filepath)
        /// </summary>
        public Image(Point p, string filepath)
        {
            this.img = new Bitmap(filepath, true);
            this.w = this.img.Width;
            this.h = this.img.Height;
            this.anchor = new Point(p.x - this.w / 2, p.y - h / 2);
        }

        /// <summary>
        /// Image(Point, ImageToClone)
        /// </summary>
        public Image(Point p, Image arg2)
        {
            this.img = arg2.img;
            this.w = this.img.Width;
            this.h = this.img.Height;
            this.anchor = new Point(p.x - this.w / 2, p.y - this.h / 2);
        }

        /// <summary>
        /// Image(filepath)
        /// </summary>
        public Image(string filepath)
        {
            this.anchor = new Point(0, 0);
            this.img = new Bitmap(filepath, true);
        }

        /// <summary>
        /// Image(width, height)
        /// </summary>
        public Image(int width, int height)
        {
            this.anchor = new Point(0, 0);
            this.img = new Bitmap(width, height);
        }

        public override void _draw(Graphics g)
        {
            canvas = this.canvas;
            Point p = this.anchor;
            double[] coords = this.canvas.toScreen(p.x, p.y);
            g.DrawImage(this.img, new System.Drawing.Point((int)coords[0], (int)coords[1]));
        }

        public override void _move(int dx, int dy)
        {
            this.anchor.move(dx, dy);
        }

        public Point getAnchor()
        {
            return this.anchor.clone();
        }

        public Image clone()
        {
            Image imgCopy = (Image) this.img.Clone();
            Image other = new Image(this.anchor, imgCopy);
            return other;
        }

        public int getWidth()
        {
            /// Returns the width of the image in pixels
            return this.img.Width;
        }

        public int getHeight()
        {
            /// Returns the height of the image in pixels
            return this.img.Height;
        }

        public double[] getPixel(int x, int y)
        {
            Color clr = this.img.GetPixel(x, y);
            double[] rgb = new double[3] { clr.R, clr.G, clr.B };
            return rgb;
        }

        public void setPixel(int x, int y, int[] rgb)
        {
            
            /// Sets pixel (x,y) to the color given by RBG
            /// values r, g, and b. 
            /// r, g, b should be in range(256)
            Color clr;
            if (rgb.Length == 3)
                clr = Color.FromArgb(255, rgb[0], rgb[1], rgb[2]);
            else
                clr = Color.FromArgb(255, Color.Black);
        }

        public void save(string filename)
        {
            /// Saves the pixmap image to filename.
            /// The format for the save image is determined from the 
            /// filename extension
            string ext = Path.GetExtension(filename);
            string NewExt = ext.ToLower();
            System.Drawing.Imaging.ImageFormat frmt;
            switch (NewExt)
            {
                case "emf":
                    frmt = System.Drawing.Imaging.ImageFormat.Emf;
                    break;
                case "gif":
                    frmt = System.Drawing.Imaging.ImageFormat.Gif;
                    break;
                case "ico":
                    frmt = System.Drawing.Imaging.ImageFormat.Icon;
                    break;
                case "jpg":
                    frmt = System.Drawing.Imaging.ImageFormat.Jpeg;
                    break;
                case "jpeg":
                    frmt = System.Drawing.Imaging.ImageFormat.Jpeg;
                    break;
                case "png":
                    frmt = System.Drawing.Imaging.ImageFormat.Png;
                    break;
                case "tiff":
                    frmt = System.Drawing.Imaging.ImageFormat.Tiff;
                    break;
                case "tif":
                    frmt = System.Drawing.Imaging.ImageFormat.Tiff;
                    break;
                case "wmf":
                    frmt = System.Drawing.Imaging.ImageFormat.Wmf;
                    break;
                default:
                    frmt = System.Drawing.Imaging.ImageFormat.Bmp;
                    break;
            }
            this.img.Save(filename, frmt);
        }
    }
    #endregion

    #region Test
    /// <summary>
    /// Testing Graphics Library
    /// </summary>
    public class Run
    {
        //[STAThread]
        public static void Main()
        {
            GraphWin win = new GraphWin("Hello World!", 500, 500);
            Entry e = new Entry(new Point(250, 250), 10);
            e.setTextColor("blue");
            e.setText(win.title);
            e.draw(win);
            Polygon p = new Polygon(new Point(300, 300), new Point(200, 400), new Point(400, 400));
            p.setFill("yellow");
            p.draw(win);
            //Image i = new Image(@"C:\Users\Vincent\Documents\Pyjama\leroy.jpg");
            //i.draw(win);
            Application.Run(win);
        }
    }
    #endregion

}