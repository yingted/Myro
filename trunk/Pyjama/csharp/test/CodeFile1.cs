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

namespace graphics
{
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

        public Dictionary<string, Color> color_map ()
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
    public class GraphicsError : Exception
    {
        string BAD_OPTION = "Illegal option value";
        string OBJ_ALREADY_DRAWN = "Object currently drawn";
        string UNSUPPORTED_METHOD = "Object doesn't support operation";
        string DEAD_THREAD = "Graphics thread quit unexpectedly";

        GraphicsError(string error)
        {

        }
    }
    #endregion

    #region Window
    /// <summary>
    /// Window class
    /// </summary>
    public class Window : Form
    {
        string _title;
        int _height;
        int _width;
        bool _autoflush;
        bool _smooth;
        bool _closed;
        object trans;
        ArrayList _items;
        //int mouseX;
        //int mouseY;
        //bool mouseCallBack;

        public int height
        {
            get { return _height; }
        }

        public int width
        {
            get { return _width; }
        }

        public string title
        {
            get { return _title; }
            set { _title = value; }
        }

        public bool autoflush
        {
            get { return _autoflush; }
        }

        private bool smooth
        {
            get { return _smooth; }
        }

        public Window()
        {
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            this._title = "My Window";
            this._height = 400;
            this._width = 400;
            this._smooth = false;
            this._autoflush = true;
            this._items = new ArrayList();
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

            this.Height = this._height;
            this.Width = this._width;
            this._closed = false;
            this._autoflush = autoflush;
            this._smooth = smooth;

            //this.mouseX = (int)MouseButtons.None;
            //this.mouseY = (int)MouseButtons.None;
            //this.mouseCallBack = false;

            this.Paint += this._onPaint;
            //this.Click += this._onClick;
            this.FormClosing += this._onFormClosing;

            this.BackColor = System.Drawing.Color.FromArgb(255, 236, 233, 216);

            // Refresh set to autoflush
            if (autoflush) this.Invalidate();
        }

        private void _onPaint(object sender, PaintEventArgs e)
        {
            if (this.smooth)
                e.Graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;

            foreach (GraphicsObject s in this._items)
            {
                s._draw(e.Graphics);
            }
        }

        private void _onClick(object sender, MouseEventArgs e)
        {
            // Save the location of mouse clicks
            //this.mouseX = e.X;
            //this.mouseY = e.Y;
        }

        private void _onFormClosing(object sender, EventArgs e)
        {
            this.Close();
        }

        private void _checkOpen()
        {
            //if (this._closed)
                //throw new ArgumentException("Window is closed");
                //throw new GraphicsError();
        }

        public void append(object itm)
        {
            this._items.Add(itm);
        }

        public void remove(object itm)
        {
            this._items.Remove(itm);
        }

        public void setBackground(string color)
        {
            /// Set background color of window
            this._checkOpen();
            MapColors map = new MapColors();
            this.BackColor = map.color_map()[color];
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

        public void _close_help()
        {
            /// Close the window
            this._closed = true;
            this.Close();
        }

        public bool isClosed()
        {
            /// Return True of this GraphWin is closed
            return this._closed;
        }

        public void plot(int x, int y, string clr)
        {
            /// Set pixel (x, y) to the given color
            this._checkOpen();


        }

        private void plotPixel(int x, int y, string clr)
        {
           /// Set pixel raw (independent of window coordinates)
           /// pixel (x, y) to color
            Point pt = new Point(x, y);
            
        }

        public int getHeight()
        {
            return this.height;
        }

        public int getWidth()
        {
            return this.width;
        }

        public int[] toScreen(int x, int y)
        {
            /// Convert x,y to screen coordinates
            int[] val = new int[2];
            val[0] = x;
            val[1] = y;
            return val;
        }

        public float[] toWorld(int x, int y)
        {
            /// Convert x,y to world coordinates
            float[] val = new float[2];
            val[0] = (float)x;
            val[1] = (float)y;
            return val;
        }

       [STAThread]
        static void Main()
        {
            Application.Run(new Window().);
        }

        private void Window_Load(object sender, System.EventArgs e)
        {

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
        string fill_color;
        string outline_color;
        int outline_width = 1;
        Brush _brush;
        Pen _pen;
        Window _canvas;

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

        public Window canvas
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

        public void setFill(string clr)
        {
            /// Set interior color to color
            if (this.fill_color == clr) return;
            MapColors map_colors = new MapColors();
            this._brush = new SolidBrush(map_colors.color_map()[clr]);
            this.fill_color = clr;
        }

        public void setOutline(string clr)
        {
            /// Set outline color to color
            if (this.outline_color == clr) return;
            MapColors map_colors = new MapColors();
            Color color = map_colors.color_map()[clr];
            this._pen = new Pen(color, this.outline_width);
            this.outline_color = clr;
        }

        public void setWidth(int width)
        {
            /// Set line weight to width
            if (this.outline_width == width) return;
            MapColors map_colors = new MapColors();
            Color clr = map_colors.color_map()[this.outline_color];
            this._pen = new Pen(clr, width);
        }

        public void draw(Window graphWin)
        {
            /// Draw the object in graphwin, which should be GraphWin
            /// object. A GraphicsObject may only be drawn into one 
            /// window. Raise an error if attempt made to draw an object that 
            /// is all ready visible.
            //if (this.canvas.isClosed())
                
            this.canvas = graphWin;
            this.canvas.append(this);
            
        }

        public void undraw()
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

        public void _draw(object canvas)
        {
            return;
        }

        private void _move(int dx, int dy)
        {
            return;
        }
    }
    #endregion

    #region Point
    /// <summary>
    /// Point Class
    /// </summary>
    class Point : GraphicsObject
    {
        float _x;
        float _y;

        public float x
        {
            get { return _x; }
            set { _x = value; }
        }

        public float y
        {
            get { return _y; }
            set { _y = value; }
        }

        public Point(float x, float y)
        /// Create a point on a canvas
        {
            this.setOutline("black");
            this._x = x;
            this._y = y;
        }

        public override string ToString()
        {
            return (String.Format("<Point at ({0},{1})>", this._x, this._y));
        }

        public void _draw(Graphics g)
        {
            /// Use a rectangle fill to draw points
            int[] coords = this.canvas.toScreen((int)this._x, (int)this._y);
            g.FillRectangle(this.brush, coords[0], coords[1], 1, 1);
        }

        public void _move(int dx, int dy)
        {
            this._x += dx;
            this._y += dy;
        }

        public Point clone()
        {
            Point other = new Point(this._x, this._y);
            other.pen = (Pen)this.pen.Clone();
            other.brush = (Brush)this.brush.Clone();
            return other;
        }
        
        float getX()
        {
            return this._x;
        }

        float getY()
        {
            return this._y;
        }


    }
    #endregion

    #region BBox
    /// <summary>
    /// Internal base class for objects represented by
    /// bounding box (opposite corners.
    /// Line segement is a degenerate case.
    /// </summary>
    class _BBox : GraphicsObject
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

        public void _move(float dx, float dy)
        {
            this.p1.x += dx;
            this.p1.y += dy;
            this.p2.x += dx;
            this.p2.y += dy;
        }

        public Point getP1()
        {
            return this.p1.clone(); 
        }

        public Point getP2()
        {
            return this.p2.clone();
        }

        public Point getCenter()
        {
            return new Point((float)(p1.x+p2.x)/2, (float) (p1.y+p2.y)/2);
        }
    }
    #endregion

    #region Rectangle
    /// <summary>
    /// Rectangle class
    /// </summary>
    class Rectangle : _BBox
    {
        public Rectangle(Point p1, Point p2)
        {
        }

        public void _draw(Graphics g)
        {
            int[] c1 = this.canvas.toScreen((int) p1.x, (int) p1.y);
            int[] c2 = this.canvas.toScreen((int) p2.x, (int) p2.y);
            // Need to write swap function here
            int width = c1[1] - c1[0];
            int height = c2[1] - c2[0];
            g.DrawRectangle(this.pen, c1[0], c1[1], width, height);
            g.FillRectangle(this.brush, c1[0], c1[1], width, height);
        }

        public Rectangle clone()
        {
            Rectangle other = new Rectangle(p1, p2);
            other.pen = (Pen) this.pen.Clone();
            other.brush = (Brush) this.brush.Clone();
            return other;
        }
    }
    #endregion

    #region Oval
    /// <summary>
    /// Oval class
    /// </summary>
    class Oval : _BBox
    {
    }
    #endregion

    #region Circle
    /// <summary>
    /// Circle class
    /// </summary>
    class Circle : Oval
    {
    }
    #endregion

    #region Line
    /// <summary>
    /// Line class
    /// </summary>
    class Line : _BBox
    {
    }
    #endregion

    #region Polygon
    /// <summary>
    /// Polygon class
    /// </summary>
    class Polygon : GraphicsObject
    {
        Point[] Points; 

        public Polygon(params Point[] points)
        {
            for (int i = 0; i < points.Length; i++)
                this.Points[i] = points[i].clone();
        }

        public Polygon clone()
        {
            Polygon other = new Polygon(this.Points);
            other.pen = (Pen)this.pen.Clone();
            other.brush = (Brush)this.brush.Clone();
            return other;
        }

        public Point[] getPoints()
        {
            Point[] pClones = new Point[this.Points.Length];
            for (int i = 0; i < this.Points.Length; i++)
                pClones[i] = this.Points[i].clone();
            return pClones;
        }

        public void _move(int dx, int dy)
        {
            foreach (Point p in this.Points)
                p.move(dx, dy);
        }

        public void _draw(Graphics g)
        {
            /// FIXME ME NOW!
            int[] value;
            System.Drawing.Point[] pts = new System.Drawing.Point[this.Points.Length];
            for (int i = 0; i < this.Points.Length; i++)
            {
                value = canvas.toScreen( (int) Points[i].x, (int) Points[i].y);
                pts[i] = new System.Drawing.Point((int)this.Points[i].x, (int)this.Points[i].y);
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
    class Text : GraphicsObject
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

        public Text(Point p, string text)
        {
            this.setFill("black");
            this.setOutline("black");
            this.font = new Font("Helvetica", 12);
            this.anchor = p.clone();
        }

        private void InitializeComponent()
        {
        }

        private void _draw(Graphics g)
        {
            this.p = this.anchor;
            int[] coords = this.canvas.toScreen((int) p.x, (int) p.y);
            // change to double
            StringFormat frmt = new StringFormat();
            frmt.Alignment = StringAlignment.Center;
            frmt.LineAlignment = StringAlignment.Center;
            g.DrawString(this._text, this.font, this.brush, coords[0], coords[1], frmt);
        }

        private void _move(int dx, int dy)
        {
            this.anchor.move(dx, dy);
        }

        public Text clone()
        {
            Text other = new Text(this.anchor, this._text); // fix me
            other.pen = (Pen) this.pen.Clone();
            this.brush = (Brush) this.brush.Clone();
            this.font = (Font) this.font.Clone();
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
            int fsize = (int) this.font.Size;
            FontStyle fstyle = this.font.Style;
            this.font = new Font(fface, fsize, fstyle);
        }

        public void setSize(int size)
        {
            float fsize;
            if ( 5 <= size & size <= 36 )
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
            catch { fstyle = FontStyle.Regular;  }
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
    class Entry : GraphicsObject
    {
    }
    #endregion

    #region Image
    /// <summary>
    /// Image class
    /// </summary>
    class Image : GraphicsObject
    {

    }
    #endregion
}