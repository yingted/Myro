using System;
using System.Windows.Forms;
using System.Drawing;
using System.Collections;
using System.Threading;

namespace Graphics
{
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

        private bool autoflush
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

            foreach (object s in this._items)
            {
                // draw graphics
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
            if (this._closed)
                throw new ArgumentException("Window is closed");

        }

        private void append(object itm)
        {
            this._items.Add(itm);
        }

        private void remove(object itm)
        {
            this._items.Remove(itm);
        }

        private void setBackground(Color color)
        {
            this._checkOpen();
        }

        public void setCoords(int x1, int y1, int x2, int y2)
        {
            trans = new Transform(this.width, this.height, x1, y1, x2, y2);
        }

        private void plot(int x, int y, Color clr)
        {
            this._checkOpen();

        }

        private void plotPixel(int x, int y, Color clr)
        {
            Point pt = new Point(x, y);

        }

        [STAThread]
        static void Main()
        {
            Application.Run(new Window());
        }

        private void Window_Load(object sender, System.EventArgs e)
        {

        }
    }

    class Transform : Object
    {
        int xbase;
        int ybase;
        float xscale;
        float yscale;

        public Transform(int w, int h, int xlow, int ylow, int xhigh, int yhigh)
        {
            int xspan = (xhigh - xlow);
            int yspan = (yhigh - ylow);
            this.xbase = xlow;
            this.ybase = ylow;
            this.xscale = xspan / (w - 1);
            this.yscale = yspan / (h - 1);
        }

        private void screen(int x, int y)
        {
            float xs = (x - this.xbase) / this.xscale;
            float ys = (y - this.ybase) / this.yscale;
            // How to return two objects in function call?
        }

        private void world(int xs, int ys)
        {
            float x = xs * this.xscale + this.xbase;
            float y = ys * this.yscale + this.ybase;
        }
    }

    class GraphicsObject : Object
    {
        Color fill_color;
        Color outline_color;
        int outline_width = 1;
        Brush brush;
        Pen pen;
        Window canvas;

        private GraphicsObject()
        {
            InitializeComponent();
        }

        private void InitializeComponent()
        {

        }

        private void setFill(Color clr)
        {
            
        }

        private void setOutline(Color clr)
        {

        }

        private void setWidth(int width)
        {

        }

        private void draw(Window graphWin)
        {

        }

        private void undraw()
        {

        }

        private void move(int dx, int dy)
        {

        }

        private void _draw(Window canvas, object options)
        {

        }

        private void _move(int dx, int dy)
        {
            // pass
        }
    }
}