using System;
using System.Windows.Forms;
using System.Drawing;
using System.Collections;

namespace graphics
{
    public class Graphics
    {
        class Window : Form
        {
            string _title;
            int _height;
            int _width;
            bool _autoflush;
            bool _smooth;
            bool _closed;
            ArrayList _items;
            //int mouseX;
            //int mouseY;
            bool mouseCallBack;

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
            }
             
            // Default Constructor
            public Window()
            {  
                this._title = "My Graphics Window";
                this._height = 400;
                this._width = 400;
                this._items = new ArrayList();
                Form win = new Form();
                win.Text = this._title;

                // Add to the size of the window to account for borders
                int dwidth = this._width - this.ClientSize.Width;
                int dheight = this._height - this.ClientSize.Height;
                
                //this.Size = System.Drawing.Size(this._width + dwidth, this._height + dheight);
                this.TopMost = true;
                
                win.Height = this._height;
                win.Width = this._width;
                this._closed = false;

                //this.mouseX = (int)MouseButtons.None;
                //this.mouseY = (int)MouseButtons.None;
                //this.mouseCallBack = false;

                win.Paint += this._onPaint;
                //win.Click += this._onClick;
                win.FormClosing += this._onFormClosing;

                win.BackColor = System.Drawing.Color.FromArgb(255, 236, 233, 216);
                win.Show();
                
            }

           /* public Window(string title)
            {
                _title = title;
                _height = 200;
                _width = 200;
                //CreateWindow();
            }

            public Window(string title, int height)
            {
                _title = title;
                if (height <= 0)
                    throw new ArgumentException("Height must be greater than 0");
                _height = height;
                _width = 200;
                //CreateWindow();
            }

            public Window(string title, int height, int width)
            {
                _title = title;
                if (height <= 0)
                    throw new ArgumentException("Height must be greater than 0");
                _height = height;

                if (width <= 0)
                    throw new ArgumentException("Width must be greater than 0");
                _width = width;
                //CreateWindow();
            }
            */

           /* private void CreateWindow()
            {
                Form win = new Form();
                win.Text = _title; 
                win.Height = _height;
                win.Width = _width;
                win.BackColor = System.Drawing.Color.FromArgb(255, 236, 233, 216);
                win.Show();    
            }
            */

            private void _onPaint(object sender, EventArgs e)
            {
                foreach (object s in this._items)
                {
                    
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
    }
}