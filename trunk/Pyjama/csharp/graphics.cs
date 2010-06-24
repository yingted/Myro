using System;
using System.Windows.Forms; 

namespace graphics
{
    public class Window : Form
    {
        private string title;
        private int width;
        private int height;
        private bool autoFlush;
        private bool smooth;

        public string Title
        {
            get
            {
                if (title == "")
                    return "Graphics Window";
                else
                    return title;
            }
        }

        public int Width
        {
            get
            {
                if (width <= 0 || width.ToString() == "")
                    return 200;
                else
                    return width;
            }
        }

        public int Height
        {
            get
            {
                if (height <= 0)
                    return 200;
                else
                    return height;
            }
        }

        public bool AutoFlush
        {
            get
            {
                return autoFlush;
            }
        }

        public bool Smooth
        {
            get
            {
                return smooth;
            }
        }

        public Window(string t,
                        int h,
                        int w,
                        bool af,
                        bool sm)
        {
            title = t;
            height = h;
            width = w;
            autoFlush = af;
            smooth = sm;
        }

            static void Main()
            {
                Window win = new Window("Graphics Window",
                                            300,
                                            -50,
                                            true,
                                            true);
                Console.WriteLine("Window Properties");
                Console.WriteLine("Title =  {0}", win.title);
                Console.WriteLine("Height = {0}", win.height);
                Console.WriteLine("Width =  {0}", win.width);
                Console.WriteLine("AutoF =  {0}", win.autoFlush);
                Console.WriteLine("Smooth = {0}", win.smooth);
            }
    }

    public class Transform
    {

    }

    public class GraphicsObject
    {

    }

    public class Point : GraphicsObject
    {

    }

    public class __BBox : GraphicsObject
    {

    }

    public class Rectangle : __BBox
    {

    }

    public class Oval : __BBox
    {

    }

    public class Circle : Oval
    {

    }

    public class Line : __BBox
    {

    }

    public class Polygon : GraphicsObject
    {

    }

    public class Text : GraphicsObject
    {

    }

    public class Entry : GraphicsObject
    {

    }

    public class Image : GraphicsObject
    {

    }


}
