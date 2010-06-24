using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace graphics
{
    public class Window
    {
        public string Title;
        public int Width;
        public int Height;
        
        public Window()
        {
            Title = "Graphics Window";
            Width = 200;
            Height = 200;
        }
        
        public Window(string title)
        {
            Title = title;
            Width = 200;
            Height = 200;
        }
        
        public Window(string title, int height)
        {
            Title = title;
            Height = height;
            Width = 200;
        }
        
        public Window(string title, int height, int width)
        {
            Title = title;
            Height = height;
            Width = width;
        }
        
        static void Main()
        {
           Window win = new Window("My Window", 500);
           Console.WriteLine("Window Properties");
           Console.WriteLine("Title =  {0}", win.Title);  
           Console.WriteLine("Height = {0}", win.Height); 
           Console.WriteLine("Width =  {0}", win.Width);  
           Console.Read();
        }
    }
}
