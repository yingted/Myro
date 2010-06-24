using System;

namespace Graphics
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
    }
}
