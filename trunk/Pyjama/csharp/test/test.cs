using graphics;
using System.Threading;
using System;

class Test
{
    [STAThread]
    static void Main()
    {
        Window win = new Window();
        Point pt = new Point(100, 100);
        pt.draw(win);
        win.Show();
        Console.Read();
        
        
    }
}