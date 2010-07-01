using graphics;
using System.Threading;
using System;

class Test
{
    [STAThread]
    static void Main()
    {
        Window win = new Window("New Window", 500, 500);
        win.setCoords(0, 0, 10, 10);
        win.setBackground("black");
        //Polygon p = new Polygon(new Point(1, 2), new Point(5, 3), new Point(2, 6));
        //p.draw(win);
        Point p = new Point(250, 250);
        p.draw(win);
        Console.Read();    
    }
}