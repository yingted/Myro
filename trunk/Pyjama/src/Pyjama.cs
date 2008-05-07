using Gtk;

using PyjamaInterfaces;
using PyjamaGraphics;

public class Pyjama
{
   public static void Main(string[] args)
    {
        Application.Init();
        MainWindow mainwin = new MainWindow(args);
        mainwin.ShowAll();
        Application.Run();
    }
}