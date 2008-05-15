using Gtk;

using PyjamaInterfaces;

public class Pyjama
{
    public static void Main(string[] args)
    {
        Application.Init();
        PyjamaGUI gui = new PyjamaGUI(args);
        gui.ShowAll();
        Application.Run();
    }
}