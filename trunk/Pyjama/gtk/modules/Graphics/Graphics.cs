/* Graphics Module for IronPython/IronRuby
 * Translated graphics module by Mark F. Russo and Doug Blank
 * Translated by Michelle Beard 07/01/2010
 * Inspired graphics module by John Zelle of Wartburg College
 * http://mcsp.wartburg.edu/zelle/pythoh
*/

public class Graphics {

    public static void init() {
	Gtk.Application.Init();
    }

    public class GraphWin : Gtk.Window {
	public GraphWin(string title) : base(title) {
	    this.Add(new Button("Press me!"));
	    Show();
	}
	public new void Show() {
	    Gtk.Application.Invoke(delegate { base.Show(); });
	}
	public new void ShowAll() {
	    Gtk.Application.Invoke(delegate { base.ShowAll(); });
	}
    }

    public class Button : Gtk.Button {
	public Button(string label) : base(label) {
	}
	public new void Show() {
	    Gtk.Application.Invoke(delegate { base.Show(); });
	}
	public new void ShowAll() {
	    Gtk.Application.Invoke(delegate { base.ShowAll(); });
	}
    }

    public static void ShowAll(object o) {
	Gtk.Application.Invoke(delegate { ((Gtk.Widget)o).ShowAll(); });
    }

    public static void Show(object o) {
	Gtk.Application.Invoke(delegate { ((Gtk.Widget)o).Show(); });
    }

    public class Turtle {

	public int x;
	public int y;
	public GraphWin window;

	public Turtle() {
	    
	}

	public void draw(GraphWin win) {
	    window = win;
	    //Show(turtle);
	}

	public void forward(int steps) {
	}

    }

}