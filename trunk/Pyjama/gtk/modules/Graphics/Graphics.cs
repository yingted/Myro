using System.Collections.Generic;
using PolygonIntersection;

public static class Graphics {

    public static bool initialize;

    public static Dictionary<string, Cairo.Color> color_map = 
	new Dictionary<string, Cairo.Color>();

    public static Cairo.Color color_rgb(int r, int g, int b) {
	return new Cairo.Color(r/255.0, g/255.0, b/255.0, 1);
    }

    public static void init() {
	if (!initialize) {
	    initialize = true;
	    color_map.Add("red", new Cairo.Color(255, 0, 0, 1));
	    color_map.Add("green", new Cairo.Color(0, 255, 0, 1));
	    color_map.Add("blue", new Cairo.Color(0, 0, 255, 1));
	    color_map.Add("yellow", new Cairo.Color(255, 255, 0, 1));
	    Gtk.Application.Init();
	}
    }

    public class GraphWin : Gtk.Window {
	public GraphWin(string title) : base(title) {
	    this.Add(new Canvas());
	    ShowAll();
	}
	public new void Show() {
	    Gtk.Application.Invoke(delegate { base.Show(); });
	}
	public new void ShowAll() {
	    Gtk.Application.Invoke(delegate { base.ShowAll(); });
	}
	public void update() {
	    Gtk.Application.Invoke(delegate { base.QueueDraw(); });
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

    public class Point {
	public int x;
	public int y;

	public Point(int x, int y) {
	    this.x = x;
	    this.y = y;
	}
    }

    public class Canvas : Gtk.DrawingArea {

	// Shape.draw() will add them here:
	public List<Shape> shapes = new List<Shape>();

	protected override bool OnExposeEvent (Gdk.EventExpose args) {
	    using (Cairo.Context g = Gdk.CairoHelper.Create(args.Window)) {
		Polygon p = new Polygon();
		p.Points.Add(new Vector(args.Area.X, 
					args.Area.Y));
		p.Points.Add(new Vector(args.Area.X + args.Area.Width,
					args.Area.Y));
		p.Points.Add(new Vector(args.Area.X + args.Area.Width,
					args.Area.Y + args.Area.Height));
		p.Points.Add(new Vector(args.Area.X, 
					args.Area.Y + args.Area.Height));
		p.BuildEdges();
		foreach (Shape shape in shapes) {
		    if (Collision.PolygonCollision(p, shape.polygon)) {
			shape.render(g);
		    }
		}
	    }
	    return true;
	}
    } 

    public class Shape {
	public Point [] points;
	public Point center;
	public Polygon polygon;
	private Cairo.Color _fill_color;
	private Cairo.Color _outline_color;
	private int _width;
	public GraphWin window;

	public void QueueDraw() {
	    if (window is GraphWin)
		Gtk.Application.Invoke(delegate {window.update();});
	}

	public Shape() {
	    center = new Point(0,0);
	    fill_color = new Cairo.Color(0.0, 0.0, 0.0, 1);
	    outline_color = new Cairo.Color(0.0, 0.0, 0.0, 1);
	    width = 1;
	}

	public void render(Cairo.Context g) {
	    g.LineWidth = width;
	    g.MoveTo(center.x + points[0].x, 
		     center.y + points[0].y);
	    for (int p = 1; p < points.Length; p++) {
		g.LineTo(center.x + points[p].x, 
			 center.y + points[p].y);
	    }
	    g.ClosePath();
	    g.Color = fill_color;
	    g.FillPreserve();
	    g.Color = outline_color;
	    g.Stroke();
	}
	
	public int width {
	    get {
		return _width;
	    }
	    set {
		_width = value;
		QueueDraw();
	    }
	}

	public double alpha {
	    get {
		return _outline_color.A;
	    }
	    set {
		_outline_color.A = value;
		_fill_color.A = value;
		QueueDraw();
	    }
	}

	public Cairo.Color outline_color {
	    get {
		return _outline_color;
	    }
	    set {
		_outline_color = value;
		QueueDraw();
	    }
	}

	public Cairo.Color fill_color {
	    get {
		return _fill_color;
	    }
	    set {
		_fill_color = value;
		QueueDraw();
	    }
	}

	public void set_points(params Point [] new_points) {
	    points = new Point [new_points.Length];
	    // copies
	    for (int p = 0; p < points.Length; p++) {
		points[p] = new Point(new_points[p].x, 
				      new_points[p].y);
	    }
	    compute_center();
	    move_to(0, 0);
	}

	public void move_to(int x, int y) {
	    int dx = x - center.x;
	    int dy = y - center.y;
	    move(dx, dy);
	}

	public void move(int dx, int dy) {
	    // Bounding box:
	    int min_x, min_y;
	    int max_x, max_y;
	    min_x = min_y =  10000;
	    max_x = max_y = -10000;
	    for (int p = 0; p < points.Length; p++) {
		points[p].x += dx;
		points[p].y += dy;
		// p1 is min's; p2 is max's
		min_x = points[p].x > min_x ? min_x : points[p].x;
		min_y = points[p].y > min_y ? min_y : points[p].y;
		max_x = points[p].x < max_x ? max_x : points[p].x;
		max_y = points[p].y < max_y ? max_y : points[p].y;
	    }
	    polygon = new Polygon();
	    polygon.Points.Add(new Vector(min_x, min_y));
	    polygon.Points.Add(new Vector(max_x, min_y));
	    polygon.Points.Add(new Vector(max_x, max_y));
	    polygon.Points.Add(new Vector(min_x, max_y));
	    polygon.BuildEdges();
	    center.x += dx;
	    center.y += dy;
	    QueueDraw();
	}

	public void compute_center() {
	    int sum_x = 0, sum_y = 0;
	    if (points.Length == 0) {
		center.x = 0;
		center.y = 0;
	    } else if (points.Length == 1) {
		center.x = points[0].x;
		center.y = points[0].y;
	    } else if (points.Length > 1) {
		for (int p = 0; p < points.Length; p++) {
		    sum_x += points[p].x;
		    sum_y += points[p].y;
		}
		center.x = sum_x/points.Length;
		center.y = sum_y/points.Length;
	    }
	}
	
	public void update() {
	    QueueDraw();
	}

	public void draw(GraphWin win) {
	    // Add this shape to the Canvas dictionary.
	    ((Canvas)win.Children[0]).shapes.Add(this);
	    // FIXME: QueueDrawRect
	    // FIXME: invalidate from and to rects on move
	    window = win;
	    QueueDraw();
	}
     }

     public class Line : Shape {
	 public Line(Point p1, Point p2) {
	     set_points(p1, p2);
	 }
     }

     public class Arrow : Shape {
	 public Arrow(Point new_center) {
	     set_points(
			new Point( -5, -5), 
			new Point(  0,  5),
			new Point(  5, -5), 
			new Point(  0,  0)
			);
	     move_to(new_center.x, new_center.y);
	 }
     }
}
