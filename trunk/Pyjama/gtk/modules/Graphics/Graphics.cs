using System.Collections.Generic;
using System;

public static class Graphics {

    public static bool initialize;
    private static Dictionary<string, Cairo.Color> _color_map = 
	new Dictionary<string, Cairo.Color>();

    public static Cairo.Color color_map(string name) {
	return _color_map[name];
    }

    public static Cairo.Color color_rgb(int r, int g, int b) {
	return new Cairo.Color(r/255.0, g/255.0, b/255.0, 1);
    }

    public static List<string> color_names() {
	return new List<string>(_color_map.Keys);
    }

    public static void init() {
	if (!initialize) {
	    initialize = true;
	    _color_map.Add("black", new Cairo.Color(0, 0, 0, 1));
	    _color_map.Add("white", new Cairo.Color(255, 255, 255, 1));
	    _color_map.Add("red", new Cairo.Color(255, 0, 0, 1));
	    _color_map.Add("green", new Cairo.Color(0, 255, 0, 1));
	    _color_map.Add("blue", new Cairo.Color(0, 0, 255, 1));
	    _color_map.Add("yellow", new Cairo.Color(255, 255, 0, 1));
	    Gtk.Application.Init();
	}
    }

    public class GraphWin : Gtk.Window {
	private Canvas _canvas;
	
	public GraphWin(string title) : base(title) {
	    if (! Graphics.initialize) 
		Graphics.init();
	    _canvas = new Canvas("draw");
	    this.Add(_canvas);
	    ShowAll();
	}

	public string mode {
	    get {
		return _canvas.mode;
	    }
	    set {
		if (value == "animate" || value == "draw")
		    _canvas.mode = value;
		else
		    throw new Exception("window mode must be 'animate' or 'draw'");
		
	    }
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

	  public double x;
	  public double y;
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
	public double x;
	public double y;

	public Point(double x, double y) {
	    this.x = x;
	    this.y = y;
	}
    }

    public class Canvas : Gtk.DrawingArea {

	// Shape.draw() will add them here:
	public List<Shape> shapes = new List<Shape>();
	private string _mode;

	public string mode {
	    get {
		return _mode;
	    }
	    set {
		if (value == "animate" || value == "draw")
		    _mode = value;
		else
		    throw new Exception("canvas mode must be 'animate' or 'draw'");
	    }
	}

	public Canvas(string mode) : base() {
	    this.mode = mode;
	    
	}

	protected override bool OnExposeEvent (Gdk.EventExpose args) {
	    using (Cairo.Context g = Gdk.CairoHelper.Create(args.Window)) {
		// clip to the visible part
		g.Rectangle(args.Area.X, args.Area.Y,
			    args.Area.Width, args.Area.Height);
		g.Clip();
		try {
		    foreach (Shape shape in shapes) {
			shape.render(g);
		    }
		} catch {
		    // updating the window while someone changed the objects
		}
	    }
	    return true;
	}
    } 

    public class Shape {
	public Point center;
	public GraphWin window;
	public double direction; // radians

	private Point [] points;
	public Point points_center;
	private Cairo.Color _fill_color;
	private Cairo.Color _outline_color;
	private int _line_width;

	private Pen _pen;
	private bool _has_pen;

	public bool has_pen {
	    get {
		return _has_pen;
	    }
	    set {
		_has_pen = value;
	    }
	}

	public int line_width {
	    get {
		return _line_width;
	    }
	    set {
		_line_width = value;
	    }
	}

	public Pen pen {
	    get {
		return _pen;
	    }
	    set {
		if (has_pen)
		    _pen = new Pen("black", 1);
		else
		    throw new Exception("this shape cannot have a pen");
	    }
	}

	public void QueueDraw() {
	    /*
	    if (window is GraphWin)
		Gtk.Application.Invoke(delegate {
			window.update();
			while (Gtk.Application.EventsPending ())
			    Gtk.Application.RunIteration ();			
		    });
	    */
	}

	public Shape(bool has_pen=true) {
	    center = new Point(0,0);
	    this.has_pen = has_pen;
	    if (this.has_pen) 
		pen = new Pen("black", 1);
	    points_center = new Point(0,0);
	    fill_color = new Cairo.Color(0.0, 0.0, 0.0, 1);
	    outline_color = new Cairo.Color(0.0, 0.0, 0.0, 1);
	    width = 1;
	}

	public void render(Cairo.Context g) {
	    g.LineWidth = width;
	    g.MoveTo(center.x + points[0].x + points_center.x, 
		     center.y + points[0].y + points_center.y);
	    for (int p = 1; p < points.Length; p++) {
		g.LineTo(center.x + points[p].x + points_center.x, 
			 center.y + points[p].y + points_center.y);
	    }
	    g.ClosePath();
	    g.Color = fill_color;
	    g.FillPreserve();
	    g.Color = outline_color;
	    g.Stroke();
	    if (has_pen)
		pen.render(g);
	}
	
	public int width {
	    get {
		return _line_width;
	    }
	    set {
		_line_width = value;
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

	public Cairo.Color ccolor {
	    set {
		_fill_color = value;
		_outline_color = value;
		QueueDraw();
	    }
	}

	public string color {
	    set {
		_fill_color = Graphics.color_map(value);
		_outline_color = Graphics.color_map(value);
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
	    compute_points_center();
	}

	public void move_to(double x, double y) {
	    double dx = x - center.x;
	    double dy = y - center.y;
	    move(dx, dy);
	}

	public void move(double dx, double dy) {
	    if (has_pen && pen.down)
		pen.append_path(new Line(false, // no pen for this line
					 new Point(center.x, center.y),
					 new Point(center.x + dx, center.y + dy)));
	    center.x += dx;
	    center.y += dy;
	    QueueDraw();
	}

	public void rotate(double degrees) {
	    rotate_to(degrees, points_center);
	}

	public void forward(double distance) {
	    double x = ((center.x + distance) * Math.Cos(direction) - 
			(center.y) * Math.Sin(direction));
	    double y = ((center.x - distance) * Math.Sin(direction) + 
			(center.y) * Math.Cos(direction));
	    center.x = x;
	    center.y = y;
	}

	public void backward(double distance) {
	    forward(-distance);
	}

	public void rotate_to(double degrees, Point rpoint) {
	    direction = degrees * (2 * Math.PI) / 360.0;
	    foreach (Point point in points) {
		double x = ((point.x - rpoint.x) * Math.Cos(direction) - 
			    (point.y - rpoint.y) * Math.Sin(direction));
		double y = ((point.x - rpoint.x) * Math.Sin(direction) + 
			    (point.y - rpoint.y) * Math.Cos(direction));
		point.x = x + rpoint.x;
		point.y = y + rpoint.y;
	    }
	    QueueDraw();
	}

	public void compute_points_center() {
	    double sum_x = 0, sum_y = 0;
	    if (points.Length == 0) {
		points_center.x = 0;
		points_center.y = 0;
	    } else if (points.Length == 1) {
		points_center.x = points[0].x;
		points_center.y = points[0].y;
	    } else if (points.Length > 1) {
		for (int p = 0; p < points.Length; p++) {
		    sum_x += points[p].x;
		    sum_y += points[p].y;
		}
		points_center.x = sum_x/points.Length;
		points_center.y = sum_y/points.Length;
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
	     move_to(points_center.x, points_center.y);
	 }
	 public Line(bool has_pen, Point p1, Point p2) : base(has_pen) {
	     set_points(p1, p2);
	     move_to(points_center.x, points_center.y);
	 }
     }

     public class Arrow : Shape {
	 public Arrow(Point new_center) : this(new_center, 0) {

	 }

	 public Arrow(Point new_center, double degrees) {
	     set_points(
			new Point(  0,  0),
			new Point( -5, -5), 
			new Point(  5,  0),
			new Point( -5,  5) 
			);
	     move_to(new_center.x, new_center.y);
	     rotate(degrees);
	 }
     }

     public class Pen : Shape {
	 private bool _down;
	 private List<Line> _path = new List<Line>();

	 public void reset_path() {
	     _path = new List<Line>();
	 }

	 public void append_path(Line line) {
	     _path.Add(line);
	 }

	 public bool down {
	    get {
		return _down;
	    }
	    set {
		_down = value;
	    }
	 }

	 public List<Line> path {
	    get {
		return _path;
	    }
	 }

	 public bool up {
	    get {
		return (! _down);
	    }
	    set {
		_down = (! value);
	    }
	 }

	 public Pen(string color, int width) : base(false) {
	     down = false;
	     this.color = color;
	     this.line_width = width;
	 }

	public new void render(Cairo.Context g) {
	    foreach (Line line in path) {
		line.render(g);
	    }
	}
     }
}
