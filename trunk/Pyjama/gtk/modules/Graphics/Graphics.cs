using System.Collections.Generic;
using System;

public static class Graphics {

    public static bool initialize;
    private static Dictionary<string, Cairo.Color> _color_map = 
	new Dictionary<string, Cairo.Color>();

    public static Cairo.Color color_map(string name) {
	if (_color_map.ContainsKey(name))
	    return _color_map[name];
	else
	    throw new Exception(String.Format("unknown color '{0}'", name));
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
	private bool _dirty = false;
	private bool timer_running = false;
	private DateTime last_update = new DateTime(2000,1,1);
	private uint _update_interval = 100;

	public GraphWin(string title, 
			int width=300, 
			int height=300) : base(title) {

	  if (! Graphics.initialize) 
		Graphics.init();
	  _canvas = new Canvas("draw");
	  SetDefaultSize(width, height);
	  this.Add(_canvas);
	  ShowAll();
	}
	
	public uint update_interval {
	  get {
		return _update_interval;
	  }
	  set {
		_update_interval = value;
	  }
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
	  
	public void need_to_redraw() {
		_dirty = true;
		DateTime now = DateTime.Now;
		// diff is TimeSpan
		if ((now - last_update).TotalMilliseconds < _update_interval) {
		  // pass, too soon!
		  // but we need to make sure that someone checks
		  // in the future. 
		  if (timer_running) {
			// already one running!
			// we'll just wait
		  } else {
			// let's spawn one to check in 100 ms or so
			timer_running = true;
			GLib.Timeout.Add(_update_interval, 
				new GLib.TimeoutHandler(redraw_now) );
		  }
		} else {
		  last_update = now;
		  _dirty = false;
		  QueueDraw();
		}
	}

	public bool redraw_now() {
		DateTime now = DateTime.Now;
		if (_dirty) {
		  last_update = now;
		  _dirty = false;
		  QueueDraw();
		}
		timer_running = false;
		return false; // return true to try again
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
	public double _direction; // radians

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
	public double direction {
	    get {
		return _direction;
	    }
	    set {
		  rotate_to(value);
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
	  if (window is GraphWin) {
		Gtk.Application.Invoke(delegate {
			  window.need_to_redraw();
			});
	  }
	}

	public bool hit(Point p) {
	    int counter = 0;
	    double xinters;
	    Point p1, p2;
	    if (points != null) {
		p1 = points[0];
		for (int i=1; i<=points.Length; i++) {
		    p2 = points[i % points.Length];
		    if (p.y > Math.Min(p1.y, p2.y)) {
			if (p.y <= Math.Max(p1.y, p2.y)) {
			    if (p.x <= Math.Max(p1.x, p2.x)) {
				if (p1.y != p2.y) {
				    xinters = (p.y - p1.y) * (p2.x - p1.x)/(p2.y - p1.y) + p1.x;
				    if (p1.x == p2.x || p.x <= xinters)
					counter++;
				}
			    }
			}
		    }
		    p1 = p2;
		}
		return (counter % 2 == 0); // hit?
	    } else {
		return false;
	    }
	}

	public Shape(bool has_pen=true) {
	  _direction = 0;
	  center = new Point(0,0);
	  this.has_pen = has_pen;
	  if (this.has_pen) 
		pen = new Pen("black", 1);
	  points_center = new Point(0,0);
	  fill_color = "black";
	  outline_color = "black";
	  width = 1;
	}
	  
	  public void render(Cairo.Context g) {
	    if (points != null) {
		  g.LineWidth = width;
		  g.MoveTo(center.x + points[0].x - points_center.x, 
			       center.y + points[0].y - points_center.y);
		  for (int p = 1; p < points.Length; p++) {
		    g.LineTo(center.x + points[p].x - points_center.x, 
				     center.y + points[p].y - points_center.y);
		  }
		  g.ClosePath();
		  g.Color = _fill_color;
		  g.FillPreserve();
		  g.Color = _outline_color;
		  g.Stroke();
		  if (has_pen)
		    pen.render(g);
	    }
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
	  
	  public string outline_color {
	    get {
		  return "#FIXME";
	    }
	    set {
		  _outline_color = Graphics.color_map(value);
		  QueueDraw();
	    }
	  }
	  
	  public string fill_color {
	    get {
		  return "#FIXME";
	    }
	    set {
		  _fill_color = Graphics.color_map(value);
		  QueueDraw();
	    }
	  }
	  
	  public Cairo.Color raw_fill_color {
		// Set color from cairo color object
	    get {
		  return _fill_color;
	    }
	    set {
		  _fill_color = value;
		  QueueDraw();
	    }
	  }

	  public Cairo.Color raw_outline_color {
		// Set color from cairo color object
	    get {
		  return _outline_color;
	    }
	    set {
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

	public void forward(double distance) {
	    double x = ((center.x + distance) * Math.Cos(direction) - 
			(center.y) * Math.Sin(direction));
	    double y = ((center.x - distance) * Math.Sin(direction) + 
			(center.y) * Math.Cos(direction));
		if (has_pen && pen.down)
		  pen.append_path(new Line(false, // no pen for this line
				  new Point(center.x, center.y),
				  new Point(x, y)));
	    center.x = x;
	    center.y = y;
	    QueueDraw();
	}

	public void backward(double distance) {
	    forward(-distance);
	}

	public void rotate(double degrees) {
	  rotate(degrees, points_center);
	}

	public void rotate(double degrees, Point rpoint) {
	    double new_direction = direction + degrees * (2 * Math.PI) / 360.0;
		rotate_to(new_direction);
	}

	public void rotate_to(double degrees) {
	    rotate_to(degrees, points_center);
	}

	public void rotate_to(double degrees, Point rpoint) {
	  _direction = degrees * (2 * Math.PI) / 360.0;
	  //_direction = direction % (2.0 * Math.PI);
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
