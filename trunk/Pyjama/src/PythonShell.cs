using Gtk;
using Gdk;
using GtkSourceView;
using System.IO;
using System.Text;
using System;

using PyjamaInterfaces;
using PyjamaGraphics;

using IronPython;
using IronPython.Compiler;
using IronPython.Hosting;
using IronPython.Runtime;

using Microsoft;

#if HOSTINGVER2
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
#endif

class TextBufferOutputStream: Stream
{
    TextBuffer buffer;
    
    public TextBufferOutputStream(TextBuffer b)
    {
        buffer = b;
    }
    
    override public bool CanRead
    {
        get
        {
            return false;
        }
    }
    
    override public bool CanSeek
    {
        get
        {
            return false;
        }
    }
    
    override public bool CanWrite
    {
        get
        {
            return true;
        }
    }
    
    override public long Length
    {
        get
        {
            return 0;
        }
    }
    
    override public long Position
    {
        get
        {
            return 0;
        }
        
        set
        {
        }
    }
    
    override public void Flush()
    {
    }
    
    override public int Read(byte[] data, int offset, int count)
    {
        return 0;
    }
    
    override public void Write(byte[] data, int offset, int count)
    {
        string str = System.Text.Encoding.ASCII.GetString(data);
        buffer.Text += str.Substring(0, count);
    }
    
    override public long Seek(long offset, SeekOrigin origin)
    {
        return 0;
    }
    
    override public void SetLength(long length)
    {
    }
}

public class PythonShell: PyjamaInterfaces.IShell
{
    SourceBuffer buffer;
    SourceLanguage language;
    SourceView source_view;
#if HOSTINGVER2
    private readonly ScriptEngine engine;
    private readonly ScriptRuntime runtime;
    private ScriptScope scope;
#elif HOSTINGVER1
    PythonEngine engine;
#endif
    public PythonShell()
    {
        SourceLanguagesManager mgr = new SourceLanguagesManager();
	language = mgr.GetLanguageFromMimeType("text/x-python");
        // Set up syntax highlighting
	buffer = new SourceBuffer(language);
        buffer.Highlight = true;
	source_view = new SourceView(buffer);
	// Change the font to fixed-width:
	Pango.FontDescription font = new Pango.FontDescription();
	font.Family = "Monospace";
	source_view.ModifyFont(font);
	// Event Handlers:
	source_view.KeyPressEvent += new KeyPressEventHandler(OnSourceViewChanged);
	source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
        TextBufferOutputStream output_stream = new TextBufferOutputStream(buffer);
#if HOSTINGVER2
	runtime = ScriptRuntime.Create();
	runtime.LoadAssembly(typeof(string).Assembly);
        //runtime.LoadAssembly(typeof(Debug).Assembly);
	engine = runtime.GetEngine("py");

	Encoding encoding = Encoding.UTF8;
	//engine.Runtime.IO.SetInput(stdin, encoding);
        engine.Runtime.IO.SetOutput(output_stream, encoding);
        engine.Runtime.IO.SetErrorOutput(output_stream, encoding);
	scope = engine.Runtime.CreateScope();
#elif HOSTINGVER1
        engine = new PythonEngine();
        engine.SetStandardOutput(output_stream); 	         
	engine.SetStandardError(output_stream);
#endif
	// Probably a more direct way to do these things:
	Execute("import sys");
	string ip_site = Environment.GetEnvironmentVariable("IRONPYTHON_PATH");
	Console.WriteLine("ip_site: {0}", ip_site);
	// FIXME: path delimiter:
	char [] c = new char[1];
	//c[0] = Path.PathSeparatorChar;
	c[0] = ':';
	if (ip_site != null) {
	    string [] path = ip_site.Split(c,
			      	StringSplitOptions.RemoveEmptyEntries);
	    foreach (string p in path) {
		if (p != "") {
		    Execute("sys.path.append('" + p + "')");
		}
	    }
	}
	Execute("sys.path.append('.')");
	buffer.Text =  "# Python " + Execute("sys.version") + "\n";
	buffer.Text += "# " + Execute("sys.copyright") + "\n";
	Execute("del sys");
	buffer.Text += ">>> ";
    }

    object Execute(string input) {
	string retval = "";
#if HOSTINGVER2
	try {
	    object o = engine.CreateScriptSourceFromString(input, 
				 SourceCodeKind.Expression).Execute(scope);
	    retval = o.ToString();
	} catch {
	    try {
		engine.CreateScriptSourceFromString(input, 
			    SourceCodeKind.Statements).Execute(scope);
	    } catch (Exception e) {
		retval = e.ToString();
	    }
	}
#elif HOSTINGVER1
        // This is a hack, but works for now
        try {
	    object o = engine.Evaluate(input);
	    retval = o.ToString();
	} catch {
	    try {
		engine.Execute(input);
	    } catch (Exception e) {
		retval = e.ToString();
	    }
	}
#endif
	return retval;
    }

    // To get in the loop before the SourceView handles the keypress
    // you need this directive:
    [GLib.ConnectBefore]
    private void OnSourceViewChanged(object obj, KeyPressEventArgs args) 
    {
	TextIter iter, start, end;
	int line_cnt, cursor_pos;
	// Had to cast as int on Windows for Gdk.Key and bit mask comparison
	if ((int)(args.Event.Key) == (int)(Gdk.Key.Return)) {
	    // FIXME: this is rough, but gives examples of most of
	    // what we'll need
            cursor_pos = buffer.CursorPosition;
	    iter = buffer.GetIterAtOffset(cursor_pos);
	    line_cnt = iter.Line;
            bool echo = false;
            start = buffer.GetIterAtLine(line_cnt);
            int line_len = iter.CharsInLine;
            int buffer_cnt = buffer.LineCount;
            if ((buffer_cnt - line_cnt) > 1) {
                line_len -= 1;
                echo = true;
	    }
            end = buffer.GetIterAtLineOffset(line_cnt, line_len);
            string line = buffer.GetText(start, end, false);
            buffer.Text += "\n";
            if (line.StartsWith(">>>")) {
		line = line.Substring(3, line.Length - 3).Trim();
	    } else {
		buffer.Text += ">>> ";
                end = buffer.EndIter;
		buffer.PlaceCursor(end);
		args.RetVal = true;
		source_view.ScrollMarkOnscreen(buffer.InsertMark);
		return;
	    }
            if (echo) {
		buffer.Text += ">>> " + line;
		end = buffer.EndIter;
                buffer.PlaceCursor(end);
		args.RetVal = true;
		source_view.ScrollMarkOnscreen(buffer.InsertMark);
		return;
	    }
	    buffer.Text += Execute(line);

	    // This is a hack, but works for now
	    // FIXME: should check for parse error
	    //        before starting to execute??
	    /*
            try {
		object o = python.Evaluate(line);
		buffer.Text += o.ToString();
            } catch (Exception e) {
		try {
		    python.Execute(line);
		} catch {
		    buffer.Text += e;
		}
            }
	    */
	    // Don't give a newline unless we need one:
            cursor_pos = buffer.CursorPosition;
	    iter = buffer.GetIterAtOffset(cursor_pos);
	    if (iter.CharsInLine != 0) {
		buffer.Text += "\n";
	    }
	    buffer.Text += ">>> ";
            end = buffer.EndIter;
            buffer.PlaceCursor(end);
	    args.RetVal = true;
	} else if (((int)(args.Event.Key) == (int)(Gdk.Key.Home)) || 
		   (((int)(args.Event.Key) == (int)(Gdk.Key.a)) && 
		    (((int)(args.Event.State) & (int)(Gdk.ModifierType.ControlMask)) != 0))) {
            cursor_pos = buffer.CursorPosition;
	    iter = buffer.GetIterAtOffset(cursor_pos);
	    line_cnt = iter.Line;
            start = buffer.GetIterAtLine(line_cnt);
	    start.ForwardChars(3);
	    buffer.PlaceCursor(start);
	    args.RetVal = true;
	} else if (((int)(args.Event.Key) == (int)(Gdk.Key.End)) && 
		   (((int)(args.Event.Key) == (int)(Gdk.Key.e)) && 
		    (((int)(args.Event.State) & (int)(Gdk.ModifierType.ControlMask)) != 0))) {
	    end = buffer.EndIter;
	    buffer.PlaceCursor(end);
	    args.RetVal = true;
	}
	source_view.ScrollMarkOnscreen(buffer.InsertMark);
    }
    
    public void ExecuteFile(string filename)
    {
	// FIXME: import file
    }

    public void EvaluateExp(string expression)
    {
	// FIXME: evaluate exp in env
    }

    public void Restart()
    {
	// Fixme: restart the shell (with new env)
    }

    public void Quit()
    {
	// FIXME: close the shell
    }

    public Widget GetView()
    {
	return (Widget) source_view;
    }
}
