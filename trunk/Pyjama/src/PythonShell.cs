using Gtk;
using Gdk;
using GtkSourceView;
using System;
using System.IO;
using System.Collections.Generic;

using PyjamaInterfaces;
using PyjamaGraphics;
using IronPython.Hosting;

#if HOSTINGVER2
using System.Text;
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
    string prompt = ">>> ";
    TextMark command_start;
    
#if HOSTINGVER2
    private readonly ScriptEngine engine;
    private readonly ScriptRuntime runtime;
    private ScriptScope scope;
#elif HOSTINGVER1
    PythonEngine engine;
#endif
    
    List<string> history = new List<string>();
    int history_pos = 0;
    
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
	source_view.KeyPressEvent += new KeyPressEventHandler(OnSourceViewKey);
        source_view.MoveCursor += new MoveCursorHandler(OnSourceViewMove);
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
	command("import sys");
	command("sys.path.append('.')");
	buffer.Text =  "# Python " + command("sys.version") + "\n";
	buffer.Text += "# " + command("sys.copyright") + "\n";
	command("del sys");
	
        add_prompt();
    }

    // Evaluates an expression or executes a statement.
    // Call this for user command-line input when you don't know if
    // the string is an expression or statement.
    public string command(string line)
    {
	string retval = "";
#if HOSTINGVER2
        try {
            object o = engine.CreateScriptSourceFromString(line, 
                                 SourceCodeKind.Expression).Execute(scope);
	    if (o != null) {
		retval = o.ToString();
	    }
        } catch (Exception) {
	    // FIXME: what is the IP2 version of PythonSyntaxErrorException?
            // Not a valid expression, so try it as a statement
            try {
                engine.CreateScriptSourceFromString(line, 
                            SourceCodeKind.Statements).Execute(scope);
            } catch (Exception e) {
                retval = e.ToString();
            }
        }
#elif HOSTINGVER1
        try {
            object o = engine.Evaluate(line);
	    if (o != null) {
		retval = o.ToString();
	    }
        } catch (IronPython.Runtime.Exceptions.PythonSyntaxErrorException) {
            // Not a valid expression, so try it as a statement
            try {
                engine.Execute(line);
            } catch (Exception e) {
                retval = (e.Message + " in " + e.Source);
            }
        } catch (Exception e) {
            // Not a parse error, so this is an exception caused by the expression.
            retval = (e.Message + " at " + e.Source);
        }
#endif
	return retval;
    }

    void add_prompt()
    {
        // Append the prompt to the buffer
        buffer.Text += prompt;
        buffer.PlaceCursor(buffer.EndIter);
        
        // Make a new start-of-command mark
        if (command_start != null)
        {
            buffer.DeleteMark(command_start);
        }
        command_start = buffer.CreateMark(null, buffer.EndIter, true);
        
        // Reset the history position
        history_pos = history.Count;
    }

    void execute()
    {
        TextIter start = buffer.GetIterAtMark(command_start);
        string line = buffer.GetText(start, buffer.EndIter, false);
        
        buffer.Text += '\n';
        
        // Ignore blank lines
        if (line == "")
        {
            add_prompt();
            return;
        }
        
        // FIXME - Add this command to history unless we were executing exactly the last command.
        //  (So XYZ, (up), (up), (up) doesn't produce four copies of XYZ in the history.)
        history.Add(line);
        
        /*
        buffer.TagTable.Foreach(print_tag);
        TextTag tag = buffer.TagTable.Lookup("Self");
        if (tag == null)
        {
            Console.WriteLine("No blank tag");
        } else {
            buffer.ApplyTag(tag, buffer.StartIter, buffer.EndIter);
            return;
        }
        */
        
        // Evaluate or execute
        buffer.Text += command(line);
        
        Console.WriteLine("History:");
        foreach (string s in history)
            Console.WriteLine(s);
        Console.WriteLine("\n");
        
        // Don't give a newline unless we need one:
        int cursor_pos = buffer.CursorPosition;
        TextIter iter = buffer.GetIterAtOffset(cursor_pos);
        if (iter.CharsInLine != 0) {
            buffer.Text += "\n";
        }

        add_prompt();
    }

    void OnSourceViewMove(object obj, MoveCursorArgs args)
    {
        Console.WriteLine("move");
        // args.RetVal=true doesn't stop the move, so we have to catch it afterwards
        // and put the cursor back at the beginning of the line.
        TextIter start_iter = buffer.GetIterAtMark(command_start);
        int start_pos = start_iter.Offset;
        if (buffer.CursorPosition < start_pos)
        {
            buffer.PlaceCursor(start_iter);
        }
    }

    // To get in the loop before the SourceView handles the keypress
    // you need this directive:
    [GLib.ConnectBefore]
    void OnSourceViewKey(object obj, KeyPressEventArgs args) 
    {
        // Had to cast as int on Windows for Gdk.Key and bit mask comparison
        int key = (int)args.Event.Key;
        
        //FIXME - Left, right, backspace
        
	if (key == (int)(Gdk.Key.Return)) {
            execute();
	    args.RetVal = true;
        } else if (key == (int)(Gdk.Key.Up)) {
            if (history_pos > 0)
            {
                history_pos--;
                SetCommand(history[history_pos]);
            }
            args.RetVal = true;
        } else if (key == (int)(Gdk.Key.Down)) {
            if (history_pos < (history.Count - 1))
            {
                history_pos++;
                SetCommand(history[history_pos]);
            }
            args.RetVal = true;
	} else if (key == (int)(Gdk.Key.Home)) {
	    buffer.PlaceCursor(buffer.GetIterAtMark(command_start));
	    args.RetVal = true;
	}
	source_view.ScrollMarkOnscreen(buffer.InsertMark);
    }
    
    // Replaces the current command.
    // Used for command history.
    public void SetCommand(string line)
    {
        // Delete the current command
        TextIter start = buffer.GetIterAtMark(command_start);
        TextIter end = buffer.EndIter;
        buffer.Delete(ref start, ref end);
        
        // Add the new command
        end = buffer.EndIter;
        buffer.Insert(ref end, line);
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
