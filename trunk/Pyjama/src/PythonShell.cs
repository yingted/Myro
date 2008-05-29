/*********************************************************************
 *
 * Copyright (c) 2008 Douglas S. Blank
 *
 * This source code is subject to terms and conditions of the
 * Microsoft Public License. A copy of the license can be found in the
 * License.html file at the root of this distribution. If you cannot
 * locate the Microsoft Public License, please send an email to
 * dlr@microsoft.com. By using this source code in any fashion, you
 * are agreeing to be bound by the terms of the Microsoft Public
 * License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *********************************************************************/

using Gtk;
using Gdk;
using GtkSourceView;
using System;
using System.IO;
using System.Collections.Generic;
using System.Threading;

using PyjamaInterfaces;
using IronPython.Hosting;
using IronPython.Runtime.Types;

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
        get { return false; }
    }
    
    override public bool CanSeek
    {
        get { return false; }
    }
    
    override public bool CanWrite
    {
        get { return true; }
    }
    
    override public long Length
    {
        get { return 0; }
    }
    
    override public long Position
    {
        get { return 0; }
        set {}
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
        
        TextIter iter = buffer.EndIter;
        buffer.Insert(ref iter, str.Substring(0, count));
    }
    
    override public long Seek(long offset, SeekOrigin origin)
    {
        return 0;
    }
    
    override public void SetLength(long length)
    {
    }
}

//FIXME - Don't let undo remove output
class ShellSourceView: SourceView
{
    string prompt = ">>> ";
    
    // Where the command begins in the buffer (immediately after the prompt)
    TextMark command_start;
    
    // Command history.
    // history is never empty.  The last entry is the current command.
    List<string> history = new List<string>();
    int history_pos = 0;
    
    public delegate void ExecuteHandler(string line);
    public event ExecuteHandler Execute;
    
    public ShellSourceView(SourceBuffer buf): base(buf)
    {
        KeyPressEvent += new KeyPressEventHandler(OnKey);
        MoveCursor += new MoveCursorHandler(OnMove);
    }
    
    public TextMark CommandStart
    {
        get { return command_start; }
    }
    
    public void AppendText(string str)
    {
        TextIter iter = Buffer.EndIter;
        Buffer.Insert(ref iter, str);
    }
    
    protected override void OnBackspace()
    {
        TextIter start_iter = Buffer.GetIterAtMark(command_start);
        int start_pos = start_iter.Offset;
#if NEWSOURCEVIEW
        int cur_pos = Buffer.CursorPosition;
#else
	TextMark mark = Buffer.InsertMark;
	TextIter iter = Buffer.GetIterAtMark(mark);
	int cur_pos = iter.Offset;
#endif
        if (cur_pos > start_pos)
        {
            base.OnBackspace();
        }
    }
    
    public void AddPrompt()
    {
        // Append the prompt to the buffer
        AppendText(prompt);
        Buffer.PlaceCursor(Buffer.EndIter);
        
        // Move the start-of-command mark
        if (command_start == null)
        {
            command_start = Buffer.CreateMark(null, Buffer.EndIter, true);
        } else {
            Buffer.MoveMark(command_start, Buffer.EndIter);
        }
        
        // Reset the history position
        //FIXME - This is hazardous.
        history.Add("");
        history_pos = history.Count - 1;
    }

    // Returns true if the cursor is in a position where the command can be edited.
    public bool CanEdit()
    {
        TextIter start_iter = Buffer.GetIterAtMark(command_start);
        int start_pos = start_iter.Offset;
#if NEWSOURCEVIEW
        return Buffer.CursorPosition >= start_pos;
#else
	TextMark mark = Buffer.InsertMark;
	TextIter iter = Buffer.GetIterAtMark(mark);
	return iter.Offset >= start_pos;
#endif
    }

    protected override bool OnButtonPressEvent(Gdk.EventButton e)
    {
        bool ret = base.OnButtonPressEvent(e);
        Editable = CanEdit();
        return ret;
    }
    
    void OnMove(object obj, MoveCursorArgs args)
    {
        // args.RetVal=true doesn't stop the move, so we have to catch it afterwards
        // and put the cursor back at the beginning of the line.
        TextIter start_iter = Buffer.GetIterAtMark(command_start);
        int start_pos = start_iter.Offset;
#if NEWSOURCEVIEW
	int cur_pos = Buffer.CursorPosition;
#else
	TextMark mark = Buffer.InsertMark;
	TextIter iter = Buffer.GetIterAtMark(mark);
	int cur_pos = iter.Offset;
#endif
        if (cur_pos < start_pos)
        {
            Buffer.PlaceCursor(start_iter);
        }
    }

    // To get in the loop before the SourceView handles the keypress
    // you need this directive:
    [GLib.ConnectBefore]
    void OnKey(object obj, KeyPressEventArgs args) 
    {
        // Had to cast as int on Windows for Gdk.Key and bit mask comparison
        int key = (int)args.Event.Key;
        
        if (key == (int)Gdk.Key.Return || key == (int)Gdk.Key.KP_Enter) {
            args.RetVal = true;
            string line = Command;
            AppendText("\n");
            
            // Ignore blank lines
            if (line == "")
            {
                AddPrompt();
                return;
            }
            
            // FIXME - Add this command to history unless we were executing exactly the last command.
            //  (So XYZ, (up), (up), (up) doesn't produce four copies of XYZ in the history.)
            history[history.Count - 1] = line;
            
            Execute(line);
            
//            AddPrompt();
        } else if (key == (int)(Gdk.Key.Up)) {
            args.RetVal = true;
            if (history_pos > 0)
            {
                // Save the current command if it's the last one
                if (history_pos == history.Count - 1)
                {
                    history[history.Count - 1] = Command;
                }
                
                history_pos--;
                Command = history[history_pos];
            }
        } else if (key == (int)(Gdk.Key.Down)) {
            args.RetVal = true;
            if (history_pos < (history.Count - 1))
            {
                history_pos++;
                Command = history[history_pos];
            }
        } else if (key == (int)(Gdk.Key.Home)) {
            //FIXME - Shift+HOME
            args.RetVal = true;
            Buffer.PlaceCursor(Buffer.GetIterAtMark(command_start));
        } else {
            // In case there's a key combination I forgot...
            Editable = CanEdit();
        }
        
        ScrollMarkOnscreen(Buffer.InsertMark);
    }
    
    public string Command
    {
        get
        {
            TextIter start = Buffer.GetIterAtMark(command_start);
            return Buffer.GetText(start, Buffer.EndIter, false);
        }
        
        set
        {
            // Delete the current command
            TextIter start = Buffer.GetIterAtMark(command_start);
            TextIter end = Buffer.EndIter;
            Buffer.Delete(ref start, ref end);
            
            // Add the new command
            AppendText(value);
            
            // Put the cursor at the end of the line
            Buffer.PlaceCursor(Buffer.EndIter);
        }
    }
}

public class PythonShell: PyjamaInterfaces.IShell
{
    SourceBuffer buffer;
    SourceLanguage language;
    ShellSourceView source_view;
    
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
        source_view = new ShellSourceView(buffer);
        
        // Change the font to fixed-width:
        Pango.FontDescription font = new Pango.FontDescription();
        font.Family = "Monospace";
        source_view.ModifyFont(font);
        
        // Event Handlers:
        source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
        source_view.Execute += execute;

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
        command("sys.path.append('python')");
        command("sys.path.append('DLLs')");
        buffer.Text += "# Python " + command("sys.version") + "\n" +
                       "# " + command("sys.copyright") + "\n";
        command("del sys");
        
        source_view.AddPrompt();
    }
    
    // Evaluates an expression or executes a statement.
    // Call this for user command-line input when you don't know if
    // the string is an expression or statement.
    public string command(string line)
    {
        string retval = "";
#if HOSTINGVER2
        try {
            // FIXME: do the conversion to REPR in C# (replace ToString())
            object o = engine.CreateScriptSourceFromString("repr(" + line + ")", 
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

    string command_line;
    void do_exec()
    {
        // Evaluate or execute
        source_view.AppendText(command(command_line));
        
        // Don't give a newline unless we need one:
#if NEWSOURCEVIEW
        int cursor_pos = buffer.CursorPosition;
        TextIter iter = buffer.GetIterAtOffset(cursor_pos);
#else
	TextMark mark = buffer.InsertMark;
	TextIter iter = buffer.GetIterAtMark(mark);
#endif
        if (iter.CharsInLine != 0) {
            source_view.AppendText("\n");
        }
        source_view.AddPrompt();
    }

    void execute(string line)
    {
        //FIXME - Prevent editing the ShellSourceView while the command is running.
        //      Also prevent execute from being called again.
        //FIXME - Gtk is probably not threeadsafe, so we need a mutex for python stdout.
        command_line = line;
        do_exec();
        /*
        Thread thread = new Thread(this.do_exec);
        thread.Start();
        
        
        thread.Join();
        
        // Don't give a newline unless we need one:
        int cursor_pos = buffer.CursorPosition;
        TextIter iter = buffer.GetIterAtOffset(cursor_pos);
        if (iter.CharsInLine != 0) {
            source_view.AppendText("\n");
        }*/
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
    
    public void Print() {
    	PrintText print = new PrintText(source_view);
    }

    public Widget GetView()
    {
        return (Widget) source_view;
    }
}
