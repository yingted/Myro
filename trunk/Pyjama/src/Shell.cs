using System;
using System.IO;
using System.Collections.Generic;
using System.Threading;

public class TextBufferOutputStream: Stream
{
  //TextBuffer buffer;
    
  public TextBufferOutputStream() //TextBuffer b)
    {
	  //buffer = b;
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
        
        //TextIter iter = buffer.EndIter;
        //buffer.Insert(ref iter, str.Substring(0, count));
    }
    
    override public long Seek(long offset, SeekOrigin origin)
    {
        return 0;
    }
    
    override public void SetLength(long length)
    {
    }
}

// //FIXME - Don't let undo remove output
// public class ShellSourceView: SourceView
// {
//     string prompt = "# ";
    
//     // Where the command begins in the buffer (immediately after the prompt)
//     TextMark command_start;
    
//     // Command history.
//     // history is never empty.  The last entry is the current command.
//     List<string> history = new List<string>();
//     int history_pos = 0;
    
//     public delegate void ExecuteHandler(string line);
//     public event ExecuteHandler Execute;
    
//     public ShellSourceView(SourceBuffer buf, string prompt): base(buf)
//     {
// 	this.prompt = prompt;
//         KeyPressEvent += new KeyPressEventHandler(OnKey);
//         MoveCursor += new MoveCursorHandler(OnMove);
//     }
    
//     public TextMark CommandStart
//     {
//         get { return command_start; }
//     }
    
//     public void AppendText(string str)
//     {
//         TextIter iter = Buffer.EndIter;
//         Buffer.Insert(ref iter, str);
//     }
    
//     protected override void OnBackspace()
//     {
//         TextIter start_iter = Buffer.GetIterAtMark(command_start);
//         int start_pos = start_iter.Offset;
// #if NEWSOURCEVIEW
//         int cur_pos = Buffer.CursorPosition;
// #else
// 	TextMark mark = Buffer.InsertMark;
// 	TextIter iter = Buffer.GetIterAtMark(mark);
// 	int cur_pos = iter.Offset;
// #endif
//         if (cur_pos > start_pos)
//         {
//             base.OnBackspace();
//         }
//     }
    
//     public void AddPrompt()
//     {
//         // Append the prompt to the buffer
//         AppendText(prompt);
//         Buffer.PlaceCursor(Buffer.EndIter);
        
//         // Move the start-of-command mark
//         if (command_start == null)
//         {
//             command_start = Buffer.CreateMark(null, Buffer.EndIter, true);
//         } else {
//             Buffer.MoveMark(command_start, Buffer.EndIter);
//         }
        
//         // Reset the history position
//         //FIXME - This is hazardous.
//         history.Add("");
//         history_pos = history.Count - 1;
//     }

//     // Returns true if the cursor is in a position where the command can be edited.
//     public bool CanEdit()
//     {
//         TextIter start_iter = Buffer.GetIterAtMark(command_start);
//         int start_pos = start_iter.Offset;
// #if NEWSOURCEVIEW
//         return Buffer.CursorPosition >= start_pos;
// #else
// 	TextMark mark = Buffer.InsertMark;
// 	TextIter iter = Buffer.GetIterAtMark(mark);
// 	return iter.Offset >= start_pos;
// #endif
//     }

//     protected override bool OnButtonPressEvent(Gdk.EventButton e)
//     {
//         bool ret = base.OnButtonPressEvent(e);
//         Editable = CanEdit();
//         return ret;
//     }
    
//     void OnMove(object obj, MoveCursorArgs args)
//     {
//         // args.RetVal=true doesn't stop the move, so we have to catch it afterwards
//         // and put the cursor back at the beginning of the line.
//         TextIter start_iter = Buffer.GetIterAtMark(command_start);
//         int start_pos = start_iter.Offset;
// #if NEWSOURCEVIEW
// 	int cur_pos = Buffer.CursorPosition;
// #else
// 	TextMark mark = Buffer.InsertMark;
// 	TextIter iter = Buffer.GetIterAtMark(mark);
// 	int cur_pos = iter.Offset;
// #endif
//         if (cur_pos < start_pos)
//         {
//             Buffer.PlaceCursor(start_iter);
//         }
//     }

//     // To get in the loop before the SourceView handles the keypress
//     // you need this directive:
//     [GLib.ConnectBefore]
//     void OnKey(object obj, KeyPressEventArgs args) 
//     {
//         // Had to cast as int on Windows for Gdk.Key and bit mask comparison
//         int key = (int)args.Event.Key;
        
//         if (key == (int)Gdk.Key.Return || key == (int)Gdk.Key.KP_Enter) {
//             args.RetVal = true;
//             string line = Command;
//             AppendText("\n");
            
//             // Ignore blank lines
//             if (line == "")
//             {
//                 AddPrompt();
//                 return;
//             }
            
//             // FIXME - Add this command to history unless we were executing exactly the last command.
//             //  (So XYZ, (up), (up), (up) doesn't produce four copies of XYZ in the history.)
//             history[history.Count - 1] = line;
            
//             Execute(line);
            
// //            AddPrompt();
//         } else if (key == (int)(Gdk.Key.Up)) {
//             args.RetVal = true;
//             if (history_pos > 0)
//             {
//                 // Save the current command if it's the last one
//                 if (history_pos == history.Count - 1)
//                 {
//                     history[history.Count - 1] = Command;
//                 }
                
//                 history_pos--;
//                 Command = history[history_pos];
//             }
//         } else if (key == (int)(Gdk.Key.Down)) {
//             args.RetVal = true;
//             if (history_pos < (history.Count - 1))
//             {
//                 history_pos++;
//                 Command = history[history_pos];
//             }
//         } else if (key == (int)(Gdk.Key.Home)) {
//             //FIXME - Shift+HOME
//             args.RetVal = true;
//             Buffer.PlaceCursor(Buffer.GetIterAtMark(command_start));
//         } else {
//             // In case there's a key combination I forgot...
//             Editable = CanEdit();
//         }
        
//         ScrollMarkOnscreen(Buffer.InsertMark);
//     }
    
//     public string Command
//     {
//         get
//         {
//             TextIter start = Buffer.GetIterAtMark(command_start);
//             return Buffer.GetText(start, Buffer.EndIter, false);
//         }
        
//         set
//         {
//             // Delete the current command
//             TextIter start = Buffer.GetIterAtMark(command_start);
//             TextIter end = Buffer.EndIter;
//             Buffer.Delete(ref start, ref end);
            
//             // Add the new command
//             AppendText(value);
            
//             // Put the cursor at the end of the line
//             Buffer.PlaceCursor(Buffer.EndIter);
//         }
//     }
// }

abstract public class BaseShell {
  //protected SourceBuffer buffer;
  //protected SourceLanguage language;
  //protected ShellSourceView source_view;
    protected string command_line;
    protected string languageName;
    protected string languageMime;
  //protected SourceLanguagesManager mgr;
    
    public BaseShell()
    {
    }
    
    public void setProgrammingLanguage(string languageName, 
				       string languageMime) {
	this.languageName = languageName;
	this.languageMime = languageMime;
    }
    
    public abstract string command(string line);

    void do_exec()
    {
        // Evaluate or execute
	  //source_view.AppendText(command(command_line));
        
        // Don't give a newline unless we need one:
//         if (iter.CharsInLine != 0) {
//             source_view.AppendText("\n");
//         }
//         source_view.AddPrompt();
    }

    protected void execute(string line)
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
	//PrintText print = new PrintText(source_view);
  }

  //    public Widget GetView()
  //{
  //return (Widget) source_view;
  //}
}

