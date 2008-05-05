using Gtk;
using Gdk;
using GtkSourceView;
using System.IO;
using System;

using PyjamaInterfaces;
using PyjamaGraphics;

public class PythonShell: PyjamaInterfaces.IShell
{
    SourceBuffer buffer;
    SourceLanguage language;
    SourceView source_view;
    
    public PythonShell()
    {
        SourceLanguagesManager mgr = new SourceLanguagesManager();
	language = mgr.GetLanguageFromMimeType("text/x-python");
        // Set up syntax highlighting
	buffer = new SourceBuffer(language);
        buffer.Highlight = true;
	source_view = new SourceView(buffer);
	source_view.KeyPressEvent += new KeyPressEventHandler(OnSourceViewChanged);
	source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
	buffer.Text = "\"\"\"IronPython 2.0A5 (2.0.11011.00) on .NET 2.0.50727.42\nCopyright (c) Microsoft Corporation. All rights reserved.\"\"\"\n>>> ";
    }

    // To get in the loop before the SourceView handles the keypress
    // you need this directive:
    [GLib.ConnectBefore]
    private void OnSourceViewChanged(object obj, KeyPressEventArgs args) 
    {
	TextIter iter, start, end;
	int line_cnt;
	// Had to cast as int on Windows for Gdk.Key and bit mask comparison
	if ((int)(args.Event.Key) == (int)(Gdk.Key.Return)) {
	    // FIXME: this is rough, but gives examples of most of
	    // what we'll need
            int cursor_pos = buffer.CursorPosition;
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
	    Console.WriteLine("[evaluate: '{0}']", line);
            //_retval = self.process_command(line)
            //if _retval != None:
	    buffer.Text += "Ok\n>>> ";
            end = buffer.EndIter;
            buffer.PlaceCursor(end);
	    args.RetVal = true;
	} else if (((int)(args.Event.Key) == (int)(Gdk.Key.Home)) || 
		   (((int)(args.Event.Key) == (int)(Gdk.Key.a)) && 
		    (((int)(args.Event.State) & (int)(Gdk.ModifierType.ControlMask)) != 0))) {
            int cursor_pos = buffer.CursorPosition;
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

