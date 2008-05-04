using Gtk;
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
	switch (args.Event.Key) {
	case Gdk.Key.Return:
	    // FIXME: this is rough, but gives examples of most of
	    // what we'll need
            int cursor_pos = buffer.CursorPosition;
	    TextIter iter = buffer.GetIterAtOffset(cursor_pos);
	    int line_cnt = iter.Line;
            bool echo = false;
            TextIter start = buffer.GetIterAtLine(line_cnt);
            int line_len = iter.CharsInLine;
            int buffer_cnt = buffer.LineCount;
	    Console.WriteLine("diff: " + (buffer_cnt - line_cnt));
            if ((buffer_cnt - line_cnt) > 1) {
                line_len -= 1;
                echo = true;
	    }
            TextIter end = buffer.GetIterAtLineOffset(line_cnt, line_len);
            string line = buffer.GetText(start, end, false);
            buffer.Text += "\n";
	    Console.WriteLine("line: '{0}'", line);
            if (line.StartsWith(">>>")) {
		line = line.Substring(1, line.Length - 1).Trim();
	    } else {
		buffer.Text += ">>> ";
                end = buffer.EndIter;
		buffer.PlaceCursor(end);
		args.RetVal = true;
		return;
	    }
            if (echo) {
		buffer.Text += ">>> " + line;
		end = buffer.EndIter;
                buffer.PlaceCursor(end);
		args.RetVal = true;
		return;
	    }
            //_retval = self.process_command(line)
            //if _retval != None:
	    buffer.Text += "Ok\n>>> ";
            end = buffer.EndIter;
            buffer.PlaceCursor(end);
	    args.RetVal = true;
	    return;
	default:
	    break;
	}
	/* Some code I have written for a CPython + Gtk application.
            line_cnt = iter.get_line()
            start = buffer.get_iter_at_line(line_cnt)
            start.forward_chars(2)
            buffer.place_cursor(start)
            return True
        elif (event.keyval == gtk.keysyms.End or 
              (event.keyval == gtk.keysyms.e and 
               event.get_state() & gtk.gdk.CONTROL_MASK)): 
            buffer = widget.get_buffer()
            end = buffer.get_end_iter()
            buffer.place_cursor(end)
            return True
        elif event.keyval == gtk.keysyms.Return: 
            echo = False
            buffer = widget.get_buffer()
            cursor_pos = buffer.get_property("cursor-position")
            iter = buffer.get_iter_at_offset(cursor_pos)
            line_cnt = iter.get_line()
            start = buffer.get_iter_at_line(line_cnt)
            line_len = iter.get_chars_in_line()
            buffer_cnt = buffer.get_line_count()
            if (buffer_cnt - line_cnt) > 1:
                line_len -= 1
                echo = True
            end = buffer.get_iter_at_line_offset(line_cnt, line_len)
            line = buffer.get_text(start, end)
            self.append_text("\n")
            if line.startswith(self.prompt):
                line = line[1:].strip()
            else:
                self.append_text("%s " % self.prompt)
                end = buffer.get_end_iter()
                buffer.place_cursor(end)
                return True
            if echo:
                self.append_text(("%s " % self.prompt) + line)
                end = buffer.get_end_iter()
                buffer.place_cursor(end)
                return True
            _retval = self.process_command(line)
            if _retval != None:
                self.append_text("%s\n" % str(_retval))
            self.append_text("%s " % self.prompt)
            end = buffer.get_end_iter()
            buffer.place_cursor(end)
            return True
	*/
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

