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
	source_view = new SourceView();
	source_view.Buffer = buffer;
	source_view.Buffer.InsertText += new Gtk.InsertTextHandler(OnSourceViewChanged);
	source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
	source_view.Buffer.Text = "\"\"\"IronPython console: IronPython 2.0A5 (2.0.11011.00) on .NET 2.0.50727.42\nCopyright (c) Microsoft Corporation. All rights reserved.\"\"\"\n>>> ";
    }

    private void OnSourceViewChanged(object obj, Gtk.InsertTextArgs args) 
    {
	// FIXME: on enter, evaluate line
	// Need to handle keypress events
	// KeyPressEventArgs a = (KeyPressEventArgs) args;
	if (args.Text == "\n") {
	    Console.WriteLine("eval");
	}
	/* Code I have written for a CPython + Gtk Evaluator
        if (event.keyval == gtk.keysyms.Home or
            ((event.keyval == gtk.keysyms.a and 
              event.get_state() & gtk.gdk.CONTROL_MASK))): 
            buffer = widget.get_buffer()
            cursor_pos = buffer.get_property("cursor-position")
            iter = buffer.get_iter_at_offset(cursor_pos)
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

