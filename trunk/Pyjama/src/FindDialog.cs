using Gtk;
using Glade;

using System;

class FindDialog
{
    [Widget] Window find_dialog;
    [Widget] Entry find_text;
    [Widget] Button find_button;
    
    TextBuffer buffer;
    
    // Next position for find.
    int find_pos = 0;
    
    PyjamaGUI gui;
    
    public FindDialog(PyjamaGUI g)
    {
        gui = g;
    }
    
    public void Run(TextBuffer b)
    {
        buffer = b;
        if (find_dialog == null)
        {
            XML ui = new XML("main-default.glade", "find_dialog", null);
            ui.Autoconnect(this);
#if NEWSOURCEVIEW
            find_pos = buffer.CursorPosition;
#else
	TextMark mark = buffer.InsertMark;
	TextIter iter = buffer.GetIterAtMark(mark);
	find_pos = iter.Offset;
#endif
        }
    }
    
    public void Find_Delete(object obj, DeleteEventArgs args)
    {
        find_dialog = null;
    }
    
    public void Find_Close(object obj, EventArgs args)
    {
        find_dialog.Destroy();
        find_dialog = null;
    }
    
    public void Find_Text_Changed(object obj, EventArgs args)
    {
        find_button.Sensitive = (find_text.Text.Length > 0);
    }
    
    public void Find_Execute(object obj, EventArgs args)
    {
        string find = find_text.Text;
        if (find.Length == 0)
        {
            // Nothing to find
            return;
        }
        
        string text = buffer.Text;
        int pos = text.IndexOf(find, find_pos);
        
        if (pos < 0)
        {
            // Not found
            MessageDialog dlg = new MessageDialog(gui.MainWindow, DialogFlags.Modal,
                MessageType.Info, ButtonsType.Ok, "No more occurrences found.");
            dlg.Run();
            dlg.Destroy();
            
            // Start searching from the beginning
            find_pos = 0;
        } else {
            TextIter start = buffer.GetIterAtOffset(pos);
            TextIter end = start;
            end.ForwardChars(find.Length);
            buffer.SelectRange(start, end);
            find_pos = pos + find.Length;
        }
    }
}
