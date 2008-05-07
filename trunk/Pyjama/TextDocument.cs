using Gtk;
using GtkSourceView;
using System.IO;
using System;
using System.Collections.Generic;

using PyjamaInterfaces;
using PyjamaGraphics;

public class TextDocument: PyjamaInterfaces.IDocument
{
    string filename;
    SourceBuffer buffer;
    SourceLanguage language;
    SourceView source_view;
    int page;
    bool untitled;
    MainWindow window;
    
    public TextDocument(MainWindow win, string fn, int page)
    {
	window = win;
        filename = fn;
	this.page = page;
	string mime_type = GetMimeType(filename);
        SourceLanguagesManager mgr = new SourceLanguagesManager();
	// mgr.LangFilesDirs.Append("./language-specs");
	source_view = new SourceView();
	if (mime_type != "text/plain") {
	    language = mgr.GetLanguageFromMimeType(mime_type);
	    buffer = new SourceBuffer(language);
	    buffer.Highlight = true;
	    source_view.Buffer = buffer;
	    // Options should be set by user:
	    source_view.WrapMode = Gtk.WrapMode.Word;
	    source_view.ShowLineNumbers = true;
	    source_view.AutoIndent = true;
	} else {
            // Plain text doesn't need any highlighting.
	    // I think this causes a Gtk assert issue if not set.
	    language = mgr.GetLanguageFromMimeType("text/x-changelog");
            buffer = new SourceBuffer(language);
	    buffer.Highlight = false;
	    source_view.Buffer = buffer;
	    // Options should be set by user:
	    source_view.WrapMode = Gtk.WrapMode.Word;
	    source_view.ShowLineNumbers = false;
	    source_view.AutoIndent = false;
	}
	// Change the font to fixed-width:
	Pango.FontDescription font = new Pango.FontDescription();
	font.Family = "Monospace";
	source_view.ModifyFont(font);

	// Set up event handlers:
	source_view.Buffer.Changed += new EventHandler(OnSourceViewChanged);
	source_view.KeyPressEvent += new KeyPressEventHandler(OnKeyPressBefore);
	
	if (filename != null) {
	    // TODO: make sure it exists, and can be read; readonly?
	    StreamReader file = File.OpenText(fn);
	    buffer.BeginNotUndoableAction();
	    buffer.Text = file.ReadToEnd();
	    buffer.EndNotUndoableAction();
	    buffer.PlaceCursor(buffer.StartIter);
            untitled = false;
	    file.Close();
	} else {
	    filename = Utils.Tran("Untitled") + "-" + (this.page + 1) + ".py";
            untitled = true;
	}
    }


    [GLib.ConnectBefore]
    private void OnKeyPressBefore(object obj, KeyPressEventArgs args) 
    {
	// FIXME:
	// This isn't correct as they should happen after the change
	// but source_view.KeyPressEvent eats the signal, and won't
	// call any other events, and CanUndoFired crashes Windows
	window.SetDirty(page, buffer.CanUndo());
	source_view.ScrollMarkOnscreen(buffer.InsertMark);
    }

    private void OnSourceViewChanged(object obj, EventArgs args) 
    {
	window.SetDirty(page, buffer.CanUndo());
	source_view.ScrollMarkOnscreen(buffer.InsertMark);
    }
    
    public bool Untitled
    {
        get
        {
            return untitled;
        }
    }
    
    public Widget GetView()
    {
	return (Widget) source_view;
    }

    public string GetShortName()
    {
	// Path.GetDirectoryName()
	string name = System.IO.Path.GetFileName(filename);
	if (name == "__init__.py") {
	    // add directory onto name
	    char [] c = new char[1];
	    c[0] = Path.DirectorySeparatorChar;
	    string [] path = filename.Split(c,
					    StringSplitOptions.RemoveEmptyEntries);
	    if (path.Length > 1)
		name = path[path.Length - 2] + Path.DirectorySeparatorChar + name;
	}
	return name;
    }
    
    public bool GetModified()
    {
	return buffer.Modified;
    }
        
    public void SetModified(bool value)
    {
	buffer.Modified = value;
    }
    
    public void Save()
    {
        StreamWriter file = new StreamWriter(filename);
        file.Write(buffer.Text);
        file.Close();
	buffer.Modified = false;
	window.SetDirty(page, false);
	// Forces no redo past this point
	//buffer.BeginNotUndoableAction(); 
	//buffer.EndNotUndoableAction();
    }
    
    public void SaveAs(string fn)
    {
	if (File.Exists(fn)) {
            MessageDialog dlg = new MessageDialog(window, DialogFlags.Modal,
                MessageType.Question, ButtonsType.None,
                "Do you want to overwrite \"{0}\"?", fn);
            dlg.AddButton("Overwrite", ResponseType.Accept);
            dlg.AddButton("Cancel", ResponseType.Cancel);
            ResponseType ret = (ResponseType)dlg.Run();
            dlg.Destroy();
            if (ret == ResponseType.Cancel) {
		return;
	    }
	} 
	untitled = false;
	filename = fn;
	string mime_type = GetMimeType(filename);
	SourceLanguagesManager mgr = new SourceLanguagesManager();
	language = mgr.GetLanguageFromMimeType(mime_type);
	if (buffer.Language != language && language != null) {
	    buffer.Language = language;
	}
	Save();
    }

    public void SetFilename(string fn)
    {
    	filename = fn;
    }

    public string GetFilename()
    {
    	return filename;
    }

    string GetMimeType(string filename) {
	if (filename != null) {
	    string extension = System.IO.Path.GetExtension(filename);
	    return Utils.GetMimeType(extension);
	} else {
	    return "text/x-python";
	}
    }

    public bool GetDirty() {
	return buffer.CanUndo();
    }

    public int GetPage() {
	return page;
    }

    public void SetPage(int value) {
	page = value;
    }
    
    public int GetSize() {
    	return buffer.CharCount;
    }
}

