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
    bool dirty;
    MainWindow window;
    
    public TextDocument(MainWindow win, string fn, int page)
    {
	window = win;
        filename = fn;
	this.page = page;
	string mime_type = GetMimeType(filename);
        SourceLanguagesManager mgr = new SourceLanguagesManager();
	// FIXME: why does text/plain complain?
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
	    language = mgr.GetLanguageFromMimeType("text/html");
	    buffer = new SourceBuffer(language);
	    source_view.Buffer = buffer;
	    source_view.ShowLineNumbers = true;
	}
	source_view.Buffer.Changed += new EventHandler(OnSourceViewChanged);
	source_view.KeyPressEvent += new KeyPressEventHandler(OnKeyPress);
	if (filename != null) {
	    // TODO: make sure it exists, and can be read; readonly?
	    StreamReader file = File.OpenText(fn);
	    buffer.Text = file.ReadToEnd();
	    buffer.PlaceCursor(buffer.StartIter);
	} else {
	    filename = Utils.Tran("Untitled") + "-" + (this.page + 1) + ".py";
	}
	dirty = false;
    }

    [GLib.ConnectBefore]
    private void OnKeyPress(object obj, KeyPressEventArgs args) 
    {
	source_view.ScrollMarkOnscreen(buffer.InsertMark);
    }

    private void OnSourceViewChanged(object obj, EventArgs args) 
    {
	if (!dirty) {
	    window.SetDirty(page, true);
	}
	dirty = true;
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
	dirty = false;
	window.SetDirty(page, false);
    }
    
    public void SaveAs(string fn)
    {
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
	return dirty;
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

