using Gtk;
using GtkSourceView;
using System.IO;
using System;

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

    private void OnSourceViewChanged(object obj, EventArgs args) 
    {
	Console.WriteLine("view change: {0}, page: {1}, dirty: {2}", filename, page, dirty);
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
	if (name == "__init__") {
	    // add directory onto name
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

