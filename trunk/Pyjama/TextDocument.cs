using Gtk;
using GtkSourceView;
using System.IO;

using PyjamaInterfaces;

public class TextDocument: PyjamaInterfaces.IDocument
{
    string filename;
    SourceBuffer buffer;
    SourceLanguage language;
    SourceView source_view;
    
    public TextDocument()
    {
	Init(null);
    }
    
    public TextDocument(string fn)
    {
	// TODO: get mime type from file or filename
	Init(null);
	StreamReader file = File.OpenText(fn);
	buffer.Text = file.ReadToEnd();
	buffer.PlaceCursor(buffer.StartIter);
        filename = fn;
    }

    void Init(string mime_type)
    {
	if (mime_type == null)
	    mime_type = "text/x-python";
        SourceLanguagesManager mgr = new SourceLanguagesManager();
	language = mgr.GetLanguageFromMimeType(mime_type);
        // Set up syntax highlighting
	buffer = new SourceBuffer(language);
        buffer.Highlight = true;
	source_view = new SourceView();
	source_view.WrapMode = Gtk.WrapMode.Word;
	source_view.Buffer = buffer;
	// Options should be set by user:
        source_view.ShowLineNumbers = true;
        source_view.AutoIndent = true;
    }
    
    public Widget GetView()
    {
	return (Widget) source_view;
    }

    public string GetShortName()
    {
	// this only works if the file already exists
        if (filename != null) {
	    FileInfo info = new FileInfo(filename);
	    return info.Name;
	} else {
	    return null;
        }
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
    }
    
    public void SaveAs(string fn)
    {
    	filename = fn;
        Save();
    }

    public void SetFilename(string fn)
    {
    	filename = fn;
    }
}

