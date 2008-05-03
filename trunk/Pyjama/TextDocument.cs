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
	Init();
    }
    
    public TextDocument(string fn)
    {
	Init();
	StreamReader file = File.OpenText(fn);
	buffer.Text = file.ReadToEnd();
        filename = fn;
    }

    void Init()
    {
        SourceLanguagesManager mgr = new SourceLanguagesManager();
	language = mgr.GetLanguageFromMimeType("text/x-python");
        // Set up syntax highlighting
	buffer = new SourceBuffer(language);
        buffer.Highlight = true;
	source_view = new SourceView();
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

