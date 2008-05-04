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
    MainWindow window;
    
    public PythonShell(MainWindow win)
    {
	window = win;
        SourceLanguagesManager mgr = new SourceLanguagesManager();
	language = mgr.GetLanguageFromMimeType("text/x-python");
        // Set up syntax highlighting
	buffer = new SourceBuffer(language);
        buffer.Highlight = true;
	source_view = new SourceView();
	source_view.Buffer = buffer;
	source_view.Buffer.Changed += new EventHandler(OnSourceViewChanged);
	source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
	source_view.Buffer.Text = "IronPython, (c) 2008 Microsoft Corp.\n" +
	    ">>> ";
    }

    private void OnSourceViewChanged(object obj, EventArgs args) 
    {
    }
    
    public void ExecuteFile(string filename)
    {
    }

    public void EvaluateExp(string expression)
    {
    }

    public void Restart()
    {
    }

    public void Quit()
    {
    }

    public Widget GetView()
    {
	return (Widget) source_view;
    }
}

