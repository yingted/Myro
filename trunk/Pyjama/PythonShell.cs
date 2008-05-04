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
	source_view.Buffer.Changed += new EventHandler(OnSourceViewChanged);
	source_view.WrapMode = Gtk.WrapMode.Word;
        source_view.AutoIndent = true;
	source_view.Buffer.Text = "\"\"\"IronPython console: IronPython 2.0A5 (2.0.11011.00) on .NET 2.0.50727.42\nCopyright (c) Microsoft Corporation. All rights reserved.\"\"\"\n>>> ";
    }

    private void OnSourceViewChanged(object obj, EventArgs args) 
    {
	// FIXME: on enter, evaluate line
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

