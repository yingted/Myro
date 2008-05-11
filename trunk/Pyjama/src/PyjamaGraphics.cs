using Gtk;
using Mono.Unix;
using System;
using System.IO;
using System.Collections.Generic;

using PyjamaInterfaces;

namespace PyjamaGraphics
{
public class MainWindow: Window
{
    Notebook notebook;
    List <IDocument> documents = new List <IDocument>();

    public MainWindow(string[] args): base(Utils.Tran("Pyjama"))
    {
	// Initialize I18N: 
	Catalog.Init("pyjama", "./locale");

        DeleteEvent += file_exit;
        SetDefaultSize(600, 600);
        
        // Layout
	// +-------------------------------------------+
	// |Menu                                       |
	// + VBox -------------------------------------+
	// |[Tab1][Tab2]------------------------------+|
	// ||                                         ||
	// |+-----------------------------------------+|
	// |+Split------------------------------------+|
	// || Scrolled Window: Python Shell           ||
	// || >>>                                     ||
	// |+-----------------------------------------+|
	// +-------------------------------------------+
        VBox vbox = new VBox(false, 2);

	WindowMenuBar menubar = new WindowMenuBar(this);        
        vbox.PackStart(menubar, false, false, 0);

        VPaned split = new VPaned();
        split.Position = 400;

	notebook = new Notebook();

	if (args.Length > 0)
        {
	    foreach (string name in args)
            {
		// FIXME: if it can open it
		add_file(name); // adds a new empty page
            }
	} else {
	    // Add a blank page:
	    add_file(null); // adds a new empty page
        }

	// Add Notebook:
        split.Pack1(notebook, true, true);

	// Add shell:
        ScrolledWindow text_win = new ScrolledWindow();
	IShell shell = new PythonShell();
	text_win.Add(shell.GetView());
        text_win.HscrollbarPolicy = PolicyType.Automatic;
        text_win.VscrollbarPolicy = PolicyType.Automatic;
        split.Pack2(text_win, true, false);

	// Add notebook and shell:
	vbox.PackStart(split);

	// Add everything:
	Add(vbox);

        // This must be done after it has been added to all its containers.
        // Set the focus to the first notbook page editor.
        documents[0].GetView().GrabFocus();
    }
    
    public IDocument CurrentDocument
    {
        get
        {
            int page_num = notebook.CurrentPage;
            return documents[page_num];
        }
    }
    
    // Returns the document that is currently selected in the file list
    public void file_exit(object obj, EventArgs args)
    {
        //FIXME - Ask to abandon changes on all modified documents
        Application.Quit();
    }
    
    public void file_new(object obj, EventArgs args)
    {
	add_file(null);
    }

    public void SetDirty(int page, bool dirty) {
	Widget widget = notebook.GetNthPage(page);
	if (widget != null) {
	    string labelText = notebook.GetTabLabelText(widget);
            //FIXME - Use a real flag.  Filenames can actually start with '*'.
	    if (dirty) {
		if (!labelText.StartsWith("*")) {
		    notebook.SetTabLabelText(widget, "*" + labelText);
		}
	    } else {
		if (labelText.StartsWith("*")) {
		    notebook.SetTabLabelText(widget, labelText.Substring(1, 
							  labelText.Length - 1));
		}
	    }
	}
    }

    public void add_file(string filename)
    {
	IDocument textdoc;
	int page = notebook.NPages;
	// FIXME: select what type of IDocument to create:
	textdoc = new TextDocument(this, filename, page);
	// FIXME: why does the cursor not stay in view?
        ScrolledWindow sw = new ScrolledWindow();
	sw.Add(textdoc.GetView());
        sw.HscrollbarPolicy = PolicyType.Automatic;
        sw.VscrollbarPolicy = PolicyType.Automatic;
	notebook.AppendPage(sw, new Label(textdoc.GetShortName()));
	documents.Add(textdoc);
	notebook.ShowAll();
        notebook.Page = notebook.NPages - 1;
        textdoc.GetView().GrabFocus();
    }
    
    public void file_open(object obj, EventArgs args)
    {
        FileChooserDialog dlg = new FileChooserDialog(
				       "Open", this, FileChooserAction.Open,
				       "Cancel", ResponseType.Cancel,
				       "Open", ResponseType.Accept);
        if (dlg.Run() == (int)ResponseType.Accept)
        {
            // Open the file
            //FIXME - Error handling
	    if (notebook.NPages > 0)
            {
                IDocument last_doc = documents[notebook.NPages - 1];
		if (!last_doc.GetDirty() && last_doc.Untitled && last_doc.GetSize() == 0)
                {
                    int page = notebook.NPages - 1;
                    notebook.RemovePage(page);
                    foreach (IDocument doc in documents){
                        if (doc.GetPage() > page) {
                            doc.SetPage(doc.GetPage() - 1);
                        }
                    }
                    documents.RemoveAt(page);
                }
	    }
	    add_file(dlg.Filename);
        }
        dlg.Destroy();
    }
    
    public void file_save(object obj, EventArgs args)
    {
        file_save();
    }
    
    public bool file_save()
    {
	IDocument doc = CurrentDocument;
        if (doc.Untitled)
        {
            return file_save_as();
        } else {
            doc.Save();
            return true;
        }
    }
    
    public void file_save_as(object obj, EventArgs args)
    {
        file_save_as();
    }
    
    public bool file_save_as()
    {
        IDocument doc = CurrentDocument;
        FileChooserDialog dlg = new FileChooserDialog("Save As", this, 
						      FileChooserAction.Save,
						      "Cancel", ResponseType.Cancel,
						      "Save", ResponseType.Accept);
        bool ret = (dlg.Run() == (int)ResponseType.Accept);
        if (ret)
        {
	    doc.SaveAs(dlg.Filename);
	    notebook.SetTabLabelText(notebook.CurrentPageWidget, 
				     doc.GetShortName());
        }
        dlg.Destroy();
        
        return ret;
    }
    
    public void file_close(object obj, EventArgs args)
    {
        IDocument currentdoc = CurrentDocument;
        if (currentdoc.GetDirty())
        {
            // Save/Discard/Cancel prompt
            MessageDialog dlg = new MessageDialog(this, DialogFlags.Modal,
                MessageType.Question, ButtonsType.None,
                "The document \"{0}\" has been modified.\nDo you want to save or discard changes?",
                currentdoc.GetFilename());
            dlg.AddButton("Save", ResponseType.Accept);
            dlg.AddButton("Discard", ResponseType.Reject);
            dlg.AddButton("Cancel", ResponseType.Cancel);
            ResponseType ret = (ResponseType)dlg.Run();
            dlg.Destroy();
            
            if ((ret == ResponseType.Accept && !file_save()) || ret == ResponseType.Cancel || ret == ResponseType.DeleteEvent)
            {
                // Save failed or cancelled
                return;
            }
        }
	
        int page_num = notebook.CurrentPage;
        notebook.RemovePage(page_num);
	foreach (IDocument doc in documents){
	    if (doc.GetPage() > page_num) {
		doc.SetPage(doc.GetPage() - 1);
	    }
	}
	documents.RemoveAt(page_num);
	// This would prevent an empty page
	//if (notebook.NPages == 0)
	//    add_file(null);
	notebook.Show();
    }
}

public class WindowMenuBar: Gtk.MenuBar
{
    MainWindow window;
    
    public WindowMenuBar(MainWindow win)
    {
	window = win;
	
	Menu file_menu = new Menu();
	MenuItem file_item = new MenuItem(Utils.Tran("_File"));
	file_item.Submenu = file_menu;
	Append(file_item);
        
        MenuItem new_item = new MenuItem(Utils.Tran("_New"));
        new_item.Activated += window.file_new;
        file_menu.Append(new_item);
        
        MenuItem open_item = new MenuItem(Utils.Tran("_Open..."));
        open_item.Activated += window.file_open;
        file_menu.Append(open_item);
        
        MenuItem save_item = new MenuItem(Utils.Tran("_Save"));
        save_item.Activated += window.file_save;
        file_menu.Append(save_item);
        
        MenuItem save_as_item = new MenuItem(Utils.Tran("Save _As..."));
        save_as_item.Activated += window.file_save_as;
        file_menu.Append(save_as_item);
        
        MenuItem close_item = new MenuItem(Utils.Tran("_Close"));
        close_item.Activated += window.file_close;
        file_menu.Append(close_item);
        
        file_menu.Append(new SeparatorMenuItem());
        
        MenuItem exit_item = new MenuItem(Utils.Tran("E_xit"));
        exit_item.Activated += window.file_exit;
        file_menu.Append(exit_item);
    }
}
} // end PyjamaGraphics namespace