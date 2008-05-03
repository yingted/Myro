using System;
using System.IO;
using Gtk;
using GtkSourceView;
using Mono.Unix;

using PyjamaInterfaces;

public class Document: PyjamaInterfaces.IDocument
{
    public SourceBuffer buffer;
    public string filename;
    public Window window;
    
    public static SourceLanguage language;
    
    public Document(Window win)
    {
	buffer = new SourceBuffer(language);
        buffer.Highlight = true;
        window = win;
    }
    
    public Document(Window win, string fn)
    {
        buffer = new SourceBuffer(language);
        buffer.Highlight = true;
        filename = fn;
        window = win;
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
    
    // Delegate wrappers
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
}

public class Utils
{
    public static string _(string msg) {
	return Catalog.GetString(msg);
    }

    public static string _s(string msg1, string msg2, int count) {
	return Catalog.GetPluralString(msg1, msg2, count);
    }
}

public class WindowMenuBar: Gtk.MenuBar
{
    Test window;
    
    public WindowMenuBar(Test win)
    {
	window = win;
	
	Menu file_menu = new Menu();
	MenuItem file_item = new MenuItem(Utils._("_File"));
	file_item.Submenu = file_menu;
	Append(file_item);
        
        MenuItem new_item = new MenuItem("_New");
        new_item.Activated += window.file_new;
        file_menu.Append(new_item);
        
        MenuItem open_item = new MenuItem("_Open...");
        open_item.Activated += window.file_open;
        file_menu.Append(open_item);
        
        MenuItem save_item = new MenuItem("_Save");
        save_item.Activated += window.file_save;
        file_menu.Append(save_item);
        
        MenuItem save_as_item = new MenuItem("Save _As...");
        save_as_item.Activated += window.file_save_as;
        file_menu.Append(save_as_item);
        
        MenuItem close_item = new MenuItem("_Close");
        close_item.Activated += window.file_close;
        file_menu.Append(close_item);
        
        file_menu.Append(new SeparatorMenuItem());
        
        MenuItem exit_item = new MenuItem("E_xit");
        exit_item.Activated += window.file_exit;
        file_menu.Append(exit_item);
    }
}

public class Test: Window
{
    ListStore file_list_store;
    SourceView source_view;
    TreeView file_list;
    
    public Test(): base("Pyjama")
    {
	// Initialize I18N: 
	Catalog.Init("pyjama", "./locale");

        // Set up syntax highlighting
        SourceLanguagesManager mgr = new SourceLanguagesManager();
        Document.language = mgr.GetLanguageFromMimeType("text/x-python");
        
        DeleteEvent += file_exit;
        SetDefaultSize(800, 600);
        
        // Layout
        VBox vbox = new VBox(false, 2);
        Add(vbox);
        
        HPaned split = new HPaned();
        split.Position = 150;

	WindowMenuBar menubar = new WindowMenuBar(this);        
        vbox.PackStart(menubar, false, false, 0);

        // File list
        file_list_store = new ListStore(typeof(Document));
        
        file_list = new TreeView();
        file_list.Model = file_list_store;
        file_list.HeadersVisible = false;
        file_list.CursorChanged += select_file;
	# MONO ON WIN: needed to cast the TreeCellDataFunc function
        file_list.AppendColumn("Filename", 
			       new CellRendererText(), 
			       ((Gtk.TreeCellDataFunc)document_filename));
        
        ScrolledWindow file_list_win = new ScrolledWindow();
        file_list_win.Add(file_list);
        split.Pack1(file_list_win, false, false);
        
        // Editor
        ScrolledWindow text_win = new ScrolledWindow();
        text_win.HscrollbarPolicy = PolicyType.Always;
        text_win.VscrollbarPolicy = PolicyType.Always;
        
        source_view = new SourceView();
        source_view.ShowLineNumbers = true;
        source_view.AutoIndent = true;
        text_win.Add(source_view);
        split.Pack2(text_win, true, false);
        
        vbox.PackStart(split);
        
        // Set the focus to the editor.
        // This must be done after it has been added to all its containers.
        source_view.GrabFocus();
        
        file_new(null, null);
    }
    
    // Returns the document that is currently selected in the file list
    Document current_document()
    {
        TreeIter iter;
        file_list.Selection.GetSelected(out iter);
        return (Document)file_list_store.GetValue(iter, 0);
    }
    
    // Called when the selected file in the file list changes.
    void select_file(object obj, EventArgs args)
    {
        Document doc = current_document();
        
        if (doc != null)
        {
            source_view.Buffer = doc.buffer;
        }
    }
    
    // Sets the text in a CellRendererText to a Document's filename.
    void document_filename(TreeViewColumn tree_column, 
	CellRenderer cell, TreeModel tree_model, TreeIter iter)
    {
        Document doc = (Document)tree_model.GetValue(iter, 0);
        
        string filename = doc.GetShortName();
        if (filename == null)
        {
            filename = "(Untitled)";
        }
        
        ((CellRendererText)cell).Text = filename;
    }
    
    public void file_new(object obj, EventArgs args)
    {
        TreeIter iter = file_list_store.AppendValues(new Document(this));
        
        // Select the new item
        TreePath path = file_list_store.GetPath(iter);
        file_list.SetCursor(path, null, false);
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
            string filename = dlg.Filename;
            StreamReader file = File.OpenText(filename);
            
            // Load into the document's buffer
            //FIXME - Error handling
            Document doc = new Document(this, filename);
            doc.buffer.Text = file.ReadToEnd();
            
            // If the only document is empty, remove it
            TreeIter iter;
            if (file_list_store.IterNChildren() == 1)
            {
                file_list_store.GetIterFirst(out iter);
                Document old_doc = (Document)file_list_store.GetValue(iter, 0);
                if (old_doc != null && old_doc.filename == null && old_doc.buffer.CharCount == 0)
                {
                    file_list_store.Remove(ref iter);
                }
            }
            
            // Add this document to the list
            iter = file_list_store.AppendValues(doc);
            
            // Select the new item
            TreePath path = file_list_store.GetPath(iter);
            file_list.SetCursor(path, null, false);
        }
        dlg.Destroy();
    }
    
    // Delegate wrappers
    public void file_save(object obj, EventArgs args)
    {
        Document doc = current_document();
        doc.Save();
    }
    
    public void file_save_as(object obj, EventArgs args)
    {
	file_save_as();
    }
    
    // Saves the current file, prompting for a name if necessary.
    // Returns true on success or false on failure or cancellation.
    public bool file_save()
    {
        Document doc = current_document();
        
        if (doc.filename == null)
        {
            // Ask for a filename and save.
            // This will call file_save() again when the filename is set.
            return file_save_as();
        }

	doc.Save();
        
        return true;
    }
    
    public bool file_save_as()
    {
        FileChooserDialog dlg = new FileChooserDialog("Save As", this, FileChooserAction.Save,
            "Cancel", ResponseType.Cancel,
            "Save", ResponseType.Accept);
        
        bool ret = (dlg.Run() == (int)ResponseType.Accept);
        if (ret)
        {
            Document doc = current_document();
            doc.filename = dlg.Filename;
            //FIXME - Update the list
            
            if (!file_save())
            {
                ret = false;
            }
        }
        dlg.Destroy();
        
        return ret;
    }
    
    public void file_close(object obj, EventArgs args)
    {
        // Save if necessary
        Document doc = current_document();
        if (doc.GetModified())
        {
            // Save/Discard/Cancel prompt
            MessageDialog dlg = new MessageDialog(this, DialogFlags.Modal,
                MessageType.Question, ButtonsType.None,
                "The document \"{0}\" has been modified.\nDo you want to save or discard changes?",
                doc.filename);
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
        
        // Remove the document from the list
        TreeIter iter;
        file_list.Selection.GetSelected(out iter);
        file_list_store.Remove(ref iter);
        
        // FIXME - Select another file
        
        // Create a new file if there aren't any left
        if (file_list_store.IterNChildren() == 0)
        {
            file_new(null, null);
        }
    }
    
    public void file_exit(object obj, EventArgs args)
    {
        Application.Quit();
    }
    
    public static void Main()
    {
        Application.Init();
        
        Test t = new Test();
        t.ShowAll();
        
        Application.Run();
    }
}
