using System;
using System.IO;
using Mono.Unix;
using Gtk;

using PyjamaInterfaces;

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

public class MainWindow: Window
{
    Notebook notebook;

    public MainWindow(): base(Utils.Tran("Pyjama"))
    {
	// Initialize I18N: 
	Catalog.Init("pyjama", "./locale");

        DeleteEvent += file_exit;
        SetDefaultSize(800, 600);
        
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
        split.Position = 800;

	notebook = new Notebook();

	// Add a page:
        file_new(null, null); // adds a new empty page

	// Add Notebook:
        split.Pack1(notebook, false, false);

	// Add shell:
        ScrolledWindow text_win = new ScrolledWindow();
        text_win.HscrollbarPolicy = PolicyType.Automatic;
        text_win.VscrollbarPolicy = PolicyType.Automatic;
        split.Pack2(text_win, true, false);

	// Add Notebook/shell:
	vbox.PackStart(split);

	// Add everything:
	Add(vbox);

        // This must be done after it has been added to all its containers.
        // Set the focus to the first notbook page editor.
        notebook.CurrentPageWidget.GrabFocus();
    }
    
    // Returns the document that is currently selected in the file list
    public void file_exit(object obj, EventArgs args)
    {
        Application.Quit();
    }
    
    public static void Main()
    {
        Application.Init();
        MainWindow mainwin = new MainWindow();
        mainwin.ShowAll();
        Application.Run();
    }

    public void file_new(object obj, EventArgs args)
    {
	Console.WriteLine("You have selected file_new.");
	add_file(null);
    }

    public void add_file(string filename)
    {
	TextDocument textdoc;
	int pages = notebook.NPages + 1;
	if (filename == null) {
	    filename = Utils.Tran("Untitled") + "-" + pages + ".py";
	    textdoc = new TextDocument();
	} else {
	    textdoc = new TextDocument(filename);
	}
        ScrolledWindow page = new ScrolledWindow();
	page.AddWithViewport(textdoc.GetView());
        page.HscrollbarPolicy = PolicyType.Automatic;
        page.VscrollbarPolicy = PolicyType.Automatic;
	notebook.AppendPage(page, new Label(filename));
        notebook.Page = notebook.NPages - 1;
	page.ShowAll();
    }
    
    public void file_open(object obj, EventArgs args)
    {
	Console.WriteLine("You have selected file_open.");
        FileChooserDialog dlg = new FileChooserDialog(
				       "Open", this, FileChooserAction.Open,
				       "Cancel", ResponseType.Cancel,
				       "Open", ResponseType.Accept);
        if (dlg.Run() == (int)ResponseType.Accept)
        {
            // Open the file
            //FIXME - Error handling
	    add_file(dlg.Filename);
        }
        dlg.Destroy();
    }
    
    public void file_save(object obj, EventArgs args)
    {
	int page_num = notebook.CurrentPage;
	Console.WriteLine("You have selected file_save page {0}.", page_num);
    }
    
    public void file_save_as(object obj, EventArgs args)
    {
	int page_num = notebook.CurrentPage;
	Console.WriteLine("You have selected file_save_as page {0}.", page_num);
	//Widget widget = notebook.GetNthPage(page_num);
	//TODO: need a list of IDocuments
    }
    
    public bool file_save()
    {
	int page_num = notebook.CurrentPage;
	Console.WriteLine("You have selected file_save page {0}.", page_num);
        return true;
    }
    
    public bool file_save_as()
    {
	int page_num = notebook.CurrentPage;
	Console.WriteLine("You have selected file_save_as page {0}.", page_num);
        return true;
    }
    
    public void file_close(object obj, EventArgs args)
    {
	int page_num = notebook.CurrentPage;
	Console.WriteLine("You have selected file_close page {0}.", page_num);
	// TODO: check for Modified
	notebook.RemovePage(page_num);
	if (notebook.NPages == 0)
	    file_new(null, null);
	notebook.Show();
    }
}
