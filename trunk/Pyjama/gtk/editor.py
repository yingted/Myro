import Gtk

from window import Window
from utils import _
from document import Document

class EditorWindow(Window):
    def __init__(self, project, files=None):
        self.project = project
        # create the parts
        self.window = Gtk.Window(_("Pyjama Editor"))
        self.window.SetDefaultSize(600, 550)
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [("_File", 
                 [("Open...", Gtk.Stock.Open, 
                   None, self.on_open_file),
                  ("New...", Gtk.Stock.New, 
                   None, self.on_new_file),
                  ("Save...", Gtk.Stock.Save, 
                   None, self.on_save_file),
                  ("Save as...", Gtk.Stock.SaveAs,
                   None, self.on_save_file_as),
                  None,
                  ("Quit", Gtk.Stock.Quit,
                   None, self.on_quit),
                  ]),
                ("_Edit", []),
                ("She_ll", [("Run", Gtk.Stock.Apply,
                            "F5", self.on_run)]),
                ("O_ptions", []),
                ("_Help", []),
                ]
        self.make_gui(menu)
        
        self.notebook = Gtk.Notebook()
        self.notebook.Scrollable = True
        self.notebook.TabHborder = 5
        self.notebook.TabBorder = 1
        self.notebook.TabVborder = 1
        self.statusbar = Gtk.Statusbar()
        self.statusbar.Push(0, "Language: Python   Column: 0 Row: 0")
        self.statusbar.HasResizeGrip = False
        self.statusbar.Show()
        # initialize
        self.window.Add(self.vbox)
        self.vbox.PackStart(self.menubar, False, False, 0)
        self.vbox.PackStart(self.toolbar, False, False, 0)
        self.vbox.PackStart(self.notebook, True, True, 0)
        self.vbox.PackStart(self.statusbar, False, False, 0)
        self.window.ShowAll()

        # Open files on command line, or just a New Script:
        if files:
            for file in files:
                page = Document(file, self.project)
                self.notebook.AppendPage(page.widget, page.tab)
        else:
            page = Document(None, self.project)
            self.notebook.AppendPage(page.widget, page.tab)

        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_quit)

    def on_open_file(self, obj, event):
        page = Document("Script-1.ppy", self.project)
        page_num = self.notebook.AppendPage(page.widget, page.tab)
        self.notebook.CurrentPage = page_num

    def on_new_file(self, obj, event):
        page = Document(None, self.project)
        page_num = self.notebook.AppendPage(page.widget, page.tab)
        self.notebook.CurrentPage = page_num

    def on_close_tab(self, page):
        page_num = self.notebook.PageNum(page)
        self.notebook.RemovePage(page_num)

    def on_save_file(self, obj, event):
        print "save file", obj

    def on_save_file_as(self, obj, event):
        print "save file as", obj

    def get_current_doc(self):
        if self.notebook.CurrentPage >= 0:
            return self.notebook.GetNthPage(self.notebook.CurrentPage).document
        else:
            return None

    def on_run(self, obj, event):
        pass

    def on_quit(self, obj, event):
        Gtk.Application.Quit()