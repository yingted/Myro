import Gtk

from window import Window
from utils import _
from document import Document, DinahDocument

class EditorWindow(Window):
    def __init__(self, project, files=None):
        self.project = project
        # create the parts
        self.window = Gtk.Window(_("Pyjama Editor"))
        self.window.SetDefaultSize(600, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_close)
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [("_File", 
                 [("Open Script...", Gtk.Stock.Open, 
                   None, self.on_open_file),
                  None,
                  # FIXME: get these from engines:
                  ("New Python Script", None, 
                   None, self.on_new_file),
                  ("New Scheme Script", None, 
                   None, self.on_new_file),
                  ("New Ruby Script", None, 
                   None, self.on_new_file),
                  ("New Dinah Script", None, 
                   None, self.on_new_dinah),
                  None,
                  ("Save...", Gtk.Stock.Save, 
                   None, self.on_save_file),
                  ("Save as...", Gtk.Stock.SaveAs,
                   None, self.on_save_file_as),
                  None,
                  ("Close", Gtk.Stock.Close,
                   None, self.on_close),
                  ("Quit", Gtk.Stock.Quit,
                   None, self.on_quit),
                  ]),
                ("_Edit", []),
                ("She_ll", [("Run", Gtk.Stock.Apply,
                            "F5", self.on_run)]),
                ("Windows", [
                    ("Editor", None, "F6", self.project.setup_editor),
                    ("Shell", None, "F7", self.project.setup_shell),
                    ]),
                ("O_ptions", []),
                ("_Help", []),
                ]
        toolbar = [(Gtk.Stock.New, self.on_new_file),
                   (Gtk.Stock.Open, self.on_open_file),
                   (Gtk.Stock.Save, self.on_save_file), 
                   (Gtk.Stock.Quit, self.on_quit),]
        self.make_gui(menu, toolbar)
        self.notebook = Gtk.Notebook()
        self.notebook.Scrollable = True
        self.notebook.TabHborder = 5
        self.notebook.TabBorder = 1
        self.notebook.TabVborder = 1
        self.statusbar = Gtk.Statusbar()
        self.statusbar.Push(0, "Language: Python   Column: 0 Row: 0")
        self.statusbar.HasResizeGrip = True
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
                page = self.make_document(file)
                self.notebook.AppendPage(page.widget, page.tab)
        else:
            page = self.make_document(None)
            self.notebook.AppendPage(page.widget, page.tab)

    def on_open_file(self, obj, event):
        retval = False
        fc = Gtk.FileChooserDialog("Select the file to open",
                                   self.window,
                                   Gtk.FileChooserAction.Open,
                                   "Cancel", Gtk.ResponseType.Cancel,
                                   "Open", Gtk.ResponseType.Accept)
        if (fc.Run() == int(Gtk.ResponseType.Accept)):
            page = self.make_document(fc.Filename)
            page_num = self.notebook.AppendPage(page.widget, page.tab)
            self.notebook.CurrentPage = page_num
            retval = True
        fc.Destroy()
        return retval

    def on_close_tab(self, page):
        page_num = self.notebook.PageNum(page)
        self.notebook.RemovePage(page_num)

    def on_new_file(self, obj, event):
        page = self.make_document(None)
        page_num = self.notebook.AppendPage(page.widget, page.tab)
        self.notebook.CurrentPage = page_num

    def on_new_dinah(self, obj, event):
        page = self.make_document("New Dinah Script.dnh")
        page_num = self.notebook.AppendPage(page.widget, page.tab)
        self.notebook.CurrentPage = page_num

    def make_document(self, filename):
        # FIXME: handle default here too (option?)
        # FIXME: get documents from registered engines
        if filename and filename.endswith(".dnh"):
            page = DinahDocument(filename, self.project)
        else:
            page = Document(filename, self.project)
        return page

    def on_save_file(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            doc.save()

    def on_save_file_as(self, obj, event):
        doc = self.get_current_doc()
        if doc:
            doc.save_as()

    def get_current_doc(self):
        if self.notebook.CurrentPage >= 0:
            return self.notebook.GetNthPage(self.notebook.CurrentPage).document
        else:
            return None

    def on_run(self, obj, event):
        self.project.setup_shell()
        doc = self.get_current_doc()
        if doc:
            if doc.save():
                self.project.shell.message("Loading file...\n")
                self.project.shell.execute_file(doc.filename, "python")
                self.project.shell.message("Done loading!\n")

    def on_close(self, obj, event):
        self.project.on_close("editor")
        return True

    def on_quit(self, obj, event):
        Gtk.Application.Quit()
