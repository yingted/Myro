# Bring .NET References into IronPython scope:
import Gtk, Pango
import System
import IronPython
import IronPython.Hosting
import IronRuby
import Microsoft.Scripting

from window import Window
from utils import _

import traceback
import sys, os

class History(object):
    def __init__(self):
        self.history = []
        self.position = None

    def up(self):
        #print "up", self.position, self.history
        if self.position is not None and 0 <= self.position - 1 < len(self.history):
            self.position -= 1
            #print "ok"
            return self.history[self.position]
        return None

    def down(self):
        #print "down", self.position, self.history
        if self.position is not None and 0 <= self.position + 1 < len(self.history):
            self.position += 1
            #print "ok"
            return self.history[self.position]
        return None

    def add(self, text):
        self.history.append(text)
        self.position = len(self.history)
        #print "add", self.position, self.history

class CustomStream(System.IO.Stream):
    def __init__(self, textview, tag=None):
        self.textview = textview
        self.tag = tag

    def set_output(self, textview):
        self.textview = textview

    def write(self, text):
        end = self.textview.Buffer.EndIter
        self.textview.Buffer.InsertWithTagsByName(end, text, "red")
        self.goto_end()

    def goto_end(self):
        end = self.textview.Buffer.EndIter
        insert_mark = self.textview.Buffer.InsertMark 
        self.textview.Buffer.PlaceCursor(end)
        self.textview.ScrollToMark(insert_mark, 0.0, True, 0, 1.0)

    def Write(self, bytes, offset, count):
        # Turn the byte-array back into a string
        text = System.Text.Encoding.UTF8.GetString(bytes, offset, count)
        #print "write: %s" % repr(text)
        #if not text.endswith("\n"):
        #    text += "\n"
        if self.tag:
            end = self.textview.Buffer.EndIter
            self.textview.Buffer.InsertWithTagsByName(end, text, self.tag)
        else:
            self.textview.Buffer.InsertAtCursor(text)
        self.goto_end()

    @property
    def CanRead(self):
        return False

    @property
    def CanSeek(self):
        return False

    @property
    def CanWrite(self):
        return True

    def Flush(self):
        pass

    def Close(self):
        pass

    @property
    def Position(self):
        return 0

# FIXME: work-around for not being able to handle
# method before
# 1) override textview so that OnKeyPressEvent doesn't do anything
# (can't override protected KeyPressEvent)
# 2) add a KeyPressEvent to handle the keypress
# 3) call the original keypress handler if not handled

class MyTextView(Gtk.TextView):
    def OnKeyPressEvent(self, gdk_eventkey):
        pass

class ShellWindow(Window):
    def __init__(self, project):
        self.project = project
        self.language = "python"
        self.window = Gtk.Window(_("Pyjama Shell"))
        self.window.SetDefaultSize(600, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_close)
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [("_File", 
                 [("Open Script...", Gtk.Stock.Open,
                   None, self.on_open_file),
                  ("New Script", Gtk.Stock.New,
                   None, self.on_new_file),
                  None,
                  ("Close", Gtk.Stock.Close,
                   None, self.on_close),
                  ("Quit", Gtk.Stock.Quit,
                   None, self.on_quit),
                  ]),
                ("_Edit", []),
                ("She_ll", [("Run", Gtk.Stock.Apply,
                            "F5", self.on_run),
                            ("Change to Python", None, "<control>p", 
                             self.change_to_python), 
                            ("Change to Ruby", None, "<control>r", 
                             self.change_to_ruby), 
                            ]),
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
        self.history = History()
        self.statusbar = Gtk.Statusbar()
        self.statusbar.Show()
        self.statusbar.Push(1, "Language: Python")
        self.command_area = Gtk.HBox()
        alignment = Gtk.Alignment( 0.5, 0.0, 0, 0)
        self.prompt = Gtk.Label("python>")
        alignment.Add(self.prompt)
        self.command_area.PackStart(alignment, False, False, 0)
        self.scrolled_window = Gtk.ScrolledWindow()
        self.command_area.PackStart(self.scrolled_window, True, True, 0)
        self.scrolled_window.ShadowType = Gtk.ShadowType.Out
        self.scrolled_window.HeightRequest = 20
        self.textview = MyTextView()
        self.textview.KeyPressEvent += self.on_key_press
        self.textview.Show()
        self.textview.ModifyFont(Pango.FontDescription.FromString("Courier 10"))
        self.scrolled_window.AddWithViewport(self.textview)
        self.results = Gtk.ScrolledWindow()
        self.history_textview = Gtk.TextView()
        tag = Gtk.TextTag("red")
        tag.Weight = Pango.Weight.Bold
        tag.Foreground = "red" # Pango.Color.Red
        self.history_textview.Buffer.TagTable.Add(tag)
        tag = Gtk.TextTag("blue")
        tag.Weight = Pango.Weight.Bold
        tag.Foreground = "blue" # Pango.Color.Red
        self.history_textview.Buffer.TagTable.Add(tag)
        tag = Gtk.TextTag("black")
        tag.Weight = Pango.Weight.Bold
        tag.Foreground = "black" # Pango.Color.Red
        self.history_textview.Buffer.TagTable.Add(tag)
        self.history_textview.ModifyFont(Pango.FontDescription.FromString("Courier 10"))

        self.history_textview.WrapMode = Gtk.WrapMode.Word
        self.history_textview.Editable = False
        self.results.Add(self.history_textview)
        self.results.Show()
        self.vpane = Gtk.VPaned()
        self.vpane.Pack1(self.command_area, True, True)
        self.vpane.Pack2(self.results, True, True)
        # initialize
        self.window.Add(self.vbox)
        self.vbox.PackStart(self.menubar, False, False, 0)
        self.vbox.PackStart(self.toolbar, False, False, 0)
        self.vbox.PackStart(self.vpane, True, True, 0)
        self.vbox.PackEnd(self.statusbar, False, False, 0)
        self.window.ShowAll()

        # DLR hosting:
        self.scriptRuntimeSetup = Microsoft.Scripting.Hosting.ScriptRuntimeSetup()
        self.scriptRuntimeSetup.LanguageSetups.Add(
            Microsoft.Scripting.Hosting.LanguageSetup(
                "IronPython.Runtime.PythonContext, IronPython",
                "IronPython",
                ["IronPython", "Python", "python", "py"],
                [".py"]));
        self.scriptRuntimeSetup.LanguageSetups.Add(
             Microsoft.Scripting.Hosting.LanguageSetup(
                "IronRuby.Runtime.RubyContext, IronRuby",
                "IronRuby",
                ["IronRuby", "Ruby", "ruby", "rb"],
                [".rb"]))

        self.environment = Microsoft.Scripting.Hosting.ScriptRuntime(
            self.scriptRuntimeSetup)
        self.scope = self.environment.CreateScope()
        self.engine = {
            "python": self.environment.GetEngine("py"),
            "ruby": self.environment.GetEngine("rb"),
            }
        engine = self.engine["python"]
        # FIXME: add debug to engine:
        #Dictionary<string, object> options = new Dictionary<string, object>();
        #options["Debug"] = true;
        #Python.CreateEngine(options);
        # Load mscorlib.dll:
        engine.Runtime.LoadAssembly(
            System.Type.GetType(System.String).Assembly);
        # Load Languages so that Host System can find DLLs:
        engine.Runtime.LoadAssembly(
            System.Type.GetType(IronPython.Hosting.Python).Assembly)
        engine.Runtime.LoadAssembly(
            System.Type.GetType(IronRuby.Hosting.RubyCommandLine).Assembly)
        engine.Runtime.LoadAssembly(
            System.Type.GetType(
             IronRuby.StandardLibrary.BigDecimal.Fraction).Assembly)
        # Load System.dll
        engine.Runtime.LoadAssembly(System.Type.GetType(
                System.Diagnostics.Debug).Assembly)
        engine.Runtime.IO.SetOutput(CustomStream(self.history_textview), 
                                    System.Text.Encoding.UTF8)
        engine.Runtime.IO.SetErrorOutput(CustomStream(self.history_textview, 
                                                      "red"), 
                                         System.Text.Encoding.UTF8)
        paths = engine.GetSearchPaths()
        # Let users find Python standard library:
        #for path in ["IronPython/ipy2"]:
        #    lib_directory = os.path.abspath(path)
        #    paths.Add(lib_directory)
        ## Let users find Pyjama modules:
        paths.Add(os.path.abspath("modules"))
        self.engine["python"].SetSearchPaths(paths)

        # Start up, in Python: ------------------
        script = """
import clr
clr.AddReference("myro.dll")
del clr
"""
	scope = self.engine["python"].Runtime.CreateScope()
	source = self.engine["python"].CreateScriptSourceFromString(script)
        source.Compile().Execute(scope)
        # ---------------------------------------

        sys.stderr = CustomStream(self.history_textview, "red")
        self.update_gui()

    def update_gui(self):
        self.window.Title = _("%s - Pyjama Shell") % self.language.title()
        self.prompt.Text = "%-6s>" % self.language
        self.statusbar.Pop(0)
        self.statusbar.Push(0, _("Language: %s") % self.language.title())

    def on_key_press(self, obj, event):
        if event.RetVal: return # already handled
        if str(event.Event.Key) == "Return":
            mark = self.textview.Buffer.InsertMark
            itermark = self.textview.Buffer.GetIterAtMark(mark)
            line = itermark.Line - 1
            iterline = self.textview.Buffer.GetIterAtLine(line)
            end = self.textview.Buffer.EndIter
            text = self.textview.Buffer.GetText(iterline, end, False)
            text = text.rstrip()
            if text == "":
                self.history.add(text)
                self.execute(text, self.language)
            elif text[-1] == ":":
                self.textview.Buffer.InsertAtCursor("\n    ")
            elif text[0] == " ":
                self.textview.Buffer.InsertAtCursor("\n    ")
            else:
                self.history.add(text)
                self.execute(text, self.language)
            event.RetVal = True
        elif str(event.Event.Key) == "Up":
            mark = self.textview.Buffer.InsertMark
            itermark = self.textview.Buffer.GetIterAtMark(mark)
            line = itermark.Line
            #print "line:", line
            if line == 0:
                text = self.history.up()
                if text:
                    self.textview.Buffer.Text = text
                    event.RetVal = True
        elif str(event.Event.Key) == "Down":
            mark = self.textview.Buffer.InsertMark
            itermark = self.textview.Buffer.GetIterAtMark(mark)
            line = itermark.Line
            if line == self.textview.Buffer.LineCount - 1:
                text = self.history.down()
                if text:
                    self.textview.Buffer.Text = text
                    event.RetVal = True
        if not event.RetVal:
            event.RetVal = Gtk.TextView.OnKeyPressEvent(self.textview, 
                                                        event.Event)

    def change_to_ruby(self, obj, event):
        self.language = "ruby"
        self.update_gui()

    def change_to_python(self, obj, event):
        self.language = "python"
        self.update_gui()

    def on_save_file_as(self, obj, event):
        pass

    def on_quit(self, obj, event):
        Gtk.Application.Quit()

    def on_close(self, obj, event):
        self.project.on_close("shell")

    def on_run(self, obj, event):
        pass

    # these aren't needed?
    def on_new_file(self, obj, event):
        pass
    def on_open_file(self, obj, event):
        pass
    def on_save_file(self, obj, event):
        pass

    def execute(self, text, language, message=None):
        if message:
            self.history_textview.Buffer.InsertAtCursor(message + "\n    ")
        else:
            self.textview.Buffer.Clear()
            prompt = "%-6s> " % language
            for line in text.split("\n"):
                end = self.history_textview.Buffer.EndIter
                self.history_textview.Buffer.InsertWithTagsByName(end, 
                                 "%s" % prompt,
                                 "black")
                end = self.history_textview.Buffer.EndIter
                self.history_textview.Buffer.InsertWithTagsByName(end, 
                                 "%s\n" % line,
                                 "blue")
                prompt = "......>"

        # pragma/meta commands, start with #;
        if text == "":
            return False
        elif text and text[0:2] == "#;":
            text = text[2:].strip()
            command = text.lower()
            if command in ["ruby", "python"]:
                self.language = command
                self.update_gui()
                return True
            else:
                exec(text)
                return True
        self.language = language
        self.update_gui()
        sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode
        source = self.engine[language].CreateScriptSourceFromString(text, sctype)
        try:
            source.Compile()
        except:
            sctype = Microsoft.Scripting.SourceCodeKind.Statements
            source = self.engine[language].CreateScriptSourceFromString(text, sctype)
            try:
                source.Compile()
            except:
                traceback.print_exc()
                return False
        try:
            source.Execute(self.scope)
        except:
            traceback.print_exc()
        return True
