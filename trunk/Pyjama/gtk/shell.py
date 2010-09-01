# Bring .NET References into IronPython scope:
import Gtk, Pango
import System
import IronPython
import IronPython.Hosting
import IronRuby
import Microsoft
import Microsoft.Scripting
import Microsoft.Scripting.Hosting

from window import Window
from utils import _

import traceback
import sys, os

def prefix(text):
    retval = ""
    prompt = ">>> "
    for line in text.split("\n"):
        retval += "%s%s\n" % (prompt, line)
        prompt = "... "
    return retval 

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
        self.window = Gtk.Window(_("Pyjama Shell"))
        self.window.SetDefaultSize(600, 550)
        self.window.DeleteEvent += Gtk.DeleteEventHandler(self.on_quit)
        self.vbox = Gtk.VBox()
        # ---------------------
        # make menu:
        menu = [("_File", 
                 [("Save as...", Gtk.Stock.SaveAs,
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

        self.history = History()
        self.statusbar = Gtk.Statusbar()
        self.statusbar.Show()
        self.statusbar.Push(0, "Language: Python")
        self.command_area = Gtk.HBox()
        alignment = Gtk.Alignment( 0.5, 0.0, 0, 0)
        alignment.Add(Gtk.Label(">>>"))
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
        #using IronPython.Hosting;   //PythonEngine
        #using IronRuby.Hosting;
        #using Microsoft.Scripting;  //ScriptDomainManager
        #using Microsoft.Scripting.Hosting;
        self.scriptRuntimeSetup = Microsoft.Scripting.Hosting.ScriptRuntimeSetup()
        self.scriptRuntimeSetup.LanguageSetups.Add(
            Microsoft.Scripting.Hosting.LanguageSetup("IronPython.Runtime.PythonContext, IronPython",
                                                      "IronPython",
                                                      ["IronPython", "Python", "py"],
                                                      [".py"]));
        self.scriptRuntimeSetup.LanguageSetups.Add(
             Microsoft.Scripting.Hosting.LanguageSetup("IronRuby.Runtime.RubyContext, IronRuby",
                                                       "IronRuby",
                                                       ["IronRuby", "Ruby", "rb"],
                                                       [".rb"]))

        self.environment = Microsoft.Scripting.Hosting.ScriptRuntime(
            self.scriptRuntimeSetup)
        self.scope = self.environment.CreateScope()
        # Determine what engine is running
        #if (engine.Setup.DisplayName == "IronPython"):
        engine = self.environment.GetEngine("py")
        #elif (engine.Setup.DisplayName == "IronRuby"):
        #    engine = self.environment.GetEngine("rb")
        #else:
        #    engine = self.environment.GetEngine("py")

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
        self.engine = engine
        self.engine.Runtime.IO.SetOutput(CustomStream(self.history_textview), 
                                         System.Text.Encoding.UTF8)
        self.engine.Runtime.IO.SetErrorOutput(CustomStream(self.history_textview, 
                                                           "red"), 
                                              System.Text.Encoding.UTF8)
        paths = self.engine.GetSearchPaths()
        # Let users find Python standard library:
        paths.Add("/usr/lib/python2.6")
        # Let users find Pyjama modules:
        paths.Add(os.path.abspath("modules"))
        self.engine.SetSearchPaths(paths)

        # Start up, in Python: ------------------
        script = """
import clr
clr.AddReference("myro.dll")
del clr
"""
	scope = self.engine.Runtime.CreateScope()
	source = self.engine.CreateScriptSourceFromString(script)
        source.Compile().Execute(scope)
        # ---------------------------------------

        sys.stderr = CustomStream(self.history_textview, "red")

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
                self.execute(text)
            elif text[-1] == ":":
                self.textview.Buffer.InsertAtCursor("\n    ")
            elif text[0] == " ":
                self.textview.Buffer.InsertAtCursor("\n    ")
            else:
                self.history.add(text)
                self.execute(text)
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

    def on_save_file_as(self, obj, event):
        pass

    def on_quit(self, obj, event):
        Gtk.Application.Quit()

    def on_run(self, obj, event):
        pass

    # these aren't needed?
    def on_new_file(self, obj, event):
        pass
    def on_open_file(self, obj, event):
        pass
    def on_save_file(self, obj, event):
        pass

    def execute(self, text, message=None):
        self.textview.Buffer.Clear()
        if message:
            self.history_textview.Buffer.InsertAtCursor(message + "\n    ")
        else:
            end = self.history_textview.Buffer.EndIter
            self.history_textview.Buffer.InsertWithTagsByName(end, 
                                                              "%s\n" % text, 
                                                              "blue")

        sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode
        source = self.engine.CreateScriptSourceFromString(text, sctype)
        try:
            source.Compile()
        except:
            sctype = Microsoft.Scripting.SourceCodeKind.Statements
            source = self.engine.CreateScriptSourceFromString(text, sctype)
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
