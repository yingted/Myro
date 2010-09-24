import clr
import traceback
import glob
import os

import Microsoft.Scripting
import System

from utils import CustomStream

class EngineManager(object):
    def __init__(self, project):
        self.project = project
        self.scriptRuntimeSetup = Microsoft.Scripting.Hosting.ScriptRuntimeSetup()
        self.engine = {}	

    def __getitem__(self, name):
        return self.engine[name]

    def get_languages(self):
        return self.engine.keys()

    def register(self, EngineClass):
        engine = EngineClass(self)
        self.engine[engine.language] = engine

    def setup(self): 
    	self.runtime = Microsoft.Scripting.Hosting.ScriptRuntime(
            self.scriptRuntimeSetup)
    	self.scope = self.runtime.CreateScope()
        self.scope.SetVariable("pyjama", self.project)
        for engine in self.engine:
            self.engine[engine].setup()

    def set_redirects(self, stdout, stderr, stdin): # textviews
        self.stderr, self.stdout, self.stdin = stderr, stdout, stdin
        for engine in self.engine:
            self.engine[engine].set_redirects(self.stdout, self.stderr, self.stdin)

    def start(self):
        for engine in self.engine:
            self.engine[engine].start()

    def reset(self):
        self.setup()
        self.start()
        self.set_redirects(self.stdout, self.stderr, self.stdin)

class Engine(object):
    def __init__(self, manager, language):
        self.manager = manager
        self.language = language
        self.text_based = True

    def execute(self, text):
        raise NotImplemented

    def execute_file(self, filename):
        raise NotImplemented

    def setup(self):
        pass

    def set_redirects(self, stdout, stderr, stdin): # textviews
        self.sterr = stderr
        self.stdout = stdout

    def start(self):
        pass

class DLREngine(Engine):
    def setup(self):
        # True? A hint from the interwebs:
        #options["Debug"] = true;
        #Python.CreateEngine(options);
	self.engine = self.manager.runtime.GetEngine(self.dlr_name)
        # Load mscorlib.dll:
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(System.String).Assembly)
        # ---------------------------------------
        for file in glob.glob("modules/*.dll"):
            #path, dll_name = os.path.split(file)
            clr.AddReference(file)
        for assembly in clr.References:
            self.engine.Runtime.LoadAssembly(assembly)
        # ---------------------------------------
        self.engine.Runtime.LoadAssembly(System.Type.GetType(
                System.Diagnostics.Debug).Assembly)

    def set_redirects(self, stdout, stderr, stdin): # textviews
        super(DLREngine, self).set_redirects(stdout, stderr, stdin)
        if stdout:
            #print "Setting stdout", stdout
            self.engine.Runtime.IO.SetOutput(stdout, 
                                             System.Text.Encoding.UTF8)
        if stderr:
            self.engine.Runtime.IO.SetErrorOutput(stderr,
                                                  System.Text.Encoding.UTF8)

    def ready_for_execute(self, text):
        """
        Is the text ready for executing?
        """
        # If more than one line in DLR, wait for a blank line
        lines = text.split("\n")
        line_count = len(lines)
        if line_count == 1:
            sctype = Microsoft.Scripting.SourceCodeKind.InteractiveCode
            source = self.engine.CreateScriptSourceFromString(text, sctype)
            return (source.GetCodeProperties() == 
                    Microsoft.Scripting.ScriptCodeParseResult.Complete)
        return lines[-1] == ""

    def execute(self, text):
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
            source.Execute(self.manager.scope)
        except:
            traceback.print_exc()
        return True

    def execute_file(self, filename):
        source = self.engine.CreateScriptSourceFromFile(filename)
        try:
            source.Compile()
        except:
            traceback.print_exc()
            return False
        try:
            source.Execute(self.manager.scope)
        except:
            traceback.print_exc()

