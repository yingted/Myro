import traceback
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

    def start(self, stderr, stdout, stdin): # textviews
        self.stderr, self.stdout, self.stdin = stderr, stdout, stdin
    	self.runtime = Microsoft.Scripting.Hosting.ScriptRuntime(
            self.scriptRuntimeSetup)
    	self.scope = self.runtime.CreateScope()
        self.scope.SetVariable("pyjama", self.project)
        for engine in self.engine:
            self.engine[engine].start(self.stderr, self.stdout, self.stdin)

    def reset(self):
        self.start(self.stderr, self.stdout, self.stdin)

class Engine(object):
    def __init__(self, manager, language):
        self.manager = manager
        self.language = language

    def execute(self, text):
        raise NotImplemented

    def execute_file(self, filename):
        raise NotImplemented

    def start(self, stderr, stdout, stdin): # textviews
        self.sterr = CustomStream(stderr)
        self.stdout = CustomStream(stdout)

class DLREngine(Engine):
    def start(self, stderr, stdout, stdin): # textviews
        # True? A hint from the interwebs:
        #options["Debug"] = true;
        #Python.CreateEngine(options);
	self.engine = self.manager.runtime.GetEngine(self.dlr_name)
        # Load mscorlib.dll:
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(System.String).Assembly)
        # Load Languages so that Host System can find DLLs:
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronPython.Hosting.Python).Assembly)
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronRuby.Hosting.RubyCommandLine).Assembly)
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(
             IronRuby.StandardLibrary.BigDecimal.Fraction).Assembly)
        # Load System.dll
        self.engine.Runtime.LoadAssembly(System.Type.GetType(
                System.Diagnostics.Debug).Assembly)
        if stdout:
            self.engine.Runtime.IO.SetOutput(CustomStream(stdout), 
                                        System.Text.Encoding.UTF8)
        if stderr:
            self.engine.Runtime.IO.SetErrorOutput(CustomStream(stderr, 
                                                      "red"), 
                                         System.Text.Encoding.UTF8)

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

