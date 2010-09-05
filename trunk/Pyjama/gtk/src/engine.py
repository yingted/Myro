import IronPython
import IronPython.Hosting
import IronRuby
import Microsoft.Scripting
import System

from utils import CustomStream

import traceback
import os

class EngineManager(object):
    def __init__(self, project):
        self.project = project
        self.scriptRuntimeSetup = Microsoft.Scripting.Hosting.ScriptRuntimeSetup()
        self.engine = {}	

    def __getitem__(self, name):
        return self.engine[name]

    def register(self, Engine):
        engine = Engine(self)
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
    def __init__(self, language):
        self.language = language

    def execute(self, text):
        raise NotImplemented

    def execute_file(self, filename):
        raise NotImplemented

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

class PythonEngine(DLREngine):
    def __init__(self, manager): 
        super(PythonEngine, self).__init__("python")
        self.manager = manager
        self.dlr_name = "py"
        self.manager.scriptRuntimeSetup.LanguageSetups.Add(
            Microsoft.Scripting.Hosting.LanguageSetup(
                "IronPython.Runtime.PythonContext, IronPython",
                "IronPython",
                ["IronPython", "Python", "python", "py"],
                [".py"]))

    def start(self, stderr, stdout, stdin):
        super(PythonEngine, self).start(stderr, stdout, stdin)
        paths = self.engine.GetSearchPaths()
        ## Let users find Pyjama modules:
        paths.Add(os.path.abspath("modules"))
        self.engine.SetSearchPaths(paths)
        # Start up, in Python: ------------------
        script = """
import clr
clr.AddReference("Myro.dll")
del clr
"""
        temp_scope = self.manager.runtime.CreateScope()
	source = self.engine.CreateScriptSourceFromString(script)
        source.Compile().Execute(temp_scope)
        # ---------------------------------------

class RubyEngine(DLREngine):
    def __init__(self, manager):
        super(RubyEngine, self).__init__("ruby")
        self.manager = manager
        self.dlr_name = "rb"
        self.manager.scriptRuntimeSetup.LanguageSetups.Add(
             Microsoft.Scripting.Hosting.LanguageSetup(
                "IronRuby.Runtime.RubyContext, IronRuby",
                "IronRuby",
                ["IronRuby", "Ruby", "ruby", "rb"],
                [".rb"]))

    def start(self, stderr, stdout, stdin):
        super(RubyEngine, self).start(stderr, stdout, stdin)
        # FIXME: IronRuby bug: returns Array, doesn't take list
        paths = list(self.engine.GetSearchPaths())
        paths.Add(os.path.abspath("modules"))
        self.engine.SetSearchPaths(System.Array[str](paths))

if __name__ == "__main__":
    EngineMan = EngineManager(None) # singleton
    EngineMan.register(RubyEngine)
    EngineMan.register(PythonEngine)
    EngineMan.start(None, None, None)
