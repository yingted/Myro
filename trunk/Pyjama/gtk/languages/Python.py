import clr
clr.AddReference("IronPython")
clr.AddReference("IronPython.Modules")
clr.AddReference("Microsoft.Scripting")
import IronPython
import System
import Microsoft.Scripting
from document import Document
from engine import DLREngine
from utils import Language
import os

class PythonEngine(DLREngine):
    def __init__(self, manager): 
        super(PythonEngine, self).__init__(manager, "python")
        self.dlr_name = "py"
        self.manager.scriptRuntimeSetup.LanguageSetups.Add(
            Microsoft.Scripting.Hosting.LanguageSetup(
                "IronPython.Runtime.PythonContext, IronPython",
                "IronPython",
                ["IronPython", "Python", "python", "py"],
                [".py"]))

    def setup(self, stderr, stdout, stdin):
        super(PythonEngine, self).setup(stderr, stdout, stdin)
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronPython.Hosting.Python).Assembly)

    def start(self):
        paths = self.engine.GetSearchPaths()
        ## Let users find Pyjama modules:
        for folder in ["modules", "src"]:
            paths.Add(os.path.abspath(folder))
        self.engine.SetSearchPaths(paths)
        # Start up, in Python: ------------------
        # FIXME: do here, not in sub-Python
        script = """
import clr
try:
    clr.AddReference("Myro.dll")
except:
    pass
try:
    clr.AddReference("Graphics.dll")
except:
    pass
del clr
"""
        temp_scope = self.manager.runtime.CreateScope()
	source = self.engine.CreateScriptSourceFromString(script)
        source.Compile().Execute(temp_scope)
        # ---------------------------------------

class Python(Language):
    def get_engine_class(self):
        return PythonEngine

    def get_document_class(self):
        return Document

def register_language():
    return Python("python", "py")

