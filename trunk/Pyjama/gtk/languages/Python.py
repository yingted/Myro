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

    def setup(self):
        super(PythonEngine, self).setup()
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronPython.Hosting.Python).Assembly)

    def start(self):
        paths = self.engine.GetSearchPaths()
        ## Let users find Pyjama modules:
        for folder in ["modules", "src"]:
            paths.Add(os.path.abspath(folder))
        self.engine.SetSearchPaths(paths)

class Python(Language):
    def get_engine_class(self):
        return PythonEngine

    def get_document_class(self):
        return Document

def register_language():
    return Python("python", "py")

