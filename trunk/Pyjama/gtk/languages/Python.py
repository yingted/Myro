import clr
clr.AddReference("Microsoft.Scripting")
import Microsoft.Scripting
from document import Document
from engine import DLREngine
from utils import Language

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

class Python(Language):
    def get_engine_class(self):
        return PythonEngine

    def get_document_class(self):
        return Document

def register_language():
    return Python("python")

