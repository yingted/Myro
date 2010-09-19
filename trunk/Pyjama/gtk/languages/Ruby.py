import clr
clr.AddReference("IronRuby")
clr.AddReference("IronRuby.Libraries")
clr.AddReference("Microsoft.Scripting")
import IronRuby
import Microsoft.Scripting
import System
from document import Document
from engine import DLREngine
from utils import Language
import os

class RubyEngine(DLREngine):
    def __init__(self, manager):
        super(RubyEngine, self).__init__(manager, "ruby")
        self.dlr_name = "rb"
        self.manager.scriptRuntimeSetup.LanguageSetups.Add(
             Microsoft.Scripting.Hosting.LanguageSetup(
                "IronRuby.Runtime.RubyContext, IronRuby",
                "IronRuby",
                ["IronRuby", "Ruby", "ruby", "rb"],
                [".rb"]))

    def setup(self, stderr, stdout, stdin):
        super(RubyEngine, self).setup(stderr, stdout, stdin)
        # FIXME: IronRuby bug: returns Array, doesn't take list
        paths = list(self.engine.GetSearchPaths())
        paths.Add(os.path.abspath("modules"))
        self.engine.SetSearchPaths(System.Array[str](paths))

    def start(self):
        # Load Languages so that Host System can find DLLs:
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(IronRuby.Hosting.RubyCommandLine).Assembly)
        self.engine.Runtime.LoadAssembly(
            System.Type.GetType(
             IronRuby.StandardLibrary.BigDecimal.Fraction).Assembly)


class Ruby(Language):
    def get_engine_class(self):
        return RubyEngine

    def get_document_class(self):
        return Document

def register_language():
    return Ruby("ruby", "rb")
