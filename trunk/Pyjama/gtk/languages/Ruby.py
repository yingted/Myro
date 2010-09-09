import Microsoft.Scripting
import System
from document import Document
from engine import DLREngine
from utils import Language

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

    def start(self, stderr, stdout, stdin):
        super(RubyEngine, self).start(stderr, stdout, stdin)
        # FIXME: IronRuby bug: returns Array, doesn't take list
        paths = list(self.engine.GetSearchPaths())
        paths.Add(os.path.abspath("modules"))
        self.engine.SetSearchPaths(System.Array[str](paths))

class Ruby(Language):
    def get_engine_class(self):
        return RubyEngine

    def get_document_class(self):
        return Document

def register_language():
    return Ruby("ruby")
