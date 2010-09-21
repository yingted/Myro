import clr
import sys
clr.AddReference("PJScheme.dll")
import PJScheme
from document import Document
from engine import Engine
from utils import Language

class SchemeEngine(Engine):
    def __init__(self, manager):
        super(SchemeEngine, self).__init__(manager, "scheme")
        self.engine = PJScheme
    def execute(self, text):
        result = self.engine.execute(text)
        self.stdout.write("%s\n" % result)
    def execute_file(self, filename):
        self.stdout.write("Run filename '%s'!\n" % filename)
    def setup(self):
        super(SchemeEngine, self).setup()
        self.engine.set_dlr(self.manager.scope, self.manager.runtime)
    def ready_for_execute(self, text):
        """
        Return True if expression parses ok.
        """
        lines = text.split("\n")
        if lines[-1] == "":
            return True # force it
        # else, only if valid parse
        retval = str(self.engine.try_parse_string(text))
        # FIXME: when exceptions have a better format in Scheme:
        return not retval.startswith("(exception ")

class Scheme(Language):
    def get_engine_class(self):
        return SchemeEngine

    def get_document_class(self):
        return Document

def register_language():
    return Scheme("scheme", "ss")
