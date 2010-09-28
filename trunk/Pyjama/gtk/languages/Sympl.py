import clr
import sys
from System.IO import StringReader
from sympl import sympl
from document import Document
from engine import Engine
from utils import Language

class SymplEngine(Engine):
    def __init__(self, manager):
        super(SymplEngine, self).__init__(manager, "sympl")
        self.engine = sympl.Sympl()
        self.scope =  self.engine.CreateScope()

    def execute(self, text):
        try:
            sympl.parser.ParseExpr(StringReader(text))
            result = self.engine.ExecuteExpr(text, self.scope)
            self.stdout.write("%s\n" % result)
        except Exception, e:
            System.Console.WriteLine(e)

    def execute_file(self, filename):
        self.engine.ExecuteFileInScope(filename, self.scope)

    def ready_for_execute(self, text):
        """
        Return True if expression parses ok.
        """
        lines = text.split("\n")
        if lines[-1] == "":
            return True # force it
        # else, only if valid parse
        try:
            sympl.parser.ParseExpr(StringReader(text))
            return True
        except Exception, e:
            return False

class Sympl(Language):
    def get_engine_class(self):
        return SymplEngine

    def get_document_class(self):
        return Document

def register_language():
    return Sympl("sympl", "sympl")


