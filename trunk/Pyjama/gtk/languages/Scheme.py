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
    def start(self, stderr, stdout, stdin):
        super(SchemeEngine, self).start(stderr, stdout, stdin)
        self.engine.set_dlr(self.manager.scope, self.manager.runtime)
    def parse(self, text):
        return False # requires an empty line

class Scheme(Language):
    def get_engine_class(self):
        return SchemeEngine

    def get_document_class(self):
        return Document

def register_language():
    return Scheme("scheme")
