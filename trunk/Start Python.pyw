#! /usr/bin/env python

from idlelib import PyShell
import sys, os
if os.name in ['nt', 'dos', 'os2'] :
    pid = os.getpid()
    os.system("""taskkill /F /FI "PID ne %d" /IM pythonw.exe /T """ % pid)
    # kill force processes named pythonw.exe
#sys.argv = [sys.argv[0]] + ['-n'] 
try:
    commands = open("mystart.py", "r").readlines()
except:
    #commands = ["from myro import *\n"] # add commands right here
    # this causes issues with scope
    commands = [] # add commands right here
command = "" 
for c in commands:
    print ">>>", c,
    command += c
if command != "":
    sys.argv.append("-c")
    sys.argv.append(command)
del command
del commands
del sys

old_cancel_callback = PyShell.PyShell.cancel_callback
def cancel_callback(self, event=None):
    retval = old_cancel_callback(self, event)
    #PyShell.flist.pyshell.interp.runcode(code)
    code = """if "stop" in dir(): stop()"""
    try:
        PyShell.flist.pyshell.interp.rpcclt.asyncqueue("exec", "runcode",
                                                       (code,), {})
    except:
        pass
    return retval
PyShell.PyShell.cancel_callback = cancel_callback
PyShell.main()
