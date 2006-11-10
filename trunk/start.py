#! /usr/bin/env python

from idlelib import PyShell
import sys
sys.argv = [sys.argv[0]] + ['-n'] #, '-c', 'from myro import *']
try:
    commands = open("mystart.py", "r").readlines()
except:
    commands = []
command = ""
for c in commands:
    print ">>>", c,
    command += c
if command != "":
    sys.argv.append("-c")
    sys.argv.append(command)
PyShell.main()
