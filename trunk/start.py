#! /usr/bin/env python

from idlelib import PyShell
import sys
sys.argv = [sys.argv[0]] + ['-n'] #, '-c', 'from myro import *']
PyShell.main()
