import sys
sys.path.append('C:\\Users\\Vincent\\Documents\\Pyjama\\csharp\\graphics\\bin\\Release')
import clr
clr.AddReference("graphics.dll")
from graphics import *
#clr.AddReference("System.Windows.Forms")
#from System.Windows.Forms import *
#import time

r = Run()
r.Main()

#i = Image("C:\Users\Vincent\Pictures\Family\kash_tongue.jpg")
#for m in i.getPixel(50, 50):
#    print m