import time
import clr
clr.AddReference("Graphics.dll")
from Graphics import *
init()
#print "crash!"
win = GraphWin("Turtles!")
turtle = Arrow(Point(50,50))
turtle.draw(win)
win.ShowAll()
for x in range(10):
    turtle.move(2, 0)
    time.sleep(.2)
