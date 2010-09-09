import time
from Graphics import *

init()
win = GraphWin("Turtles!")
turtle = Arrow(Point(50,50))
turtle.draw(win)
win.ShowAll()
for x in range(10):
    turtle.move(2, 0)
    time.sleep(.2)
