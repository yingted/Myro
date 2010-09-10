import time
import random
from Graphics import *

init()
win = GraphWin("Turtles!")

turtles = []
for x in range(100):
    turtle = Arrow(Point(random.random() * 200,random.random() * 200), random.random() * 360)
    turtle.draw(win)
    turtle.color = color_map(random.choice(["red", "green", "blue", "yellow"]))
    turtles.append(turtle)

win.ShowAll()
for x in range(10):
    for t in turtles:
        t.move(random.random() * 6 - 3, 
               random.random() * 6 - 3)
