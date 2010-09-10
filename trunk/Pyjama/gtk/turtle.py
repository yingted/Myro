import time
import random
from Graphics import *

init()
win = GraphWin("Turtles!")

turtle = Arrow(Point(random.random() * 200,random.random() * 200), random.random() * 360)
turtle.draw(win)
turtle.color = random.choice(["red", "green", "blue", "yellow"])

win.ShowAll()

def move():
    turtle.move(random.random() * 6 - 3, 
                random.random() * 6 - 3)
    win.update()
