from Graphics import *
import random

win = Window("Turtles!")

turtles = []
for x in range(1000):
    width, height = win.DefaultWidth, win.DefaultHeight
    turtle = Arrow(Point(random.random() * win.DefaultWidth, 
                         random.random() * win.DefaultHeight), 
                   random.random() * 360)
    turtle.draw(win)
    turtle.color = random.choice(["red", "green", "blue", "yellow"])
    turtles.append(turtle)

win.ShowAll()

for x in range(10):
    for t in turtles:
        t.move(random.random() * 6 - 3, 
               random.random() * 6 - 3)

for x in range(1000):
    turtles[x].color = ["red", "green", "blue", "black"][x / 333]
