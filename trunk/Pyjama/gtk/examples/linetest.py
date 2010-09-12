from Graphics import *
import random

win = Window("Group Test")

turtles = []
for x in range(4):
    turtle = Arrow(Point(random.random() * win.DefaultWidth, 
                         random.random() * win.DefaultHeight))
    turtle.color = random.choice(["red", "blue", "green"])
    turtle.draw(win)
    turtles.append(turtle)

win.mode = "animate"

#turtle.pen_down()
#turtle
poly = Polygon("red", Point(10, 10), Point(50, 10), Point(50, 60), Point(10, 60))
poly.draw(win)

group = Group(*turtles)
group.rotate(10)

for x in range(10):
    poly.rotate(36)
    win.step()

win.animate_step_time = 20
for x in range(360):
    group.rotate(1)
    win.step()
