from Graphics import *
import random

win = Window("Octagons!")

turtle1 = Arrow(Point(30, 35))
turtle1.color = "red"

turtle1.draw(win)
win.mode = "animate"
win.animate_step_time = 5

hexes = []
count = 0
for rows in range(10):
    col = 6 if count % 2 == 0 else 5
    for cols in range(col):
        turtle1.pen_down()
        for x in range(8):
            turtle1.rotate(45)
            turtle1.forward(10)
            win.step()
        s = turtle1.pen_up()
        hexes.append(s)
        s.fill_color = random.choice(["red", "black", "green", "white"])
        s.outline_color = "black"
        s.draw(win)
        turtle1.move(35 + 17, 0)
    if count % 2 == 0:
        turtle1.move_to(30 + 27, turtle1.center.y + 27)
    else:
        turtle1.move_to(30, turtle1.center.y + 27)
    count += 1

win.animate_step_time = 20
#hexes = list(hexes)
for x in range(10):
    random.shuffle(hexes)
    for s in hexes:
        s.alpha = min(max(s.alpha + random.random() * .5 - .25, 0), 1)
        win.step()
