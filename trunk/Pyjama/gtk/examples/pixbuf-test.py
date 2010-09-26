from Graphics import *
win = Window()
pic = Picture("images/blankenship.jpg")
pic.draw(win)

arrow = Arrow(Point(10, 10), 0)
arrow.draw(win)

win.update()

win.mode = "animate"
for x in range(36):
    pic.rotate(10)
    win.step()
