def move():
    for x in range(100):
        for t in turtles:
            t.move(random.random() * 6 - 3, 
                   random.random() * 6 - 3)
        win.update()
