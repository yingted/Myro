import time

def update(num):
    for x in range(num):
        for t in turtles:
            t.move(random.random() * 6 - 3, 
                   random.random() * 6 - 3)
        #time.sleep(.0001)
    turtles[0].update()
    print "Ok"
