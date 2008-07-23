import math
import time

num_times = 3

v = 0
while v <= 2.0 * math.pi * num_times:
	ledpower = 0.1 * (-math.cos(v) + 1)
	#print ledpower
	setLEDBack(ledpower)
	v = v + .03
	time.sleep(0.01)

setLEDBack(0)
