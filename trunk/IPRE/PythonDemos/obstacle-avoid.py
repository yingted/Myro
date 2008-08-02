setIRPower(140)

print "Welcome, this program avoids obstacles (usually)..."

# Enter an endless loop.
# In this loop, we will read some sensor values, decide what to do, then do it!
# The loop means we'll keep repeating this cycle over and over again, quickly.
# Known as "sense, think, act"
while(get("led", "left") == 0.0):
    
    # Sense: Read the obstacle sensors (IR sensors)
    obstacle = getObstacle()
    obstacleLeft = obstacle[0]
    obstacleMiddle = obstacle[1]
    obstacleRight = obstacle[2]
    print "Obs:", obstacleLeft, ",", obstacleRight

    # Think: Perform a calculation on the sensor values to determine the
    # desired motion.
    # But if the motors are stalled, back up
    #if getStall() == 0:
    leftMotorSpeed = 0.6 * (1.0 - (obstacleRight / 500))
    rightMotorSpeed = 0.6 * (1.0 - (obstacleLeft / 500))
    #else:
    #    leftMotorSpeed = -1.0
    #    rightMotorSpeed = -0.6

    if leftMotorSpeed < 0.1 and rightMotorSpeed < 0.1:
        leftMotorSpeed = 0.1
        rightMotorSpeed = 0.1

    # For debugging, print the motor speeds
    print "Motors:", leftMotorSpeed, ",", rightMotorSpeed

    # Act: Set the motor speeds
    motors(leftMotorSpeed, rightMotorSpeed)

stop()