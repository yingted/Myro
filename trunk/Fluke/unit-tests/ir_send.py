from myro import *
import sys

init()

setIRPower(250)
sendIRMessage('Hello World')

while 1:
    for ch in 'abcdefghijklmnopqrstuvwxyz':
        sendIRMessage(ch)
        wait(.5)
