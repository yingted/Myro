"""
Reads a "song" file and plays it.
"""

from myro import *

robot = Scribbler("com7")

print "Reading frequency data...",
freqFile = open("freq.txt", "r")
freq = {}
for line in freqFile:
    name, f = line.split()
    freq[name] = float(f)
freqFile.close()
print "done!"

def readSong(filename):
    print ("Reading in song file '%s'..." % filename),
    songFile = open(filename, "r")
    song = []
    for line in songFile:
        name, dur = line.split()
        song.append( (name.upper(), float(dur)) )
    songFile.close()
    print "done!"
    return song

def play(filename, wholeNoteDuration = .545):
    """ Plays a song (list of note names, durations) """
    # 1 whole note should be .545 seconds for normal
    song = readSong(filename)
    for (name, dur) in song:
        print name, dur
        robot.beep(dur * wholeNoteDuration, freq[name])

print """>>> play("song.txt") ## wholeNoteDuration = .545 is normal tempo"""
play("song.txt")
