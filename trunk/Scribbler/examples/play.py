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

print "Reading a song...",
songFile = open("song.txt", "r")
song = []
for line in songFile:
    name, dur = line.split()
    song.append( (name, float(dur) * .2) )
    # FIX: long tones cause problems
    # 1 whole note should be .545 seconds
songFile.close()
print "done!"

def play(song):
    """ Plays a song (list of note names, durations) """
    for (name, dur) in song:
        print name, freq[name], dur
        robot.beep(freq[name], min(dur, .5))

print "Enter: 'play(song)' to play it"
