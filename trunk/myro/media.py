"""
Media manipulation.

A Myro song is composed of lines in a file like:

NOTE1 NOTE2 WHOLEPART
NOTE1 NOTE2 WHOLEPART
...

where:
    NOTE1 is either a frequency or a NOTENAME
    NOTE2 is the same, and optional. Use for Chords.
    WHOLEPART is a number representing how much of
              a whole note to play.

NOTENAMES are case-insensitive strings laid out like:

C C#/Db D D#/Eb E F F#/Gb G G#/Ab A A#/Bb B C

with the octave number following, from 0 (low) to 9 (high).

The middle octave on a keyboard is number 4, so a keyboard
goes from C0 through C9, with 8 complete octaves. There are 
88 keys.

There are also a few other special note names, including PAUSE,
you can leave the octive number off of the middle octave notes
if you wish. Use "#" for sharp, and "b" for flat.

WHOLEPART can either be a decimal notation, or division. For
example:

Ab2 .125

or

Ab2 1/8

represents the A flat in the second octave (two below middle).

As an example, the beginning of Jingle Bells looks like:

C .125
C .125
D F .25
C .25
F .25

You may leave blank lines, and comments should begin with a #
sign.

s = readSong(filename): returns a list of tuples: [(freq1, [freq2], wholepart), ...]
robot.playSong(s): plays a song on the robot
"""

__VERSION__ = "$Revision$ "
__AUTHOR__  = "Doug Blank <dblank@cs.brynmawr.edu>"

import globvars

_frequency = {
              "rest":     0,
              "pause":    0,
              "a0":   27.50,
              "a#0":   29.14,
              "bb0":   29.14,
              "b0":   30.87,
              "c1":   32.70,
              "c#1":   34.65,
              "db1":   34.65,
              "d1":   36.71,
              "d#1":   38.89,
              "eb1":   38.89,
              "e1":   41.20,
              "f1":   43.65,
              "f#1":   46.25,
              "gb1":   46.25,
              "g1":   49.00,
              "g#1":   51.91,
              "ab1":   51.91,
              "a1":   55.00,
              "a#1":   58.27,
              "bb1":   58.27,
              "b1":   61.74,
              "c2":   65.41,
              "c#2":   69.30,
              "db2":   69.30,
              "d2":   73.42,
              "d#2":   77.78,
              "eb2":   77.78,
              "e2":   82.41,
              "f2":   87.31,
              "f#2":   92.50,
              "gb2":   92.50,
              "g2":   98.00,
              "g#2":   103.80,
              "ab2": 103.80,
              "a2":   110.00,
              "a#2":  116.50,
              "bb2":   116.50,
              "b2":   123.471,
              "c3":   130.8,
              "c#3":   138.6,
              "db3":   138.6,
              "d3":   146.8,
              "d#3":   155.6,
              "eb3":   155.6,
              "e3":   164.8,
              "f3":   174.6,
              "f#3":   185.0,
              "gb3":   185.0,
              "g3":   196.0,
              "g#3":   207.7,
              "ab3":   207.7,
              "a3":   220.0,
              "a#3":   233.1,
              "bb3":   233.1,
              "b3":   246.9,
              "c4":   261.6,
              "c#4":   277.2,
              "db4":   277.2,
              "d4":   293.7,
              "d#4":   311.1,
              "eb4":   311.1,
              "e4":   329.6,
              "f4":   349.2,
              "f#4":   370.0,
              "gb4":   370.0,
              "g4":   392.0,
              "g#4":   415.3,
              "ab4":   415.3,
              "a4":   440.0,
              "a#4":   466.2,
              "bb4":   466.2,
              "b4":   493.9,
              "c5":   523.3,
              "c#5":   554.4,
              "db5":   554.4,
              "d5":   587.3,
              "d#5":   622.3,
              "eb5":   622.3,
              "e5":   659.3,
              "f5":   698.5,
              "f#5":   740.0,
              "gb5":   740.0,
              "g5":   784.0,
              "g#5":   830.6,
              "ab5":   830.6,
              "a5":   880.0,
              "a#5":   932.3,
              "bb5":   932.3,
              "b5":   987.8,
              # -------------------- default octave
              "c":   523.3,
              "c#":   554.4,
              "db":   554.4,
              "d":   587.3,
              "d#":   622.3,
              "eb":   622.3,
              "e":   659.3,
              "f":   698.5,
              "f#":   740.0,
              "gb":   740.0,
              "g":   784.0,
              "g#":   830.6,
              "ab":   830.6,
              "a":   880.0,
              "a#":   932.3,
              "bb":   932.3,
              "b":   987.8,
              # --------------------
              "c6":   1047,
              "c#6":   1109,
              "db6":   1109,
              "d6":   1175,
              "d#6":   1245,
              "eb6":   1245,
              "e6":   1319,
              "f6":   1397,
              "f#6":   1480,
              "gb6":   1480,
              "g6":   1568,
              "g#6":   1661,
              "ab6":   1661,
              "a6":   1760,
              "a#6":   1865,
              "bb6":   1865,
              "b6":   1976,
              "c7":   2093,
              "c#7":   2217,
              "db7":   2217,
              "d7":   2349,
              "d#7":   2489,
              "eb7":   2489,
              "e7":   2637,
              "f7":   2794,
              "f#7":   2960,
              "gb7":   2960,
              "g7":   3136,
              "g#7":   3322,
              "ab7":   3322,
              "a7":   3520,
              "a#7":   3729,
              "bb7":   3729,
              "b7":   3951,
              "c8":   4186,
             }

def _getFrequency(s, line, text):
    """ Takes a string that is a note name, or a frequency. Returns """
    if len(s) > 0 and s[0].isalpha(): 
        if s.lower() in _frequency:
            return _frequency[s.lower()]
        else:
            raise ValueError, "invalid note name/frequency '%s' on line %d: %s" % (s, line, text)
    else:
        return int(float(s))

def getNoteFromFrequency(frequency):
    """ Return closest note name based on a given frequency. """
    diff = 100000
    diffNote = None
    for key in sorted(_frequency.keys()):
        if abs(_frequency[key] - frequency) < diff:
            diff = abs(_frequency[key] - frequency)
            diffNote = key
    return diffNote[0].upper() + diffNote[1:]

_getNoteFromFrequency = getNoteFromFrequency

def _getDuration(v, line, text):
    """ Takes a string that is a fraction, or a number. Returns whole note portion as float. """
    if "/" in v:
        try:
            return eval(v + ".")
        except:
            raise ValueError, ("invalid duration value '%s' on line %d: %s" %
                               (v, line, text))
    return float(v)

def song2text(song):
    """ Given a song list, return a text string form """
    text = ""
    for tup in song:
        if len(tup) == 2:
            f, d = tup
            text += "%s %s; " % (_getNoteFromFrequency(f), d)
        elif len(tup) == 3:
            f1, f2, d = tup
            text += "%s %s %s; " % (_getNoteFromFrequency(f1),
                                    _getNoteFromFrequency(f2), d)
    return text

def saveSong(song, filename, append = 1):
    """ Writes a song list to a file. """
    if append:
        mode = "w+"
    else:
        mode = "w"
    fp = open(filename, mode) # will append it if it exists
    if type(song) in [list]:
        for tup in song:
            if len(tup) == 2:
                f, d = tup
                fp.write("%s %s\n" % (_getNoteFromFrequency(f), d))
            elif len(tup) == 3:
                f1, f2, d = tup
                fp.write("%s %s %s\n" % (_getNoteFromFrequency(f),
                                         _getNoteFromFrequency(f), d))
    else: # string
        song = song.replace("\n", ";")
        lines = song.split(";")
        for line in lines:
            fp.write(line + "\n")
    fp.close()

def makeSong(text):
    """ Given a text string format of a song, return a song list """
    song = []
    text = text.replace("\n", ";")
    songData = text.split(";")
    lineNumber = 1
    for line in songData:
        _parseSongLine(song, line, lineNumber, "text")
        lineNumber += 1
    return song

text2song = makeSong # alias, for symmetry since we have song2text

def readSong(filename = None):
    """ Read a song file. Returns a song list """
    if filename == None: return []
    songFile = open(filename, "r")
    song = []
    lineNumber = 1
    for line in songFile:
        notes = line.split(";")
        for n in notes:
            _parseSongLine(song, n, lineNumber, filename)
        lineNumber += 1
    songFile.close()
    return song

def _parseSongLine(song, line, lineNumber, filename):
    name1 = name2 = None
    lineList = line.split()
    if len(lineList) == 0:
        # blank line, skip
        pass
    elif lineList[0][0] == "#":
        # first word, first char is #, then skip comment
        pass
    elif len(lineList) == 2:
        name1, dur = line.split()
        song.append( (_getFrequency(name1, lineNumber, line),
                      _getDuration(dur, lineNumber, line)))
    elif len(lineList) == 3:
        name1, name2, dur = line.split()
        song.append( (_getFrequency(name1, lineNumber, line),
                      _getFrequency(name2, lineNumber, line),
                      _getDuration(dur, lineNumber, line)) )
    else:
        raise ValueError, ("song format error in '%s' at line %d: %s" %
                           (filename, lineNumber, line))

class Song:
    def __init__(self, robot, filename = None):
        self.robot = robot
        self.filename = filename
        self.position = 0
        self.song = getSong(self.filename)
    def play(self):
        self.robot.play(self.song)
        self.position = 0
    def playNextNote(self):
        if self.position < len(self.song):
            self.robot.playNote(self.song[self.position])
            self.position += 1
            return 1
        else:
            self.position = 0
            return 0
    # TODO:make an iterator

_functions = ('get Note From Frequency',
              'song2text',
              'save Song',
              'make Song',
              'read Song',)

globvars.makeEnvironment(locals(), _functions)
