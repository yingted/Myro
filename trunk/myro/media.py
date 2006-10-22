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

C C# Db D D# Eb E E# Fb F F# Gb G# G Ab A A# Bb B B#

with the octave number following, from 0 (low) to 10 (high).

The middle octave on a keyboard is number 6, so a keyboard
goes from c4 through c8, with 4 complete octaves.

There are also a few other special note names, including PAUSE,
you can leave the octive number off of the middle octave notes
if you wish.

WHOLEPART can either be a decimal notation, or division. For
example:

Ab4 .125

or

Ab4 1/8

represents the A flat in the fourth octave (two below middle).

As an example, the beginning of Jingle Bells looks like:

c .125
c .125
d f .25
c .25
f .25

You may leave blank lines, and comments should begin with a #
sign.

s = readSong(filename): returns a list of tuples: [(freq1, [freq2], wholepart), ...]
robot.playSong(s): plays a song on the robot
"""

__VERSION__ = "$Revision$ "
__AUTHOR__  = "Doug Blank <dblank@cs.brynmawr.edu>"

_frequency = {
    'pause'   :        0,
    'c0'      :        8.2,
    'c#0'     :        8.7,
    'db0'     :        8.7,
    'd0'      :        9.2,
    'd#0'     :        9.7,
    'eb0'     :        9.7,
    'e0'      :       10.3,
    'f0'      :       10.9,
    'g0'      :       12.2,
    'f#0'     :       13,
    'gb0'     :       13,
    'a0'      :       13.8,
    'a#0'     :       14.6,
    'bb0'     :       14.6,
    'b0'      :       15.4,
    'c1'      :       16.4,
    'c#1'     :       17.3,
    'db1'     :       17.3,
    'd1'      :       18.4,
    'd#1'     :       19.4,
    'eb1'     :       19.4,
    'e1'      :       20.6,
    'f1'      :       21.8,
    'f#1'     :       23.1,
    'gb1'     :       23.1,
    'g1'      :       24.5,
    'ab1'     :       26,
    'g#1'     :       26,
    'a1'      :       27.5,
    'a#1'     :       29.1,
    'bb1'     :       29.1,
    'b1'      :       30.9,
    'c2'      :       32.7,
    'c#2'     :       34.6,
    'db2'     :       34.6,
    'd2'      :       36.7,
    'd#2'     :       38.9,
    'eb2'     :       38.9,
    'e2'      :       41.2,
    'f2'      :       43.7,
    'f#2'     :       46.2,
    'gb2'     :       46.2,
    'g2'      :       49,
    'ab2'     :       51.9,
    'g#2'     :       51.9,
    'a2'      :       55,
    'a#2'     :       58.3,
    'bb2'     :       58.3,
    'b2'      :       61.7,
    'c3'      :       65.4,
    'c#3'     :       69.3,
    'db3'     :       69.3,
    'd3'      :       73.4,
    'd#3'     :       77.8,
    'eb3'     :       77.8,
    'e3'      :       82.4,
    'f3'      :       87.3,
    'f#3'     :       92.5,
    'gb3'     :       92.5,
    'g3'      :       98,
    'ab3'     :      103.8,
    'g#3'     :      103.8,
    'a3'      :      110,
    'a#3'     :      116.5,
    'bb3'     :      116.5,
    'b3'      :      123.5,
    'c4'      :      130.8,
    'c#4'     :      138.6,
    'db4'     :      138.6,
    'd4'      :      146.8,
    'd#4'     :      155.6,
    'eb4'     :      155.6,
    'e4'      :      164.8,
    'f4'      :      174.6,
    'f#4'     :      185,
    'gb4'     :      185,
    'g4'      :      196,
    'ab4'     :      207.7,
    'g#4'     :      207.7,
    'a4'      :      220,
    'a#4'     :      233.1,
    'bb4'     :      233.1,
    'b4'      :      246.9,
    'c5'      :      261.6,
    'c#5'     :      277.2,
    'db5'     :      277.2,
    'd5'      :      293.7,
    'd#5'     :      311.1,
    'eb5'     :      311.1,
    'e5'      :      329.6,
    'f5'      :      349.2,
    'f#5'     :      370,
    'gb5'     :      370,
    'g5'      :      392,
    'ab5'     :      415.3,
    'g#5'     :      415.3,
    'a5'      :      440,
    'a#5'     :      466.2,
    'bb5'     :      466.2,
    'b5'      :      493.9,
    #------------------------------------ middle
    'c'       :      523.3,
    'c#'      :      554.4,
    'db'      :      554.4,
    'd'       :      587.3,
    'd#'      :      622.3,
    'eb'      :      622.3,
    'e'       :      659.3,
    'f'       :      698.5,
    'f#'      :      740,
    'gb'      :      740,
    'g'       :      784,
    'ab'      :      830.6,
    'g#'      :      830.6,
    'a'      :      880,
    'a#'     :      932.3,
    'bb'     :      932.3,
    'b'      :      987.8,
    #------------------------------------
    'c6'      :      523.3,
    'c#6'     :      554.4,
    'db6'     :      554.4,
    'd6'      :      587.3,
    'd#6'     :      622.3,
    'eb6'     :      622.3,
    'e6'      :      659.3,
    'f6'      :      698.5,
    'f#6'     :      740,
    'gb6'     :      740,
    'g6'      :      784,
    'ab6'     :      830.6,
    'g#6'     :      830.6,
    'a6'      :      880,
    'a#6'     :      932.3,
    'bb6'     :      932.3,
    'b6'      :      987.8,
    'c7'      :     1046.5,
    'c#7'     :     1108.7,
    'db7'     :     1108.7,
    'd7'      :     1174.7,
    'd#7'     :     1244.5,
    'eb7'     :     1244.5,
    'e7'      :     1318.5,
    'f7'      :     1396.9,
    'f#7'     :     1480,
    'gb7'     :     1480,
    'g7'      :     1568,
    'ab7'     :     1661.2,
    'g#7'     :     1661.2,
    'a7'      :     1760,
    'a#7'     :     1864.7,
    'bb7'     :     1864.7,
    'b7'      :     1975.5,
    'c8'      :     2093,
    'c#8'     :     2217.5,
    'db8'     :     2217.5,
    'd8'      :     2349.3,
    'd#8'     :     2489,
    'eb8'     :     2489,
    'e8'      :     2637,
    'f8'      :     2793.8,
    'f#8'     :     2960,
    'gb8'     :     2960,
    'g8'      :     3136,
    'ab8'     :     3322.4,
    'g#8'     :     3322.4,
    'a8'      :     3520,
    'a#8'     :     3729.3,
    'bb8'     :     3729.3,
    'b8'      :     3951.1,
    'c9'      :     4186,
    'c#9'     :     4434.9,
    'db9'     :     4434.9,
    'd9'      :     4698.6,
    'd#9'     :     4978,
    'eb9'     :     4978,
    'e9'      :     5274,
    'f9'      :     5587.7,
    'f#9'     :     5919.9,
    'gb9'     :     5919.9,
    'g9'      :     6271.9,
    'ab9'     :     6644.9,
    'g#9'     :     6644.9,
    'a9'      :     7040,
    'a#9'     :     7458.6,
    'bb9'     :     7458.6,
    'b9'      :     7902.1,
    'c10'     :     8372,
    'c#10'    :     8869.8,
    'db10'    :     8869.8,
    'd10'     :     9397.3,
    'd#10'    :     9956.1,
    'eb10'    :     9956.1,
    'e10'     :    10548.1,
    'f10'     :    11175.3,
    'f#10'    :    11839.8,
    'gb10'    :    11839.8,
    'g10'     :    12543.9,
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
    for key in _frequency.keys():
        if abs(_frequency[key] - frequency) < diff:
            diff = abs(_frequency[key] - frequency)
            diffNote = key
    return diffNote[0].upper() + diffNote[1:]

def _getDuration(v, line, text):
    """ Takes a string that is a fraction, or a number. Returns whole note portion as float. """
    if "/" in v:
        try:
            return eval(v + ".")
        except:
            raise ValueError, ("invalid duration value '%s' on line %d: %s" %
                               (v, line, text))
    return float(v)

def saveSong(song, filename, append = 1):
    if append:
        mode = "w+"
    else:
        mode = "w"
    fp = open(filename, mode) # will append it if it exists
    if type(song) in [list]:
        for tup in song:
            if len(tup) == 2:
                f, d = tup
                fp.write("%s %s\n" % (getNoteFromFrequency(f), d))
            elif len(tup) == 3:
                f1, f2, d = tup
                fp.write("%s %s %s\n" % (getNoteFromFrequency(f),
                                         getNoteFromFrequency(f), d))
    else: # string
        lines = song.split(";")
        for line in lines:
            fp.write(line + "\n")
    fp.close()

def makeSong(text):
    song = []
    songData = text.split(";")
    lineNumber = 1
    for line in songData:
        _parseSongLine(song, line, lineNumber, "text")
        lineNumber += 1
    return song
        
def readSong(filename = None):
    if filename == None: return []
    songFile = open(filename, "r")
    song = []
    lineNumber = 1
    for line in songFile:
        _parseSongLine(song, line, lineNumber, filename)
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

