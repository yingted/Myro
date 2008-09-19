import myro.globvars
import os, sys, re, random

if not "darwin" in sys.platform:
    try:
        import pygame
        pygame.mixer.init(16000)
    except:
        pass

class TTSEngine:
    def __init__(self, name = None, echo = 1):
	self.name = name
	self.echo = echo 
    def speak(self, message, async = 0):
	if self.echo:
            print message
    def stop(self):
        pass
    def setVoice(self, name):
        pass
    def getVoice(self):
        return self.name
    def getVoices(sel):
        return []
    def playSpeech(self, filename):
        sound = pygame.mixer.Sound(filename)
        sound.play()
    def saveSpeech(self, message, filename):
        pass

class LinuxTTSEngine(TTSEngine):
    def speak(self, message, async=0):
        if self.echo:
            print message
        self.filename = "/tmp/%06d.wav" % random.randint(1,999999)
        message = message.replace('"', '\\"')
        os.system("""echo "%s" | text2wave -scale 10 -o %s"""
                  % (message, self.filename))
        self.playSpeech(self.filename)

class WindowsTTSEngine(TTSEngine):
    def __init__(self, name = None, echo = 1):
        import pyTTS
        self.tts = pyTTS.Create()
        if name != None:
            self.setVoice(name)
	self.echo = echo

    def speak(self, message, async = 0):
	if self.echo:
	    print message
        self.tts.Speak(message, async) # 0 is default, 1 is async

    def setVoice(self, name):
        self.tts.SetVoiceByName(name) # For example, 'MSMary'

    def getVoices(self):
        return map(str, self.tts.GetVoiceNames())

    def getVoice(self):
        return self.tts.GetVoice()

    def stop(self):
        self.tts.Stop()
    # --------------------------------------------------
    def playSpeech(self, filename):
        self.tts.SpeakFromWave(filename)

    def saveSpeech(self, message, filename):
        self.tts.SpeakToWave(filename, message)

class MacTTSEngine(TTSEngine):
    def __init__(self, name = None, echo = 1):
        self.echo = echo
        if name:
            self.voice = name
        else:
            self.voice = ""

    def speak(self, message, async = 0):
        if self.echo:
            print message

        if async:
            background = "&"
        else:
            background = ""
            
        cmd = "say -v \"%s\" \"%s\" %s" % (self.voice, message, background)
        os.system(cmd)

    def setVoice(self, name):
        self.voice = name

    def getVoices(self):
        return ['Agnes', 'Kathy', 'Princess', 'Vicki','Victoria', 'Bruce', 'Fred','Junior', 'Ralph','Albert', 'Bad News','Bahh', 'Bells' 'Boing', 'Bubbles', 'Cellos','Deranged','Good News', 'Hysterical','Pipe Organ', 'Trinoids', 'Whisper', 'Zarvox']

    def getVoice(self):
        return self.voice

def speak(message, async = 0):
    if myro.globvars.tts != None:
        myro.globvars.tts.speak(message, async)
    else:
        print "Text-to-speech is not loaded"
def stopSpeaking():
    if myro.globvars.tts != None:
        myro.globvars.tts.stop()
    else:
        print "Text-to-speech is not loaded"
def setVoice(name):
    if myro.globvars.tts != None:
        myro.globvars.tts.setVoice(name)
    else:
        print "Text-to-speech is not loaded"
def getVoice():
    if myro.globvars.tts != None:
        return myro.globvars.tts.getVoice()
    else:
        print "Text-to-speech is not loaded"
def getVoices():
    if myro.globvars.tts != None:
        return myro.globvars.tts.getVoices()
    else:
        print "Text-to-speech is not loaded"
def playSpeech(filename):
    if myro.globvars.tts != None:
        myro.globvars.tts.playSpeech(filename)
    else:
        print "Text-to-speech is not loaded"
def playSound(filename):
    myro.globvars.tts.playSpeech(filename)

def saveSpeech(message, filename):
    if myro.globvars.tts != None:
        myro.globvars.tts.saveSpeech(message, filename)
    else:
        print "Text-to-speech is not loaded"

def makeStory(story):
    from myro import ask
    # go through story, get "items"
    variables = re.findall('"(.*?)"', story)
    variables = list(set(variables))
    variables.sort()
    variables = map(lambda v: "%s =" % v, variables)
    values = ask(variables, useDict=1,
                 title = "For each variable below, fill in a value:")
    for variable in variables:
        value = values[variable]
        if value == "":
            value = '"%s"' % variable.replace(" =", "")
        variable = variable.replace(" =", "")
        story = story.replace('"%s"' % variable, value)
    return story

def variables():
    from myro import askQuestion
    raceStory = """
One day, the "animal1" and the "animal2" decided to race to the "place_name".
The "animal1" decided to go the "adjective1" way while "animal2" decided to go the "adjective2" way.
On the way, the "animal1" "verb1_past" under a "noun1".
Meanwhile, the "animal2" was winning.
Soon, the "animal2"'s "part_of_body" got tangled with a "noun2" and it fell.
The "animal1" caught up with the "animal2" and "verb2_past" that the "animal2" was in trouble.
So, the "animal1" untangled the "animal2"'s "part_of_body" and "noun2".
They both went to the "place_name" and won together!"""
    redStory = """
One day, "name" was going to "place_name1" to "verb1_present" her grandmother.
"name" saw a "adjective1" "place_name2" and decided to "verb2_present" some "noun1".
On the way, a "animal" stopped her to ask her for the "noun2".
But, the "animal" did not want the "noun2", instead, the "animal" "verb3_past" on "name".
Then "name" "verb4_past" the "animal"'s "part_of_body" and ran to "verb5_present" her grandmother."""
    if askQuestion("Which story?", ["The Race", "Red Riding Hood"]) == "The Race":
        story = makeStory(raceStory)
    else:
        story = makeStory(redStory)
    speak(story, async=1)
    return story

def numberGame(stop=100, maxGuesses=10):
    from myro import ask
    count = 1
    secret = int(random.random() * 100) + 1
    ok = False
    guess = 0
    while not ok:
        try:
            guess = int(ask("Number", 
                            title = "Try #%d: Guess a number between 1 and %d" % (count, stop)
                            ))
            ok = True
        except KeyboardInterrupt:
            raise
        except:
            ok = False
    while (guess != secret and count <= maxGuesses):
        if guess < secret:
            speak("On try number %d you guessed %d and that is too low." % 
                  (count, guess), async=1)
        else:
            speak("On try number %d you guessed %d and that is too high." % 
                  (count, guess), async=1)
        guess = 0
        ok = False
        while not ok:
            try:
                guess = int(ask("Number", 
                                title = "Try #%d: Guess a number between 1 and %d" % (count, stop)))
                ok = True
            except KeyboardInterrupt:
                raise
            except:
                ok = False
        count += 1
    if count > maxGuesses:
        speak("Sorry, but you didn't guess it in %d tries. My number was %d." %
              (maxGuesses, secret), async=1)
    else:
        speak("You guessed my secret number in %d tries! It was %d." %
              (count, secret), async=1)

if "darwin" in sys.platform:
    try:
        myro.globvars.tts = MacTTSEngine()
    except:
        myro.globvars.tts = TTSEngine()
elif "win" in sys.platform:
    try:
        myro.globvars.tts = WindowsTTSEngine()
    except:
        myro.globvars.tts = TTSEngine()
elif "linux" in sys.platform:
    myro.globvars.tts = LinuxTTSEngine()
else:
    myro.globvars.tts = TTSEngine()

_functions = (
    "speak",
    "stop Speaking",
    "set Voice",
    "get Voice",
    "get Voices",
    "play Speech",
    "save Speech",
    "make Story",
    "number Game"
)

myro.globvars.makeEnvironment(locals(), _functions)
