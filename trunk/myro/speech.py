import myro.globvars
import os

class TTSEngine:
    def __init__(self, name = None, echo = 1):
	self.name = name
	self.echo = echo 
    def speak(self, message, async = 1):
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

class LinuxTTSEngine(TTSEngine):
    def speak(self, message, async=1):
        if async:
            background = "&"
        else:
            background = ""
        os.system("""echo "%s" | festival --tts %s""" % (message, background))


class WindowsTTSEngine(TTSEngine):
    def __init__(self, name = None, echo = 1):
        import pyTTS
        self.tts = pyTTS.Create()
        if name != None:
            self.setVoice(name)
	self.echo = echo

    def speak(self, message, async = 1):
        self.tts.Speak(message, async) # 0 is default, 1 is async
	if self.echo:
	    print message

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
            
def speak(message, async = 1):
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
def saveSpeech(message, filename):
    if myro.globvars.tts != None:
        myro.globvars.tts.saveSpeech(message, filename)
    else:
        print "Text-to-speech is not loaded"
           
try:
    myro.globvars.tts = WindowsTTSEngine()
except:
    try:
       myro.globvars.tts = LinuxTTSEngine()
    except:
       myro.globvars.tts = None
    
