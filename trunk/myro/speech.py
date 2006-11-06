import myro.globals

class TTSEngine:
    def Speak(self, message, async = 1):
        print message
    def stop(self):
        pass
    def setVoice(self, name):
        pass
    def getVoice(self):
        return None
    def getVoices(sel):
        return []

class WindowsTTSEngine(TTSEngine):
    def __init__(self, name = None):
        import pyTTS
        self.tts = pyTTS.Create()
        if name != None:
            self.setVoice(name)

    def speak(self, message, async = 1):
        self.tts.Speak(message, async) # 0 is default, 1 is async

    def setVoice(self, name):
        self.tts.SetVoiceByName(name) # For example, 'MSMary'

    def getVoices(self):
        return self.tts.GetVoiceNames()

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
    myro.globals.tts.speak(message, async)
def stopSpeaking():
    myro.globals.tts.stop()
def setVoice(name):
    myro.globals.tts.setVoice(name)
def getVoice():
    return myro.globals.tts.getVoice()
def getVoices():
    return myro.globals.tts.getVoices()
def playSpeech(filename):
    myro.globals.tts.playSpeech(filename)
def saveSpeech(message, filename):
    myro.globals.tts.saveSpeech(message, filename)

if myro.globals.tts == None:
    try:
        myro.globals.tts = WindowsTTSEngine()
    except:
        myro.globals.tts = TTSEngine()
    
