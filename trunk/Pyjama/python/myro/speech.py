import clr

try:
    clr.AddReference('System.Speech')
except:
    pass

from System.Speech.Synthesis import SpeechSynthesizer

spk = SpeechSynthesizer()
spk.Speak('Hello world!')

voices = spk.GetSelectedVoices()
names = [voice.VoiceInfo.Name for voice in voices]

# Select the first voice in the list
spk.SelectVoice(names[0])
