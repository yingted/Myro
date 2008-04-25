using System;
using System.Collections.Generic;
using System.Text;

namespace MyroInterfaces
{
    public interface IMyroSpeech
    {
        string GetVoice();
        void SetVoice(string voiceName);
        void Speak(string message, bool async);
        void Speak(string message);
        string[] GetVoices();
        void PlaySpeech(string fileName); // REVIEW (jaredj): PlaySound maybe since if plays a WAV file
        void SaveSpeech(string message, string fileName);
    }
}
