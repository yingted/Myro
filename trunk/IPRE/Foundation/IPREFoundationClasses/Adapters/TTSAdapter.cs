using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Hosting;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System.Threading;
using W3C.Soap;

using tts = Microsoft.Robotics.Technologies.Speech.TextToSpeech.Proxy;


namespace IPREFoundationClasses.Adapters
{
    public class TTSAdapter : MyroInterfaces.IMyroSpeech, IAdapter
    {
        protected string serviceUri;
        protected tts.SpeechTextOperations ttsPort;

        public TTSAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port and subscribe to the service
            ttsPort = DssEnvironment.ServiceForwarder<tts.SpeechTextOperations>(new Uri(serviceUri));
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.TextToSpeech; }
        }

        public override bool Equals(Object obj)
        {
            if (obj is String)
            {
                string truncUri = serviceUri.Substring(serviceUri.IndexOf('/', serviceUri.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.serviceUri, ((TTSAdapter)obj).serviceUri);
        }

        public virtual string GetVoice()
        {
            string voice = null;
            AutoResetEvent aevent = new AutoResetEvent(false);

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice(ttsPort.Get(),
                    delegate(tts.TextToSpeechState state)
                    {
                        voice = state.Voice;
                        aevent.Set();
                    },
                    delegate(Fault f)
                    {
                        aevent.Set();
                    }
            ));

            aevent.WaitOne(1000, false);
            return voice;
        }

        public virtual void SetVoice(string voiceName)
        {
            tts.SetVoiceRequest stReq = new tts.SetVoiceRequest();
            stReq.Voice = voiceName;
            ttsPort.SetVoice(stReq);
        }

        public virtual void Speak(string message, bool async)
        {
            tts.SayTextRequest streq = new tts.SayTextRequest();
            streq.SpeechText = message;
            if (async)
                ttsPort.SayText(streq);
            else
            {
                AutoResetEvent aevent = new AutoResetEvent(false);
                Arbiter.Activate(DssEnvironment.TaskQueue,
                    Arbiter.Choice(ttsPort.SayText(streq),
                        delegate(DefaultUpdateResponseType state)
                        {
                            aevent.Set();
                        },
                        delegate(Fault f)
                        {
                            aevent.Set();
                        }
                ));
                aevent.WaitOne(1000, false);
            }
        }

        public virtual void Speak(string message)
        {
            tts.SayTextRequest streq = new tts.SayTextRequest();
            streq.SpeechText = message;

            AutoResetEvent aevent = new AutoResetEvent(false);

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice(ttsPort.SayText(streq),
                    delegate(DefaultUpdateResponseType state)
                    {
                        aevent.Set();
                    },
                    delegate(Fault f)
                    {
                        aevent.Set();
                    }
            ));
            aevent.WaitOne(1000, false);
        }

        public virtual string[] GetVoices()
        {
            string[] voiceList = null;
            AutoResetEvent aevent = new AutoResetEvent(false);

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice(ttsPort.Get(),
                    delegate(tts.TextToSpeechState state)
                    {
                        voiceList = state.Voices.ToArray();
                        aevent.Set();
                    },
                    delegate(Fault f)
                    {
                        aevent.Set();
                    }
            ));

            aevent.WaitOne(1000, false) ;
            return voiceList;
        }

        public virtual void PlaySpeech(string fileName)        // REVIEW (jaredj): PlaySound maybe since if plays a WAV file
        {
            throw new NotImplementedException();
        }

        public virtual void SaveSpeech(string message, string fileName)
        {
            throw new NotImplementedException();
        }
    }

}
