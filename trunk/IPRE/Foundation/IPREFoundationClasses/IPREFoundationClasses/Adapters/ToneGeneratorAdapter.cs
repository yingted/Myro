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

using tonegen = IPREGenericContracts.ToneGenerator.Proxy;

namespace IPREFoundationClasses.Adapters
{
    public class ToneGeneratorAdapter : MyroInterfaces.IMyroSound, IAdapter
    {
        protected string serviceUri;
        protected tonegen.ToneGeneratorOperations tonePort;

        public ToneGeneratorAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port and subscribe to the service
            tonePort = DssEnvironment.ServiceForwarder<tonegen.ToneGeneratorOperations>(new Uri(serviceUri));
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.ToneGeneratorAdapter; }
        }

        public MyroInterfaces.IMyroSong ReadSong(string filename)
        {
            MyroSong song = new MyroSong();
            //song.SongName = filename;
            song.readSong(filename);
            return song;
        }

        public MyroInterfaces.IMyroSong MakeSong(string text)
        {
            MyroSong song = new MyroSong();
            song.makeSong(text);
            return song;
        }

        public void PlaySong(MyroInterfaces.IMyroSong song)
        {
            MyroSong msong = (MyroSong)song;

            foreach (MyroSong.ListItem li in msong.songSequence)
            {
                if (li.chord)
                {
                    tonegen.PlayTone2Request request = new tonegen.PlayTone2Request();
                    request.Duration = (int)(li.duration * 1000f);
                    request.Frequency1 = (int)li.frequency1;
                    request.Frequency2 = (int)li.frequency2;
                    tonegen.PlayTone2 ptone = new tonegen.PlayTone2(request);
                    tonePort.Post(ptone);
                    bool done = false;
                    Arbiter.Activate(DssEnvironment.TaskQueue,
                        Arbiter.Receive<DefaultUpdateResponseType>(false,
                            ptone.ResponsePort,
                            delegate(DefaultUpdateResponseType state)
                            {
                                done = true;
                            }
                    ));

                    while (!done) ;
                }
                else
                {
                    tonegen.PlayToneRequest request = new tonegen.PlayToneRequest();
                    request.Duration = (int)(li.duration * 1000f);
                    request.Frequency = (int)li.frequency1;
                    tonegen.PlayTone ptone = new tonegen.PlayTone(request);
                    tonePort.Post(ptone);
                    bool done = false;
                    Arbiter.Activate(DssEnvironment.TaskQueue,
                        Arbiter.Receive<DefaultUpdateResponseType>(false,
                            ptone.ResponsePort,
                            delegate(DefaultUpdateResponseType state)
                            {
                                done = true;
                            }
                    ));
                    while (!done) ;
                }
            }
        }

        public void SaveSong(string text, string fileName)
        {
            throw new NotImplementedException();
        }

        public void beep(float duration, float frequency)
        {
            bool flag = false;
            tonegen.PlayToneRequest request = new tonegen.PlayToneRequest();
            request.Duration = (int)duration;
            request.Frequency = (int)frequency;

            tonegen.PlayTone ptone = new tonegen.PlayTone(request);
            tonePort.Post(ptone);

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<DefaultUpdateResponseType>(false, ptone.ResponsePort,
                delegate(DefaultUpdateResponseType state)
                {
                    flag = true;
                }
            ));

            while (!flag) ;
        }

        public void beep(float duration, float frequency1, float frequency2)
        {
            bool flag = false;
            tonegen.PlayTone2Request request = new tonegen.PlayTone2Request();
            request.Duration = (int)duration;
            request.Frequency1 = (int)frequency1;
            request.Frequency2 = (int)frequency2;

            tonegen.PlayTone2 ptone = new tonegen.PlayTone2(request);
            tonePort.Post(ptone);

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<DefaultUpdateResponseType>(false, ptone.ResponsePort,
                delegate(DefaultUpdateResponseType state)
                {
                    flag = true;
                }
            ));

            while (!flag) ;
        }

        public override bool Equals(Object obj)
        {
            if (obj is String)
            {
                string truncUri = serviceUri.Substring(serviceUri.IndexOf('/', serviceUri.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.serviceUri, ((ToneGeneratorAdapter)obj).serviceUri);
        }
    }

}
