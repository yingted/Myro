using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Utilities;
using Myro.Adapters;

namespace Myro.API
{
    class MyroSound : IMyroSound
    {
        Myro.Adapters.AdapterSpec soundAdapter;

        public MyroSound(Myro.Adapters.AdapterSpec soundAdapter)
        {
            this.soundAdapter = soundAdapter;
        }

        public IMyroSong ReadSong(string fileName)
        {
            throw new NotImplementedException();
        }

        public IMyroSong MakeSong(string text)
        {
            throw new NotImplementedException();
        }

        public void SaveSong(string text, string fileName)
        {
            throw new NotImplementedException();
        }

        public void PlaySong(IMyroSong song)
        {
            throw new NotImplementedException();
        }

        public void beep(double duration, double frequency)
        {
            beep(duration, frequency, 0.0);
        }

        public void beep(double duration, double frequency1, double frequency2)
        {
            var values = new List<double>(){ frequency1, frequency2, duration };
            try
            {
                soundAdapter.GetVectorAdapter().SetAllElements(values);
            }
            catch (UnattachedAdapter)
            {
            }
        }
    }
}
