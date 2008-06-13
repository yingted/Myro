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
        Myro.Adapters.AdapterBank bank;
        Myro.Adapters.AdapterSpec _soundAdapter = null;
        Myro.Adapters.AdapterSpec SoundAdapter
        {
            get
            {
                if (_soundAdapter == null)
                    try
                    {
                        _soundAdapter = bank.GetAdapterSpec("tonegen");
                    }
                    catch (Myro.Adapters.UnknownAdapterNameException)
                    {
                    }
                return _soundAdapter;
            }
        }

        public MyroSound(Myro.Adapters.AdapterBank bank)
        {
            this.bank = bank;
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
                SoundAdapter.GetVectorAdapter().SetAllElements(values);
            }
            catch (UnattachedAdapter)
            {
            }
        }
    }
}
