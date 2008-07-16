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
        AdapterSpec<VectorAdapter> soundAdapter;

        public MyroSound(Myro.Adapters.AdapterBank bank)
        {
            soundAdapter = bank.GetAdapterSpec<VectorAdapter>("tonegen");
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
            ((VectorAdapter)soundAdapter.Adapter).Set(
                new List<int>() { 0, 1, 2 },
                new List<double>() { frequency1, frequency2, duration });
        }

        public void SetLoud(bool loud)
        {
            ((VectorAdapter)soundAdapter.Adapter).Set(3, loud ? 1.0 : 0.0);
        }
    }
}
