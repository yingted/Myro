using System;
using System.Collections.Generic;
using System.Text;

namespace Myro.API
{
    public interface IMyroSong
    {
        string SongName {get;set;}
    }

    public interface IMyroSound
    {
        IMyroSong ReadSong(string fileName);
        IMyroSong MakeSong(string text); // TODO (jaredj): Return type should be song data
        void SaveSong(string text, string fileName);
        void PlaySong(IMyroSong song);
        void beep(double duration, double frequency);
        void beep(double duration, double frequency1, double frequency2);
        void SetLoud(bool loud);
        // TODO (jaredj): Add a void SaveSong(MyroSong song, string fileName);
    }
}
