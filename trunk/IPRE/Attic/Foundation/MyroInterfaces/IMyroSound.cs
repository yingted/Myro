using System;
using System.Collections.Generic;
using System.Text;

namespace MyroInterfaces
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
        void beep(float duration, float frequency);
        void beep(float duration, float frequency1, float frequency2);
        // TODO (jaredj): Add a void SaveSong(MyroSong song, string fileName);
    }
}
