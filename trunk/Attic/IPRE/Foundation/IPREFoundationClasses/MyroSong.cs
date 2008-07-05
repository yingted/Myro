using System.Collections;
using System.Collections.Generic;
using System.IO;
using System;


namespace IPREFoundationClasses
{
    /// <summary>
    /// MyroSong class. Encapsulates all information about a song that can be played by Robots.
    /// </summary>
    public class MyroSong : MyroInterfaces.IMyroSong
    {
        /// <summary>
        /// The name of the song.
        /// </summary>
        protected string songName;
        
        /// <summary>
        /// Accessor functions.
        /// </summary>
        public string SongName
        {
            get { return songName; }
            set { SongName = value; }
        }

        /// <summary>
        /// Conversion lookup table between the tone letter and its frequency
        /// </summary>
        public static Dictionary<string, float> conversionTable = null;

        /// <summary>
        /// The actual song sequence which is played.
        /// </summary>
        public List<ListItem> songSequence = null;

        public MyroSong()
        {
            if (conversionTable == null)
                populateConversionTable();
        }

        /// <summary>
        /// Creates a song object from its text representation
        /// </summary>
        /// <param name="songStr">The text form of the song</param>
        public void makeSong(string songStr)
        {
            songSequence = new List<ListItem>();

            String[] noteStr = songStr.Split(';');
            foreach (string note in noteStr)
            {
                String[] parts = note.Split(' ');
                ListItem li = new ListItem();

                float freq;
                string durationString;

                if (!conversionTable.TryGetValue(parts[0], out freq))
                    throw new IllegalFormatException();
                li.frequency1 = freq;

                if (parts.Length == 3)
                {
                    if (!conversionTable.TryGetValue(parts[1], out freq))
                        throw new IllegalFormatException();
                    else
                    {
                        li.frequency2 = freq;
                        li.chord = true;
                    }
                    durationString = parts[2];
                }
                else
                    durationString = parts[1];

                int slash = durationString.IndexOf('/');
                if (slash == -1)
                    li.duration = float.Parse(durationString);
                else
                    li.duration = float.Parse(durationString.Substring(0, slash)) / float.Parse(durationString.Substring(slash));

                songSequence.Add(li);
            }
        }

        /// <summary>
        /// Reads in a songfile and converts it to a song object
        /// </summary>
        /// <param name="filename">The filename containing the text representation of the song.</param>
        public void readSong(string filename)
        {
            TextReader reader = new StreamReader(filename);
            if (reader == null)
                throw new FileNotFoundException();

            songSequence = new List<ListItem>();

            string str = reader.ReadLine();
            while (str != null)
            {
                String[] noteStr = str.Split(';');
                foreach (string note in noteStr)
                {
                    String[] parts = note.Split(' ');
                    ListItem li = new ListItem();

                    float freq;
                    string durationString;

                    if (!conversionTable.TryGetValue(parts[0], out freq))
                        throw new IllegalFormatException();
                    li.frequency1 = freq;

                    if (parts.Length == 3)
                    {
                        if (!conversionTable.TryGetValue(parts[1], out freq))
                            throw new IllegalFormatException();
                        else
                        {
                            li.frequency2 = freq;
                            li.chord = true;
                        }
                        durationString = parts[2];
                    }
                    else
                        durationString = parts[1];

                    int slash = durationString.IndexOf('/');
                    if (slash == -1)
                        li.duration = float.Parse(durationString);
                    else
                        li.duration = float.Parse(durationString.Substring(0, slash)) / float.Parse(durationString.Substring(slash));

                    songSequence.Add(li);
                }
                str = reader.ReadLine();
            }
            reader.Close();
        }

        /// <summary>
        /// Populates the conversion lookup table.
        /// </summary>
        private void populateConversionTable()
        {
            TextReader reader = new StreamReader(@"C:\Microsoft Robotics Studio (1.5)\samples\IPRE\Foundation\IPREFoundationClasses\IPREFoundationClasses\musicnotes.csv");
            if (reader == null)
                throw new FileNotFoundException();
           
            conversionTable = new Dictionary<string, float>();

            string str = reader.ReadLine();
            while (str != null)
            {
                int comma = str.IndexOf(',');
                conversionTable.Add(str.Substring(0, comma), float.Parse(str.Substring(comma + 1)));
                str = reader.ReadLine();
            }
            reader.Close();
        }

        /// <summary>
        /// A "note" in the song.
        /// </summary>
        public class ListItem
        {
            public bool chord = false;
            public float frequency1;
            public float frequency2;
            public float duration;
        }

        public class IllegalFormatException: Exception
        {
        }
    }
}