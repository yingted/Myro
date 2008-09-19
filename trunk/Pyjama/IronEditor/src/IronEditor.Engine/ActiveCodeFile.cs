using System.IO;

namespace IronEditor.Engine
{
    public class ActiveCodeFile
    {
        public string FileName
        {
            get
            {
                return Location != null ? Path.GetFileName(Location) : string.Empty;
            }
        }

        public string FileExtension
        {
            get
            {
                return Path.GetExtension(Location);
            }
        }

        public string Location { get; set; }

        public bool Untitled { get; set; }

        public bool Unsaved { get; set; }

        public ActiveCodeFile()
        {
            Untitled = false;
            Unsaved = false;
        }

        public ActiveCodeFile(string path)
        {
            Location = path;
        }
    }
}
