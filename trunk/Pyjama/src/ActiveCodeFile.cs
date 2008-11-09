
namespace Pyjama
{
    public class ActiveCodeFile
    {
        public string FileExtension;
        public string Location;
        public string FileName;
        public bool Unsaved;
        public bool Untitled;

        public ActiveCodeFile()
        {
        }

        public void SetFileName(string filename) {
            FileName = System.IO.Path.GetFileName(filename);
            if (FileName == "__init__.py")
            {
                string[] path = filename.Split(new char[] { System.IO.Path.DirectorySeparatorChar });
                FileName = System.IO.Path.Combine(path[path.Length - 2], path[path.Length - 1]);
            }
        }

        public ActiveCodeFile(string filename)
        {
            Location = filename;
            SetFileName(filename);
        }

    }
}
