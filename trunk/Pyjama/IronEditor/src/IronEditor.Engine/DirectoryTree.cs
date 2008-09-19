using System.Collections.Generic;
using System.IO;

namespace IronEditor.Engine
{
    public class DirectoryTree
    {
        public string Name { get; set; }
        public string Location { get; set; }

        public List<FileLeaf> Files { get; set; }

        public List<DirectoryTree> Directories { get; set; }

        public void PopulateTree()
        {
            PopulateFiles();
            PopulateDirectories();
        }

        private void PopulateDirectories()
        {
            Directories = new List<DirectoryTree>();
            foreach (string f in Directory.GetDirectories(Location))
            {
                DirectoryTree t = new DirectoryTree();
                t.Name = DirectoryTreePopulator.GetName(f);
                t.Location = f;
                t.PopulateTree();
                Directories.Add(t);
            }
        }

        private void PopulateFiles()
        {
            Files = new List<FileLeaf>();
            foreach (string f in Directory.GetFiles(Location))
            {
                FileLeaf l = new FileLeaf();
                l.Name = DirectoryTreePopulator.GetName(f);
                l.Location = f;
                Files.Add(l);
            }
        }
    }
}