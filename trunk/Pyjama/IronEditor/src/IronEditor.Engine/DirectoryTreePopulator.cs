using System.IO;

namespace IronEditor.Engine
{
    public class DirectoryTreePopulator
    {
        public DirectoryTreePopulator()
        {
        }

        public DirectoryTree GetTree(string directory)
        {
            DirectoryTree tree = new DirectoryTree();
            tree.Name = GetName(directory);
            tree.Location = directory;
            tree.PopulateTree();

            return tree;
        }

        public static string GetName(string path)
        {
            return Path.GetFileName(path);
        }
    }
}
