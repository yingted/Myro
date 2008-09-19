using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class DirectoryTreeTests
    {
        [Test]
        public void GetRoot_RootDirectory_HasAllFiles()
        {
            string rootDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            DirectoryTree tree = new DirectoryTree();
            tree.Name = DirectoryTreePopulator.GetName(rootDirectory);
            tree.Location = rootDirectory;
            tree.PopulateTree();

            Assert.IsTrue(tree.Files.Count > 0);
        }

        [Test]
        public void GetRoot_RootDirectory_HasDirectories()
        {
            string rootDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            DirectoryTree tree = new DirectoryTree();
            tree.Name = DirectoryTreePopulator.GetName(rootDirectory);
            tree.Location = rootDirectory;
            tree.PopulateTree();

            Assert.IsTrue(tree.Directories.Count > 0);
        }

        [Test]
        public void GetRoot_RootDirectory_EachNodeHasFiles()
        {
            string rootDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            DirectoryTree tree = new DirectoryTree();
            tree.Name = DirectoryTreePopulator.GetName(rootDirectory);
            tree.Location = rootDirectory;
            tree.PopulateTree();

            List<DirectoryTree> badTrees = new List<DirectoryTree>();

            foreach (DirectoryTree file in tree.Directories)
            {
                if (file.Files.Count > 0 || file.Directories.Count > 0)
                    continue;

                badTrees.Add(file);
            }

            foreach (DirectoryTree badTree in badTrees)
            {
                Console.WriteLine(badTree.Location);
            }
        }
    }
}