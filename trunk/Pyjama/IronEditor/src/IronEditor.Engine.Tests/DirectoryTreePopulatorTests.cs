using System.IO;
using System.Reflection;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class DirectoryTreePopulatorTests
    {
        [Test]
        public void GetRoot_RootDirectory_RootNameCorrect()
        {
            string rootDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            DirectoryTreePopulator root = new DirectoryTreePopulator();
            DirectoryTree tree = root.GetTree(rootDirectory);
            Assert.AreEqual(Path.GetFileName(rootDirectory), tree.Name);
        }
    }
}
