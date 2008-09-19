using System;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class ActiveCodeFileTests
    {
        [Test]
        public void FileName_GivenFullLocation_ReturnsFileName()
        {
            ActiveCodeFile file = new ActiveCodeFile();
            file.Location = @"Z:\Test\Tester\MyFile.cs";
            Assert.AreEqual("MyFile.cs", file.FileName);
        }

        [Test]
        public void FileName_LocationNotSet_ReturnsStringEmpty()
        {
            ActiveCodeFile file = new ActiveCodeFile();
            Assert.IsTrue(file.FileName.Equals(String.Empty));
        }
    }
}
