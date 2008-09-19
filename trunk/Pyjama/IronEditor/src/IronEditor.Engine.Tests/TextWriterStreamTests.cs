using System.IO;
using System.Text;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class TextWriterStreamTests
    {
        [Test]
        public void Write_WriteToStream_OutputRedirectToTextWriter_ThenOutToStringBuilder()
        {
            StringBuilder outputString = new StringBuilder();
            TextWriter writer = new StringWriter(outputString);

            Stream textWriter = new TextWriterStream(writer);
            byte[] hello = Encoding.Default.GetBytes("Hello World");
            textWriter.Write(hello, 0, hello.Length);

            Assert.AreEqual("Hello World", outputString.ToString());
        }
    }
}
