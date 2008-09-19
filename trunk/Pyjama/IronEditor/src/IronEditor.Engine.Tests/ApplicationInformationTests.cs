using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class ApplicationInformationTests
    {
        [Test]
        public void Title_IronEditor_ReturnedAsString()
        {
            string title = ApplicationInformation.Title();
        }

        [Test]
        public void Version_CurrentVersion_ReturnedAsString()
        {
            string version = ApplicationInformation.Version();
        }
    }
}
