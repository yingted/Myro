using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class LanguageSettingsTests
    {
        [Test]
        public void GetExecutingDirectory_CurrentPath_StringReturned()
        {
            string directory = LanguageSettings.GetExecutingDirectory();
            Assert.IsNotNull(directory);
            Assert.IsFalse(directory.Contains("*.dll"));
        }
    }
}
