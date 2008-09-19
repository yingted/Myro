using System.Collections.Generic;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class LanguageSettingsLoaderTests
    {
        [Test]
        public void LoadSettingsFromDirectory_AllFilesInDirectory_ListWithIronPythonObject()
        {
            LanguageSettingsLoader loader = new LanguageSettingsLoader();
            List<LanguageSettings> settingsList = loader.LoadSettings("TestConfig");
            Assert.AreEqual(1, settingsList.Count);
        }
    }
}
