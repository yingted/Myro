using System.Drawing;
using System.IO.IsolatedStorage;
using IronEditor.UI.WinForms;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class ApplicationOptionsTests
    {
        private UserSettings CreateSettings()
        {
            UserSettings settings = new UserSettings();
            settings.FontName = FontFamily.GenericSansSerif.Name;
            settings.FontSize = 10;
            return settings;
        }

        [Test]
        public void GetIsolatedStorage_HasDomainID_AssemblyID()
        {
            IsolatedStorageFile isoFile = ApplicationOptions.GetIsolatedStorage();
            Assert.IsNotNull(isoFile.DomainIdentity);
            Assert.IsNotNull(isoFile.AssemblyIdentity);
        }

        [Test]
        public void SaveUserSettings_UIFont_WritesToIsolatedStorage()
        {
            IsolatedStorageFile isoFile = ApplicationOptions.GetIsolatedStorage();
            UserSettings settings = CreateSettings();
            ApplicationOptions.SaveUserSettings(isoFile, settings);

            Assert.IsTrue(isoFile.GetFileNames("UserSettings.xml").Length > 0);
        }

        [Test]
        public void LoadSettings_WritesToIsolatedStorage_UIFont()
        {
            IsolatedStorageFile isoFile = ApplicationOptions.GetIsolatedStorage();
            UserSettings savedSettings = CreateSettings();
            ApplicationOptions.SaveUserSettings(isoFile, savedSettings);

            UserSettings settings = ApplicationOptions.LoadUserSettings(isoFile);
            Assert.IsNotNull(settings);
            Assert.AreEqual(FontFamily.GenericSansSerif.Name, settings.FontName);
            Assert.AreEqual(10, settings.FontSize);
        }
    }
}
