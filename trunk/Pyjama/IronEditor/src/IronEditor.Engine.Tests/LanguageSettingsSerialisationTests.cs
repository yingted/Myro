using System.IO;
using System.Reflection;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class LanguageSettingsSerialisationTests
    {
        [Test]
        public void SerialiseObject_ValidIronPythonObjectSettings_XmlFileCreated()
        {
            LanguageSettings ironPython = Helper.CreateIronPythonSettings();

            LanguageSettingsSerialisation serialise = new LanguageSettingsSerialisation();
            serialise.Serialise("TestConfig\\SerialiseIronPython.xml", ironPython);
            Assert.IsTrue(File.Exists("TestConfig\\SerialiseIronPython.xml"));
            File.Delete("TestConfig\\SerialiseIronPython.xml");
        }

        [Test]
        public void CreateDirectoryIfNotExists_IfDirectoryDoesNotExist_CreateDirectory()
        {
            LanguageSettingsSerialisation serialise = new LanguageSettingsSerialisation();

            string directory = @"SerialiseObjectTest";
            if(Directory.Exists(directory))
                Directory.Delete(directory);

            serialise.CreateDirectoryIfNotExists(directory + @"\test.xml");

            Assert.IsTrue(Directory.Exists(directory), "Directory does not exist: {0}", directory);
        }

        [Test]
        public void CreateDirectoryIfNotExists_IfFullDirectoryPathDoesNotExist_CreateDirectory()
        {
            LanguageSettingsSerialisation serialise = new LanguageSettingsSerialisation();

            string directory = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"SerialiseObjectTest");
            if (Directory.Exists(directory))
                Directory.Delete(directory);

            serialise.CreateDirectoryIfNotExists(directory + @"\test.xml");

            Assert.IsTrue(Directory.Exists(directory), "Directory does not exist: {0}", directory);
        }

        [Test]
        public void DeserialiseObject_KnownIronPythonTestConfigFile_CreatesObject()
        {
            LanguageSettingsSerialisation serialise = new LanguageSettingsSerialisation();
            LanguageSettings ironPython = serialise.Deserialise("TestConfig\\IronPython.xml");

            Assert.IsNotNull(ironPython, "Object null");

            Assert.AreEqual(ironPython.Language, "IronPython", "Langauge not correct");
            Assert.AreEqual(ironPython.AssembliesToLoad, "IronPython.dll;IronPython.Modules.dll", "AssembliesToLoad not correct");
            Assert.AreEqual(ironPython.LanguageContextObject, "IronPython.Runtime.PythonContext", "LanguageContextObject not correct");
        }

        
    }
}
