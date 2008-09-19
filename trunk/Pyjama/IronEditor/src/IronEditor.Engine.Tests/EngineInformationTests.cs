using Microsoft.Scripting.Hosting;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class EngineInformationTests
    {
        [Test]
        public void GetSaveFilter_RegisteredTypes_String()
        {
            string expected = "IronPython 2.0 Beta|*.py";
            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngineFactory factory = new DLREngineFactory();
            ScriptEngine engine = factory.CreateEngine(python);

            EngineInformation info = new EngineInformation(engine);
            string actual = info.GetSaveFilter();
            Assert.AreEqual(expected, actual);
        }
    }
}
