using System.Diagnostics;
using System.IO;
using System.Text;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class DLREngineTests
    {
        [Test]
        public void GetSaveFilter()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();

            DLREngine engine = new DLREngine(python, null);
            string filter = engine.GetSaveFilter();
            Assert.AreEqual("IronPython 2.0 Beta|*.py", filter);
        }

        [Test]
        public void ExecuteStatement()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            StringBuilder outputString = new StringBuilder();
            TextWriter writer = new StringWriter(outputString);

            DLREngine engine = new DLREngine(python, writer);
            engine.ExecuteStatement("print 'Hello World'");
            Assert.AreEqual("Hello World\r\n", outputString.ToString());
        }

        [Test]
        public void CanExecuteConsole_IfLanguageSettingHasValue_WhichExists_ReturnTrue()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngine engine = new DLREngine(python, null);
            Assert.IsTrue(engine.CanExecuteConsole);
        }

        [Test]
        public void CanExecuteConsole_IfLanguageSettinDoesNotHaveValue_ReturnFalse()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            python.CommandLineApplication = string.Empty;
            DLREngine engine = new DLREngine(python, null);
            Assert.IsFalse(engine.CanExecuteConsole);
        }

        [Test]
        public void CanExecuteConsole_IfPathIsValid_ReturnTrue()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngine engine = new DLREngine(python, null);
            Assert.IsTrue(engine.CanExecuteConsole);
        }

        [Test]
        public void CanExecuteConsole_IfPathIsInvalid_ReturnFalse()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            python.CommandLineApplication = "ip.exe";
            DLREngine engine = new DLREngine(python, null);
            Assert.IsFalse(engine.CanExecuteConsole);
        }

        [Test]
        public void LaunchConsole_CanExecuteConsole_StartsProcess()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngine engine = new DLREngine(python, null);
            engine.LaunchConsole();

            Process[] p = Process.GetProcessesByName("ipy");
            Assert.IsTrue(p.Length > 0);
            foreach (var process in p)
            {
                process.Kill();
                process.WaitForExit();
            }
        }

        [Test]
        public void LaunchConsole_CannotExecuteConsole_DoesNotStartProcess()
        {
            Process[] ipy = Process.GetProcessesByName("ipy");
            if(ipy.Length > 0)
                Assert.Ignore("ipy.exe already running");

            LanguageSettings python = Helper.CreateIronPythonSettings();
            python.CommandLineApplication = "ip.exe";
            DLREngine engine = new DLREngine(python, null);
            engine.LaunchConsole();

            Process[] p = Process.GetProcessesByName("ipy");
            Assert.IsTrue(p.Length == 0);
        }
    }
}
