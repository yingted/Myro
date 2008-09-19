using System.IO;
using System.Text;
using Microsoft.Scripting.Hosting;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class LanguageTests
    {
        [Test]
        public void IronPython_HelloWorld()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            StringBuilder outputString = new StringBuilder();
            TextWriter writer = new StringWriter(outputString);

            DLREngine engine = new DLREngine(python, writer);
            engine.ExecuteStatement("print 'Hello World'");
            Assert.AreEqual("Hello World\r\n", outputString.ToString());
        }

        [Test, Ignore("This only works if it is in Interactive mode")]
        public void IronRuby_HelloWorld_Ignored_InteractiveOnly()
        {
            LanguageSettings ruby = Helper.CreateIronRubySettings();
            StringBuilder outputString = new StringBuilder();
            TextWriter writer = new StringWriter(outputString);

            DLREngine engine = new DLREngine(ruby, writer);
            engine.ExecuteStatement("puts 'Hello World'");
            Assert.AreEqual("Hello World\n=> nil\n", outputString.ToString());
        }

        [Test]
        public void IronRuby_HelloWorld()
        {
            LanguageSettings ruby = Helper.CreateIronRubySettings();
            StringBuilder outputString = new StringBuilder();
            TextWriter writer = new StringWriter(outputString);

            DLREngine engine = new DLREngine(ruby, writer);
            engine.ExecuteStatement("puts 'Hello World'");
            Assert.AreEqual("Hello World\n", outputString.ToString());
        }

        [Test]
        public void IronPython_CLRConsole_HelloWorld()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngineFactory factory = new DLREngineFactory();
            ScriptEngine pythonEngine = factory.CreateEngine(python);
            StringBuilder outputString = new StringBuilder();
            StringWriter writer = new StringWriter(outputString);

            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            executor.ExecuteStatement("import clr\nprint clr\nimport System\nSystem.Console.WriteLine('hello world')\n");

            Assert.IsFalse(outputString.ToString().Contains("Error"), outputString.ToString());
            Assert.IsFalse(outputString.ToString().Contains("Exception"), outputString.ToString());
        }

        [Test]
        public void IronRuby_CLRConsole_HelloWorld()
        {
            LanguageSettings ruby = Helper.CreateIronRubySettings();
            DLREngineFactory factory = new DLREngineFactory();
            ScriptEngine rubyEngine = factory.CreateEngine(ruby);
            StringBuilder outputString = new StringBuilder();
            StringWriter writer = new StringWriter(outputString);

            ScriptExecutor executor = new ScriptExecutor(rubyEngine, writer);
            executor.ExecuteStatement("require 'mscorlib'\nConsole = System::Console;\nConsole.WriteLine 'hello world'");

            Assert.IsFalse(outputString.ToString().Contains("Error"), outputString.ToString());
            Assert.IsFalse(outputString.ToString().Contains("Exception"), outputString.ToString());
        }
    }
}
