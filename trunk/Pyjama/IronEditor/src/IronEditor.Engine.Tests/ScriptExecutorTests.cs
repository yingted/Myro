using System;
using System.IO;
using System.Text;
using Microsoft.Scripting.Hosting;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class ScriptExecutorTests
    {
        private ScriptEngine pythonEngine;
        private StringWriter writer;
        private StringBuilder outputString;

        [SetUp]
        public void Setup()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngineFactory factory = new DLREngineFactory();
            pythonEngine = factory.CreateEngine(python);

            outputString = new StringBuilder();
            writer = new StringWriter(outputString);
        }

        [Test]
        public void Ctor_OutAndError_RedirectToWriter()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);

            Assert.AreEqual(writer, pythonEngine.Runtime.IO.OutputWriter);
            Assert.AreEqual(writer, pythonEngine.Runtime.IO.ErrorWriter);
        }

        [Test]
        public void Ctor_ConsoleWriteLine_RedirectToWriter()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            Console.WriteLine("Test");
            Assert.AreEqual("Test\r\n", outputString.ToString());
        }

        [Test]
        public void Ctor_ConsoleWrite_RedirectToWriter()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            Console.Write("Test");
            Assert.AreEqual("Test", outputString.ToString());
        }


        [Test]
        public void Ctor_ConsoleError_RedirectToWriter()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            Console.Error.Write("Test");
            Assert.AreEqual("Test", outputString.ToString());
        }


        [Test]
        public void ExecuteStatement_PrintHelloWorld_HelloWorldWroteToStream()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            executor.ExecuteStatement("print \"Hello World\"");

            Assert.AreEqual("Hello World\r\n", outputString.ToString());
        }

        [Test]
        public void ExecuteStatement_ImportCLRPrintHelloWorld_NoException()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            executor.ExecuteStatement("import clr\nprint clr\nimport System\nSystem.Console.WriteLine('hello world')\n");

            Assert.IsFalse(outputString.ToString().Contains("Error"), outputString.ToString());
            Assert.IsFalse(outputString.ToString().Contains("Exception"), outputString.ToString());
        }

        [Test]
        public void ExecuteStatement_ImportCLR_NoException()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            executor.ExecuteStatement("import clr");

            Assert.IsFalse(outputString.ToString().Contains("Error"), outputString.ToString());
            Assert.IsFalse(outputString.ToString().Contains("Exception"), outputString.ToString());
        }

        [Test]
        public void ExecuteFile_HelloWorldPy_HelloWorldWroteToStream()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            executor.ExecuteFile("TestCode\\HelloWorld.py");

            Assert.AreEqual("Hello World\r\n", outputString.ToString());
        }


        [Test]
        public void WriteSyntaxException_InValidCode_MessageWroteToStream()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            executor.ExecuteStatement("print Hello \r\n World");
            Assert.IsTrue(writer.ToString().Contains("Error: "));
        }

        [Test]
        public void WriteSyntaxException_InValidCode_MessageWroteToStream2()
        {
            ScriptExecutor executor = new ScriptExecutor(pythonEngine, writer);
            executor.ExecuteStatement("c");
            Assert.IsTrue(writer.ToString().Contains("Error: "));
        }
    }
}
