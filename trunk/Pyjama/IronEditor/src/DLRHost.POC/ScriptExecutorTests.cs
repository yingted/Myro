using System;
using System.IO;
using System.Text;
using DLRHost.Engine;
using MbUnit.Framework;
using Microsoft.Scripting.Hosting;

namespace DLRHost.POC
{
    [TestFixture]
    public class ScriptExecutorTests
    {
      
        [Test]
        public void ExecuteStatement_ImportCLRPrintHelloWorld_NoException()
        {
            LanguageSettingsSerialisation serialise = new LanguageSettingsSerialisation();
            LanguageSettings ironPython = serialise.Deserialise("TestConfig\\IronPython.xml");
            EngineFactory factory = new EngineFactory(ironPython);
            ScriptEngine engine = factory.CreateEngine();


            StringBuilder outputString = new StringBuilder();
            TextWriter writer = new StringWriter(outputString);
            ScriptExecutor exec = new ScriptExecutor(engine, writer);
            exec.ExecuteStatement("import clr");


            Assert.IsFalse(outputString.ToString().Contains("Error"), outputString.ToString());
            Assert.IsFalse(outputString.ToString().Contains("Exception"), outputString.ToString());
        }

    }
}