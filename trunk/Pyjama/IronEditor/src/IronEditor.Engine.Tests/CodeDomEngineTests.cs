using System.CodeDom.Compiler;
using System.IO;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class CodeDomEngineTests
    {
        StringBuilder outputString;
        TextWriter writer;
        LanguageSettings csharp;
        CodeDomEngine engine;

        private string validCode = "public class MyClass" +
                                   "{" +
                                       "public static void Main(string[] args)" +
                                       "{" +
                                            "System.Console.WriteLine(\"Hello World\");" +
                                       "}" +
                                   "}";

        [SetUp]
        public void Setup()
        {
            outputString = new StringBuilder();
            writer = new StringWriter(outputString);
            csharp = Helper.CreateCSharpSettings();
            engine = new CodeDomEngine(csharp, writer);
        }

        [Test]
        public void ExecuteStatement_ConsoleWriteHelloWorld_OutputToString()
        {
            engine.ExecuteStatement(validCode);
            Assert.AreEqual("Hello World\r\n", outputString.ToString());
        }

        [Test]
        public void ExecuteStatement_BadCode_OutputsError()
        {
            engine.ExecuteStatement("public class MyClass" +
                                    "{" +
                                       "public static void Main(string[] args)" +
                                       "{" +
                                            "System.WriteLine(\"Hello World\");" +
                                       "}" +
                                    "}");
            Assert.IsTrue(outputString.ToString().Contains("Error"));
        }

        [Test]
        public void GetClassName_RegEx_Example()
        {
            string pattern = "public class (.*?){";
            Regex regex = new Regex(pattern);

            MatchCollection collection = regex.Matches(validCode);

            foreach (Match match in collection)
            {
                Assert.IsTrue(match.Success);
                Assert.AreEqual("MyClass", match.Groups[1].Value);
            }
        }

        [Test]
        public void GetClassName_ValidCode_ReturnMyClass()
        {
            string className = engine.GetClassName(validCode);
            Assert.AreEqual("MyClass", className);
        }

        [Test]
        public void Compile_ValidCode_BuiltAssembly()
        {
            CodeDomProvider provider = CodeDomProvider.CreateProvider(CodeDomProvider.GetLanguageFromExtension(".cs"));
            Assembly compiledAssembly = engine.Compile(provider, validCode);
            Assert.IsNotNull(compiledAssembly);
            Assert.IsFalse(outputString.ToString().Contains("Error"));
        }

        [Test]
        public void Compile_InvalidCode_ErrorOutputted()
        {
            CodeDomProvider provider = CodeDomProvider.CreateProvider(CodeDomProvider.GetLanguageFromExtension(".cs"));
            Assembly compiledAssembly = engine.Compile(provider, "asdasdasd");
            Assert.IsNull(compiledAssembly);
            Assert.IsTrue(outputString.ToString().Contains("Error"));
        }

        [Test]
        public void ExecuteMainMethod_ValidCode_Outputted()
        {
            CodeDomProvider provider = CodeDomProvider.CreateProvider(CodeDomProvider.GetLanguageFromExtension(".cs"));
            Assembly compiledAssembly = engine.Compile(provider, validCode);
            engine.ExecuteMainMethod(compiledAssembly, "MyClass");
        }
    }
}
