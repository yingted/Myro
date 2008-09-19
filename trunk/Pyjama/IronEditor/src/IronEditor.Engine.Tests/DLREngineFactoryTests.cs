using System;
using System.Reflection;
using Microsoft.Scripting.Hosting;
using NUnit.Framework;

namespace IronEditor.Engine.Tests
{
    [TestFixture]
    public class DLREngineFactoryTests
    {
        [Test]
        public void CreateEngine_IronPythonLanguageSettingsObject_ScriptRuntimeReturned()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();

            DLREngineFactory factory = new DLREngineFactory();
            ScriptEngine engine = factory.CreateEngine(python);
            Assert.AreEqual("IronPython 2.0 Beta", engine.LanguageDisplayName);
        }

        [Test]
        [ExpectedException(typeof(UnsupportedDynamicLanuage))]
        public void CreateEngine_CSharpLanguageSettingsObject_ExceptionThrown()
        {
            LanguageSettings csharp = Helper.CreateCSharpSettings();

            DLREngineFactory factory = new DLREngineFactory();
            factory.CreateEngine(csharp);
            Assert.Fail("Exception should have been thrown");
        }

        [Test]
        public void LoadAssemblies_IronPythonSettings_AssembliesLoadedIntoCurrentAppDomain()
        {
            int loaded = 0;

            LanguageSettings python = Helper.CreateIronPythonSettings();

            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            factory.LoadAssemblies();

            foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                if (assembly.FullName.Contains("IronPython"))
                    loaded++;
            }


            Assert.AreEqual(2, loaded, "Assembly Load event not fired");
        }

        [Test]
        public void GetLanguageContextAssembly_IronPythonLanguageContextString_LangContextType()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();

            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            factory.LoadAssemblies();
            string assemblyName = factory.GetLanguageContextAssembly(python.LanguageContextObject);
            Assert.AreEqual("IronPython", assemblyName);
        }

        [Test]
        public void GetLanguageContextAssembly_IronRubyLanguageContextString_LangContextType()
        {
            LanguageSettings ruby = Helper.CreateIronRubySettings();

            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = ruby;
            factory.LoadAssemblies();
            string assemblyName = factory.GetLanguageContextAssembly(ruby.LanguageContextObject);
            Assert.AreEqual("IronRuby", assemblyName);
        }


        [Test]
        public void GetAndLoadAssembly_ShortIronPython_ReturnsAssembly()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();

            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            Assembly assembly = factory.GetAndLoadAssembly("IronPython");
            Assert.IsTrue(assembly.FullName.StartsWith("IronPython, "));
        }

        [Test]
        public void GetAndLoadAssembly_ShortIronPythonModules_ReturnsAssembly()
        {
            LanguageSettings python = Helper.CreateIronPythonSettings();

            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            Assembly assembly = factory.GetAndLoadAssembly("IronPython.Modules");
            Assert.IsTrue(assembly.FullName.StartsWith("IronPython.Modules"));
        }

        [Test]
        public void GetAndLoadAssembly_ShortIronRuby_ReturnsAssembly()
        {
            LanguageSettings python = Helper.CreateIronRubySettings();

            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            Assembly assembly = factory.GetAndLoadAssembly("IronRuby");
            Assert.IsTrue(assembly.FullName.StartsWith("IronRuby, "));
        }

        [Test]
        public void GetAndLoadAssembly_FullNameIronPython_ReturnsAssembly()
        {
            string fullName = "IronPython, Version=2.0.0.2000, Culture=neutral, PublicKeyToken=31bf3856ad364e35";
            string actualName = "IronPython.dll";

            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            bool result = factory.AssembliesMatch(fullName, actualName);
            Assert.IsTrue(result);
        }

        [Test]
        public void AssembliesMatch_FullNameIronPythonModules_ReturnsAssembly()
        {
            string fullName = "IronPython.Modules, Version=2.0.0.2000, Culture=neutral, PublicKeyToken=31bf3856ad364e35";
            string actualName = "IronPython.Modules.dll";

            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            bool result = factory.AssembliesMatch(fullName, actualName);
            Assert.IsTrue(result);
        }


        [Test]
        public void AssembliesMatch_ShortIronPython_ReturnsAssembly()
        {
            string fullName = "IronPython";
            string actualName = "IronPython.dll";

            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            bool result = factory.AssembliesMatch(fullName, actualName);
            Assert.IsTrue(result);
        }

        [Test]
        public void AssembliesMatch_ShortIronPythonModules_ReturnsAssembly()
        {
            string fullName = "IronPython.Modules";
            string actualName = "IronPython.Modules.dll";

            LanguageSettings python = Helper.CreateIronPythonSettings();
            DLREngineFactory factory = new DLREngineFactory();
            factory.CurrentLanguageSettings = python;
            bool result = factory.AssembliesMatch(fullName, actualName);
            Assert.IsTrue(result);
        }
    }
}
