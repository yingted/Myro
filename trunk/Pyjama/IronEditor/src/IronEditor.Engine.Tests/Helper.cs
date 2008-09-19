namespace IronEditor.Engine.Tests
{
    internal class Helper
    {

        internal static LanguageSettings CreateIronPythonSettings()
        {
            LanguageSettings ironPython = new LanguageSettings("IronPython");
            ironPython.BinDirectory = "TestAssemblies";
            ironPython.AssembliesToLoad = "IronPython.dll;IronPython.Modules.dll";
            ironPython.LanguageContextObject = "IronPython.Runtime.PythonContext";
            ironPython.CommandLineApplication = "ipy.exe";
            ironPython.FileExtensions = ".py";
            return ironPython;
        }

        public static LanguageSettings CreateCSharpSettings()
        {
            LanguageSettings cSharpSettings = new LanguageSettings("C#");
            cSharpSettings.FileExtensions = ".cs";
            return cSharpSettings;
        }

        public static LanguageSettings CreateIronRubySettings()
        {
            LanguageSettings ironRuby = new LanguageSettings("IronRuby");
            ironRuby.BinDirectory = "TestAssemblies";
            ironRuby.AssembliesToLoad = "IronRuby.dll;IronRuby.Libraries.dll";
            ironRuby.LanguageContextObject = "Ruby.Runtime.RubyContext";
            ironRuby.CommandLineApplication = "ir.exe";
            ironRuby.FileExtensions = ".rb";
            return ironRuby;
        }
    }
}
