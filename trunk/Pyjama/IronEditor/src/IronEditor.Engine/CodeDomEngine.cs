using System;
using System.CodeDom.Compiler;
using System.IO;
using System.Reflection;
using System.Text.RegularExpressions;

namespace IronEditor.Engine
{
    public class CodeDomEngine : IEngine
    {
        private LanguageSettings Language;
        TextWriter Output;

        public CodeDomEngine(LanguageSettings language, TextWriter writer)
        {
            Language = language;
            Output = writer;
            Console.SetOut(writer);
            Console.SetError(writer);
        }

        public void ExecuteStatement(string code)
        {
            CodeDomProvider provider = CodeDomProvider.CreateProvider(CodeDomProvider.GetLanguageFromExtension(".cs"));
            Assembly compiledAssembly = Compile(provider, code);
            
            if (compiledAssembly == null)
                return;

            ExecuteMainMethod(compiledAssembly, GetClassName(code));
        }

        internal string GetClassName(string code)
        {
            string pattern = "public class (.*?){";
            Regex regex = new Regex(pattern, RegexOptions.Singleline | RegexOptions.IgnoreCase);

            MatchCollection collection = regex.Matches(code);

            foreach (Match match in collection)
            {
                return match.Groups[1].Value.Replace(Environment.NewLine, "");
            }
            return string.Empty;
        }

        internal Assembly Compile(CodeDomProvider provider, string code)
        {
            CompilerParameters parameters = new CompilerParameters();
            parameters.ReferencedAssemblies.Add("System.dll");
            parameters.ReferencedAssemblies.Add("System.Windows.Forms.dll");
            parameters.GenerateInMemory = false;

            CompilerResults result = provider.CompileAssemblyFromSource(parameters, code);

            if(result.Errors.Count > 0)
            {
                Output.WriteLine("Compiler Errors");
                foreach (CompilerError error in result.Errors)
                {
                    Output.WriteLine(error.Line + "\t" + error.ErrorText);
                }

                return null;
            }

            return result.CompiledAssembly;
        }

        internal void ExecuteMainMethod(Assembly compiledAssembly, string className)
        {
            object instance = compiledAssembly.CreateInstance(className);
            MethodInfo info = instance.GetType().GetMethod("Main");
            info.Invoke(null, new object[] {null});
        }

        public string GetSaveFilter()
        {
            return "C# Source Code (*.cs)|*.cs";
        }

        public string LanguageName
        {
            get { return Language.Language; }
        }

        public bool CanExecuteConsole
        {
            get { return false; }
        }

        public void AddPath(string path)
        {
            throw new System.NotImplementedException();
        }

        public string[] ScriptSourceSearch()
        {
            throw new System.NotImplementedException();
        }

        public void LaunchConsole()
        {
            throw new System.InvalidOperationException("Console application is not available for this engine.");
        }
    }
}
