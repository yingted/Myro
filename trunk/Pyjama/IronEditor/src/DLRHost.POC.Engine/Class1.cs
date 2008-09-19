using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using Microsoft.Scripting.Hosting;

namespace DLRHost.POC.Engine
{
    public class EngineFactory
    {
        public void LoadAssemblies()
        {
            foreach (var assembly in "IronPython.dll;IronPython.Modules.dll".Split(';'))
            {
                GetAndLoadAssembly(assembly);
            }
        }

        public Assembly GetAndLoadAssembly(string assemblyName)
        {
            foreach (var assembly in "IronPython.dll;IronPython.Modules.dll".Split(';'))
            {
                if (AssembliesMatch(assemblyName, assembly))
                    return Assembly.LoadFrom(Path.Combine(Directory.GetCurrentDirectory() + "\\LanguageBinaries\\IronPython", assembly));
            }

            return null;
        }

        public bool AssembliesMatch(string assemblyAttemptingToLoad, string assemblyWeCanLoad)
        {
            string assemblyAttemptingToLoadWithoutDll = assemblyAttemptingToLoad.Replace(".dll", "");
            string cleanAssemblyAttemptingToLoad;

            if (assemblyAttemptingToLoadWithoutDll.Contains(",")) // full Name
                cleanAssemblyAttemptingToLoad = assemblyAttemptingToLoadWithoutDll.Substring(0, assemblyAttemptingToLoadWithoutDll.IndexOf(','));
            else
                cleanAssemblyAttemptingToLoad = assemblyAttemptingToLoadWithoutDll;

            string cleanAssemblyWeCanLoad = assemblyWeCanLoad.Replace(".dll", "");

            if (cleanAssemblyAttemptingToLoad == cleanAssemblyWeCanLoad)
                return true;
            else
                return false;
        }

        Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            return GetAndLoadAssembly(args.Name);
        }

        //public Assembly LoadAssemblies(string assembly)
        //{
        //    string binaryPath = Path.Combine(Directory.GetCurrentDirectory(), "LanguageBinaries\\IronPython");

        //    if (assembly.StartsWith("IronPython.Modules"))
        //        return Assembly.LoadFrom(Path.Combine(binaryPath, "IronPython.Modules.dll"));
        //    else
        //        return Assembly.LoadFrom(Path.Combine(binaryPath, "IronPython.dll"));
        //}



        public Type GetLanguageContextType()
        {
            foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                Type t = assembly.GetType("IronPython.Runtime.PythonContext");
                if (t != null)
                    return t;
            }

            return null;
        }

        public ScriptEngine CreateEngine()
        {
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;

            LoadAssemblies();

            ScriptRuntime runtime = ScriptRuntime.Create();
            ScriptEngine engine = runtime.GetEngine(GetLanguageContextType());


            runtime.LoadAssembly(typeof(string).Assembly);
            runtime.LoadAssembly(typeof(System.Diagnostics.Debug).Assembly);

            Assembly assem = Assembly.GetAssembly(Type.GetType("System.Text.StringBuilder"));
            engine.Runtime.LoadAssembly(assem);

            return engine;
        }
    }

    public class Executor
    {
        public void ExecuteCode(ScriptEngine engine)
        {
            ScriptScope source = engine.CreateScope();
            source.Execute("import clr\nprint clr\nimport System\nSystem.Console.WriteLine('hello world')\n");
        }
    }
}
