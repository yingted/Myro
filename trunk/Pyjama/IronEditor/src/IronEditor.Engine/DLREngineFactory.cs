using System;
using System.IO;
using System.Reflection;
using Microsoft.Scripting.Hosting;
using System.Linq;

namespace IronEditor.Engine
{
    internal class DLREngineFactory
    {
        internal LanguageSettings CurrentLanguageSettings { get; set; }
        public DLREngineFactory()
        {
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
        }

        Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            return GetAndLoadAssembly(args.Name);
        }

        public ScriptEngine CreateEngine(LanguageSettings settings)
        {
            CurrentLanguageSettings = settings;

            if (CurrentLanguageSettings.Language == "C#")
                throw new UnsupportedDynamicLanuage(string.Format("The language {0} is not supported by the DLR", CurrentLanguageSettings.Language));

            return CreateDLRScriptEngine();
        }

        private ScriptEngine CreateDLRScriptEngine()
        {
            ScriptRuntimeSetup setup = new ScriptRuntimeSetup();
            setup.LanguageProviders = new[]
                                          {
                                              new LanguageProviderSetup(CurrentLanguageSettings.LanguageContextObject,
                                                                        GetLanguageContextAssembly(CurrentLanguageSettings.LanguageContextObject),
                                                                        CurrentLanguageSettings.FileExtensions,
                                                                        CurrentLanguageSettings.Language)
                                          };

            ScriptRuntime runtime = ScriptRuntime.Create();
            runtime.LoadAssembly(typeof(string).Assembly);
            runtime.LoadAssembly(typeof(System.Diagnostics.Debug).Assembly);

            ScriptEngine engine = runtime.GetEngine(CurrentLanguageSettings.Language);

            return engine;
        }

        public string GetLanguageContextAssembly(string languageContextObject)
        {
            foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                Type t = assembly.GetType(languageContextObject);
                if (t != null)
                    return assembly.GetName().Name;
            }

            return string.Empty;
        }

        public void LoadAssemblies()
        {
            foreach (var assembly in CurrentLanguageSettings.AssembliesToLoad.Split(';'))
            {
                if(!AssemblyAlreadyLoaded(assembly))
                    GetAndLoadAssembly(assembly);
            }
        }

        private bool AssemblyAlreadyLoaded(string assembly)
        {
            return AppDomain.CurrentDomain.GetAssemblies().Where(a => AssembliesMatch(assembly, a.GetName().Name)).Count() != 0;
        }

        public Assembly GetAndLoadAssembly(string assemblyName)
        {
            foreach (var assembly in CurrentLanguageSettings.AssembliesToLoad.Split(';'))
            {
                if(AssembliesMatch(assemblyName, assembly))
                    return Assembly.LoadFrom(Path.Combine(CurrentLanguageSettings.BinDirectory, assembly));
            }

            return null;
        }

        public bool AssembliesMatch(string assemblyAttemptingToLoad, string assemblyWeCanLoad)
        {
            string assemblyAttemptingToLoadWithoutDll = assemblyAttemptingToLoad.Replace(".dll", "");
            string cleanAssemblyAttemptingToLoad;

            if(assemblyAttemptingToLoadWithoutDll.Contains(",")) // full Name
                cleanAssemblyAttemptingToLoad = assemblyAttemptingToLoadWithoutDll.Substring(0, assemblyAttemptingToLoadWithoutDll.IndexOf(','));
            else
                cleanAssemblyAttemptingToLoad = assemblyAttemptingToLoadWithoutDll;

            string cleanAssemblyWeCanLoad = assemblyWeCanLoad.Replace(".dll", "");

            if (cleanAssemblyAttemptingToLoad ==cleanAssemblyWeCanLoad)
                 return true;
            
            return false;
        }
    }
}
