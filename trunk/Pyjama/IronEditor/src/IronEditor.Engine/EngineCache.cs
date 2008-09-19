using System.Collections.Generic;
using System.IO;

namespace IronEditor.Engine
{
    public class EngineCache
    {
        private Dictionary<LanguageSettings, IEngine> Engines;
        List<string> paths = new List<string>();
        public int CachedEngines
        {
            get
            {
                return Engines.Count;
            }
        }

        public EngineCache()
        {
            Engines = new Dictionary<LanguageSettings, IEngine>();
        }

        public IEngine GetEngine(LanguageSettings language, TextWriter outputStream)
        {
            if (language != null && Engines.ContainsKey(language))
                return Engines[language];

            IEngine engine = CreateEngine(language, outputStream);

            Engines.Add(language, engine);
            return engine;
        }

        private IEngine CreateEngine(LanguageSettings language, TextWriter outputStream)
        {
            IEngine engine;
            if (language.Language == "C#")
                engine = new CodeDomEngine(language, outputStream);
            else
                engine = new DLREngine(language, outputStream);

            SetPaths(engine);

            return engine;
        }

        private void SetPaths(IEngine engine)
        {
            foreach (string path in paths)
                engine.AddPath(path);
        }

        public void AppendPathToEngines(string path)
        {
            paths.Add(path);

            foreach (KeyValuePair<LanguageSettings, IEngine> pair in Engines)
                SetPaths(pair.Value);
        }
    }
}
