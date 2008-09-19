using Microsoft.Scripting.Hosting;

namespace IronEditor.Engine
{
    public class EngineInformation
    {
        public ScriptEngine Engine { get; set; }
        public EngineInformation(ScriptEngine engine)
        {
            Engine = engine;
        }

        public string GetSaveFilter()
        {
            string filter = string.Empty;
            foreach (string ext in Engine.GetRegisteredExtensions())
            {
                filter += string.Format("{0}|*{1}", Engine.LanguageDisplayName, ext);
            }

            return filter;
        }
    }
}
