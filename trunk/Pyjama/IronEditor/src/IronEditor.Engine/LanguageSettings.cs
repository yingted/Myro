using System.Diagnostics;
using System.IO;
using System.Reflection;

namespace IronEditor.Engine
{
    [DebuggerDisplay("Language ( {Language} )")]
    public class LanguageSettings
    {
        string _binDirectory = string.Empty;
        public string BinDirectory
        {
            get
            {
                return _binDirectory;
            }
            set
            {
                if (value.StartsWith("\\"))
                    value = value.Remove(0, 1);

                _binDirectory = Path.Combine(GetExecutingDirectory(), value);
            }
        }

        public string Language { get; set; }

        public string AssembliesToLoad { get; set; }

        public string LanguageContextObject { get; set; }

        public string CommandLineApplication { get; set; }

        public string FileExtensions { get; set; }

        public LanguageSettings(string languageName)
        {
            Language = languageName;
        }

        public LanguageSettings()
        {
            
        }

        public static string GetExecutingDirectory()
        {
            return Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
        }
    }
}
