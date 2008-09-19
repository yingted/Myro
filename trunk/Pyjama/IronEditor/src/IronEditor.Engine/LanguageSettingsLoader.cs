using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace IronEditor.Engine
{
    public class LanguageSettingsLoader
    {
        public List<LanguageSettings> LoadSettings(string directory)
        {
            DirectoryInfo d = new DirectoryInfo(directory);
            FileInfo[] files = d.GetFiles("*.xml");

            List<LanguageSettings> settings = new List<LanguageSettings>(files.Count());

            foreach (FileInfo file in files)
            {
                LanguageSettingsSerialisation serialiser = new LanguageSettingsSerialisation();
                settings.Add(serialiser.Deserialise(file.FullName));
            }

            return settings;
        }
    }
}
