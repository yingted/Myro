using System.IO;
using System.Xml.Serialization;

namespace IronEditor.Engine
{
    public class LanguageSettingsSerialisation
    {
        public LanguageSettingsSerialisation()
        {
        }

        public void Serialise(string outputPath, LanguageSettings settings)
        {
            CreateDirectoryIfNotExists(outputPath);

            TextWriter writer = new StreamWriter(outputPath, false);
            XmlSerializer serialiser = new XmlSerializer(typeof(LanguageSettings));
            serialiser.Serialize(writer, settings);
            writer.Close();
        }

        public void CreateDirectoryIfNotExists(string outputPath)
        {
            if (!Directory.Exists(Path.GetDirectoryName(outputPath)))
                Directory.CreateDirectory(Path.GetDirectoryName(outputPath));
        }

        public LanguageSettings Deserialise(string configPath)
        {
            TextReader reader = new StreamReader(configPath);
            XmlSerializer serialiser = new XmlSerializer(typeof(LanguageSettings));
            LanguageSettings settings = serialiser.Deserialize(reader) as LanguageSettings;
            reader.Close();
            return settings;
        }
    }
}
