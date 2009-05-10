using System;
using System.Drawing;
using System.IO;
using System.IO.IsolatedStorage;
using System.Reflection;
using System.Security.Policy;
using System.Xml.Serialization;

namespace Pyjama
{
    [Serializable]
    public class UserSettings
    {
        public string FontName { get; set; }
        public float FontSize { get; set; }
        public string ShellFontName { get; set; }
        public float ShellFontSize { get; set; }
        public Font UIFont
        {
            get
            {
                return new Font(FontName, FontSize);
            }
        }
        public Font ShellFont
        {
            get
            {
                return new Font(ShellFontName, ShellFontSize);
            }
        }
    }

    public static class ApplicationOptions
    {
        public static string SettingsDirectory { get; set; }
        public static string DefaultExtension { get; set; }
        public static void LoadOptions()
        {
            SettingsDirectory = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "Config");

            DefaultExtension = Properties.Settings.Default.DefaultExtension;
        }

        public static void SaveUserSettings(IsolatedStorageFile isoFile, UserSettings settings)
        {
            IsolatedStorageFileStream isoStream = new IsolatedStorageFileStream( "UserSettings.xml", 
		FileMode.OpenOrCreate, FileAccess.Write, isoFile);

            try
            {
                XmlSerializer serializer = new XmlSerializer(typeof (UserSettings));
                serializer.Serialize(isoStream, settings);
            }
            finally
            {
                isoStream.Close();
            }
        }

        public static IsolatedStorageFile GetIsolatedStorage()
        {
            return IsolatedStorageFile.GetStore(IsolatedStorageScope.User |
                                                 IsolatedStorageScope.Assembly |
                                                 IsolatedStorageScope.Domain,
                                                 GetTypeFromEvidence(AppDomain.CurrentDomain.Evidence, typeof(Url)),
                                                 GetTypeFromEvidence(Assembly.GetAssembly(typeof(ApplicationOptions)).Evidence, 
								     typeof(Url)));
        }


        private static object GetTypeFromEvidence(Evidence evidence, Type type)
        {
            foreach (object e in evidence)
            {
                if (e.GetType().Equals(type))
                    return e;
            }
            return null;
        }


        public static UserSettings LoadUserSettings(IsolatedStorageFile isoFile)
        {
            IsolatedStorageFileStream isoStream = new IsolatedStorageFileStream("UserSettings.xml", 
                  FileMode.OpenOrCreate, FileAccess.Read, isoFile);

            try
            {
                XmlSerializer serializer = new XmlSerializer(typeof (UserSettings));
                object o = serializer.Deserialize(isoStream);
                return o as UserSettings;
            }
            catch
            {
                return null;
            }
            finally
            {
                isoStream.Close();
            }
        }
    }
}
