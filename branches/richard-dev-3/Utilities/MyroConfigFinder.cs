// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using System.IO;

namespace Myro.Utilities
{
    /// <summary>
    /// The Myro configuation that is serialized and stored in the config directory.
    /// </summary>
    [XmlRoot()]
    public class MyroRobotConfiguration
    {
        [XmlElement()]
        public string FriendlyName = "";

        [XmlElement()]
        public int HttpPort = Myro.Utilities.Params.DefaultHttpPort;

        [XmlElement()]
        public int DsspPort = Myro.Utilities.Params.DefaultDsspPort;
    }

    /// <summary>
    /// The list of files associated with a particular robot, this is returned by
    /// a MyroConfigFinder class.
    /// </summary>
    public class MyroConfigFiles
    {
        public string ConfigFilePath { get; internal set; }
        public string ManifestFilePath { get; internal set; }
        public string IconFilePath { get; internal set; }
        public string BaseName { get; internal set; }
        public MyroRobotConfiguration MyroConfiguration { get; set; }
        public bool MyroConfigExisted { get; internal set; }
    }

    /// <summary>
    /// This class enumerates robot configurations, and deals with saving them when
    /// they are modified using the configuration editor.
    /// </summary>
    public class MyroConfigFinder
    {
        string configPath;
        public const string ManifestSuffix = ".manifest";
        public const string IconSuffix = ".icon";

        /// <summary>
        /// Use the configPath from the Params class
        /// </summary>
        /// <param name="configPath">This should be the config path from the Params class for normal operation.</param>
        public MyroConfigFinder(string configPath)
        {
            this.configPath = configPath;
        }

        private MyroConfigFiles readConfig(string baseName)
        {
            string configFilePath = Path.Combine(configPath, baseName + ".myro.xml");
            string[] icons = Directory.GetFiles(configPath, baseName + IconSuffix + ".*");
            bool myroConfigExisted = File.Exists(configFilePath);
            return new MyroConfigFiles()
            {
                ConfigFilePath = configFilePath,
                ManifestFilePath = Path.Combine(Path.Combine(configPath, (baseName + ".manifest")), (baseName + ".manifest.xml")),
                IconFilePath =
                    (icons.Length >= 1 ?
                    Path.Combine(configPath, icons[0]) :
                    null),
                BaseName = baseName,
                MyroConfiguration =
                    (myroConfigExisted ?
                    (MyroRobotConfiguration)(new XmlSerializer(typeof(MyroRobotConfiguration))
                        .Deserialize(new StreamReader(configFilePath))) :
                    new MyroRobotConfiguration()),
                MyroConfigExisted = myroConfigExisted
            };
        }

        /// <summary>
        /// Enumerate the available robot configurations
        /// </summary>
        /// <returns></returns>
        public List<MyroConfigFiles> FindConfigFiles()
        {
            // Find manifest files of the form
            // "configPath/basename.manifest/basename.manifest.xml"
            return new List<MyroConfigFiles>(
                from f in Directory.GetFileSystemEntries(configPath, ("*" + ManifestSuffix))
                where ((File.GetAttributes(f) & FileAttributes.Directory) != 0)
                let dirName = Path.GetFileName(f)
                let baseName = dirName.Substring(0, (dirName.Length - ManifestSuffix.Length))
                where (File.Exists(Path.Combine(f, (baseName + ".manifest.xml"))))
                select readConfig(baseName));
        }

        /// <summary>
        /// Look for a robot configuration matching the base name, ignoring case.  Throws
        /// BaseNameNotFoundException.
        /// </summary>
        /// <param name="baseName"></param>
        /// <returns></returns>
        public MyroConfigFiles FindFromBaseName(string baseName)
        {
            MyroConfigFiles config = FindConfigFiles().Find(
                c => c.BaseName.Equals(baseName, StringComparison.InvariantCultureIgnoreCase));
            if (config == null)
                throw new BaseNameNotFoundException();
            else
                return config;
        }

        /// <summary>
        /// Store the MyroRobotConfiguration member of the MyroConfigFiles parameter.
        /// The GUI calls this when the user modifies a robot configuration.
        /// </summary>
        /// <param name="config"></param>
        public void WriteConfig(MyroConfigFiles config)
        {
            using (var writer = new StreamWriter(config.ConfigFilePath))
            {
                new XmlSerializer(typeof(MyroRobotConfiguration))
                    .Serialize(writer, config.MyroConfiguration);
                config.MyroConfigExisted = true;
            }
        }

        /// <summary>
        /// Copy the image to the proper location and name, i.e. make it the robot's icon.
        /// </summary>
        /// <param name="config"></param>
        /// <param name="imagePath"></param>
        public void WriteImage(MyroConfigFiles config, string imagePath)
        {
            string iconPath = Path.Combine(configPath,
                (config.BaseName + IconSuffix + Path.GetExtension(imagePath)));
            File.Copy(imagePath, iconPath);
            config.IconFilePath = iconPath;
        }
    }

    /// <summary>
    /// This exception is thrown when FindFromBaseName cannot find the specified robot configuration in the config directory.
    /// </summary>
    public class BaseNameNotFoundException : Exception
    {
        public BaseNameNotFoundException() : base(Strings.BaseNameNotFound) { }
    }
}
