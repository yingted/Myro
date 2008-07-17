// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace Myro.GUI.SimpleIDE
{
    [XmlRoot()]
    public class MyroConfiguration
    {
        [XmlElement()]
        public string FriendlyName = "";

        [XmlElement()]
        public int HttpPort = Myro.Utilities.Params.DefaultHttpPort;

        [XmlElement()]
        public int DsspPort = Myro.Utilities.Params.DefaultDsspPort;
    }

    public class MyroConfigFiles
    {
        public string ConfigFilePath { get; internal set; }
        public string ManifestFilePath { get; internal set; }
        public string IconFilePath { get; internal set; }
        public string BaseName { get; internal set; }
        public MyroConfiguration MyroConfiguration { get; internal set; }
        public bool MyroConfigExisted { get; internal set; }
    }

    public class MyroConfigFinder
    {
        string configPath;
        public const string ManifestSuffix = ".manifest";
        public const string IconSuffix = ".icon";

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
                    (MyroConfiguration)(new XmlSerializer(typeof(MyroConfiguration))
                        .Deserialize(new StreamReader(configFilePath))) :
                    new MyroConfiguration()),
                MyroConfigExisted = myroConfigExisted
            };
        }

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

        public void WriteConfig(MyroConfigFiles config)
        {
            using (var writer = new StreamWriter(config.ConfigFilePath))
            {
                new XmlSerializer(typeof(MyroConfiguration))
                    .Serialize(writer, config.MyroConfiguration);
                config.MyroConfigExisted = true;
            }
        }

        public void WriteImage(MyroConfigFiles config, string imagePath)
        {
            // Try to load the image, so exception can be thrown
            BitmapImage img = new BitmapImage(new Uri("file://" + imagePath));

            string iconPath = Path.Combine(configPath,
                (config.BaseName + IconSuffix + Path.GetExtension(imagePath)));
            File.Copy(imagePath, iconPath);
            config.IconFilePath = iconPath;
        }

        public UIElement MakeListItem(MyroConfigFiles myroConfig)
        {
            Grid s1 = new Grid() { HorizontalAlignment = HorizontalAlignment.Stretch };
            s1.ColumnDefinitions.Add(new ColumnDefinition() { Width = GridLength.Auto });
            s1.ColumnDefinitions.Add(new ColumnDefinition());
            StackPanel s2 = new StackPanel()
            {
                VerticalAlignment = VerticalAlignment.Center,
                Orientation = Orientation.Vertical
            };
            s2.SetValue(Grid.ColumnProperty, 1);
            Label l1 = new Label()
            {
                Content = myroConfig.BaseName,
                Padding = new Thickness(0, 0, 5, 0)
            };
            Label l2 = new Label()
            {
                Content =
                    myroConfig.MyroConfigExisted ?
                    myroConfig.MyroConfiguration.FriendlyName :
                    "No Myro configuration",
                Padding = new Thickness(0, 0, 5, 0)
            };
            Image i = new Image()
            {
                VerticalAlignment = VerticalAlignment.Center,
                Width = 64,
                Height = 64,
                Margin = new Thickness(2, 2, 3, 2),
                Stretch = Stretch.Uniform
            };
            i.SetValue(Grid.ColumnProperty, 0);
            if (myroConfig.IconFilePath != null)
                i.Source = new BitmapImage(new Uri("file://" + myroConfig.IconFilePath));

            s2.Children.Add(l1);
            s2.Children.Add(l2);
            s1.Children.Add(i);
            s1.Children.Add(s2);

            return s1;
        }

    }
}
