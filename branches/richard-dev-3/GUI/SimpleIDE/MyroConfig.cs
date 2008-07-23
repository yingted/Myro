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
using Myro.Utilities;

namespace Myro.GUI.SimpleIDE
{
    public static class MyroConfigGUI
    {
        public static UIElement MakeListItem(MyroConfigFiles myroConfig)
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
