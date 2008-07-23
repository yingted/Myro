// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Media.Animation;
using System.Windows.Media.Effects;
using System.Threading;

using Myro.GUI.WPFControls;
using Myro.Utilities;

namespace Myro.GUI.SimpleIDE
{
    /// <summary>
    /// Interaction logic for TopBar.xaml
    /// </summary>
    public partial class TopBar : UserControl
    {
        #region Properties

        public string DisplayedBaseName
        {
            set
            {
                if (BaseNameLabel != null)
                    BaseNameLabel.Content = value;
            }
        }
        public string DisplayedFriendlyName
        {
            set
            {
                if (FriendlyNameLabel != null)
                    FriendlyNameLabel.Content = value;
            }
        }
        public BitmapImage DisplayedIcon
        {
            set
            {
                if (JewelImage != null)
                    if (value == null)
                        JewelImage.Source = new BitmapImage(new Uri("jewellogo.png", UriKind.Relative));
                    else
                        JewelImage.Source = value;
            }
        }
        private MyroConfigFiles currentConfig;
        public MyroConfigFiles CurrentConfig
        {
            get { return currentConfig; }
            set
            {
                if (currentConfig != value)
                {
                    var evt = new RobotChangeEventArgs() { ConfigFiles = value };
                    RobotChange.Invoke(this, new RobotChangeEventArgs() { ConfigFiles = value });
                    if (evt.Cancel == false)
                    {
                        currentConfig = value;
                        DisplayedBaseName = value.BaseName;
                        DisplayedFriendlyName = value.MyroConfiguration.FriendlyName;
                        DisplayedIcon = value.IconFilePath == null ? null :
                            new BitmapImage(new Uri("file://" + value.IconFilePath));
                    }
                }
            }
        }
        private bool isRunning = false;
        public bool IsRunning
        {
            set
            {
                isRunning = value;
                if (isRunning)
                    JewelButton.BitmapEffect = (BitmapEffect)JewelButton.Resources["RunGlowEffect"];
                else
                    JewelButton.BitmapEffect = null;
            }
            get { return isRunning; }
        }
        #endregion

        #region Events

        public class RobotChangeEventArgs : EventArgs
        {
            public MyroConfigFiles ConfigFiles;
            public bool Cancel = false;
        }
        public delegate void RobotChangeEventHandler(object sender, RobotChangeEventArgs e);
        public event RobotChangeEventHandler RobotChange;

        #endregion

        #region Commands
        #endregion

        #region Private variables

        ContextMenu robotChooserMenu;
        Storyboard jewelAnimation;

        List<MenuItem> cachedConfigMenu;

        #endregion

        public TopBar()
        {
            InitializeComponent();
        }

        #region Event handlers

        private void OnJewelClick(object sender, RoutedEventArgs e)
        {
            try
            {
                if (jewelAnimation != null)
                {
                    jewelAnimation.Stop(JewelButton);
                    jewelAnimation = null;
                }
                if (CurrentConfig == null)
                    displayJewelRobotChooser();
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnInitialized(object sender, EventArgs e)
        {
            try
            {
                jewelAnimation = (Storyboard)JewelButton.FindResource("JewelGlow");
                jewelAnimation.Begin(JewelButton, true);
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
            try
            {
                cachedConfigMenu = makeConfigMenuItems();
            }
            catch (Exception) { }
        }

        #endregion


        #region Helper methods

        private List<MenuItem> makeConfigMenuItems()
        {
            var ret = new List<MenuItem>();
            var finder = new MyroConfigFinder(Myro.Utilities.Params.ConfigPath);
            foreach (var config in finder.FindConfigFiles())
            {
                MenuItem menuitem = new MenuItem()
                {
                    Header = MyroConfigGUI.MakeListItem(config),
                };
                var myConfig = config;
                menuitem.Click += delegate
                {
                    CurrentConfig = myConfig;

                    // Call this to update the state of grayed-out controls
                    // that depend on CurrentConfig being set to become ungrayed.
                    CommandManager.InvalidateRequerySuggested();
                };
                ret.Add(menuitem);
            }
            return ret;
        }

        private void displayJewelRobotChooser()
        {
            //Console.WriteLine(robotChooserMenu);
            if (robotChooserMenu == null)
            {
                JewelButton.SetValue(CheckBox.IsCheckedProperty, true);

                robotChooserMenu = new ContextMenu()
                {
                    PlacementTarget = JewelButton,
                    Placement = PlacementMode.Relative,
                    VerticalOffset = JewelButton.ActualHeight
                };
                robotChooserMenu.Closed += delegate
                {
                    robotChooserMenu = null;
                    JewelButton.SetValue(CheckBox.IsCheckedProperty, false);
                };

                // Use the cached menu the first time - speeds up the first
                // time the user clicks the menu because files are read from
                // disk.  Rebuild it from scratch after the first time though,
                // so that new configs are read.
                List<MenuItem> menuitems;
                if (cachedConfigMenu != null)
                {
                    Console.WriteLine("Using cached menu items");
                    menuitems = cachedConfigMenu;
                    cachedConfigMenu = null;
                }
                else
                {
                    menuitems = makeConfigMenuItems();
                }

                foreach (var menuitem in menuitems)
                    robotChooserMenu.Items.Add(menuitem);

                robotChooserMenu.IsOpen = true;
            }
            //else
            //    robotChooserMenu.IsOpen = false;
        }

        #endregion
    }
}
