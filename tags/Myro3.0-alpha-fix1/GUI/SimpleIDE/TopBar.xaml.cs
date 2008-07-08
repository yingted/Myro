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
using System.Diagnostics;

using Myro.GUI.WPFControls;

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
                    if (evt.Cancel != true)
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
        public static RoutedCommand ShowServices = new RoutedCommand();
        #endregion

        #region Private variables

        ContextMenu robotChooserMenu;
        Storyboard jewelAnimation;

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
        }

        private void IsConfigLoaded(object sender, CanExecuteRoutedEventArgs e)
        {
            e.CanExecute = (CurrentConfig != null);
        }

        private void OnShowServices(object sender, ExecutedRoutedEventArgs e)
        {
            try
            {
                if (CurrentConfig != null)
                    Process.Start(new ProcessStartInfo(
                        "http://localhost:" + CurrentConfig.MyroConfiguration.HttpPort.ToString() + "/directory"));
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        #endregion


        #region Helper methods

        private void displayJewelRobotChooser()
        {
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
                var finder = new MyroConfigFinder(Myro.Utilities.Params.ConfigPath);
                foreach (var config in finder.FindConfigFiles())
                {
                    MenuItem menuitem = new MenuItem()
                    {
                        Header = finder.MakeListItem(config),
                    };
                    var myConfig = config;
                    menuitem.Click += delegate { CurrentConfig = myConfig; };
                    robotChooserMenu.Items.Add(menuitem);
                }

                robotChooserMenu.IsOpen = true;
            }
            else
                robotChooserMenu.IsOpen = false;
        }

        #endregion
    }
}
