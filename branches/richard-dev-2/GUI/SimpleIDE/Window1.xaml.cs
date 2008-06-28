using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Microsoft.Win32;
using System.Threading;

using Myro;

namespace SimpleIDE
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        string curManifest = null;
        Robot robot = null;
        Object connectedLock = new Object();
        Thread connectionThread = null;
        Object connectionThreadLock = new Object();

        public Window1()
        {
            InitializeComponent();
        }

        private void OnClosed(object sender, EventArgs e)
        {
            controlPanel.Dispose();
            commandWindow.Dispose();
            if (robot != null)
                robot.Shutdown();
        }

        private void disconnect()
        {
            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                new ThreadStart(delegate()
                {
                    manifestBox.Text = "Disconnecting from robot...";
                }));
            controlPanel.SetRobot(null);
            if (robot != null)
                robot.Shutdown();
            robot = null;
            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                new ThreadStart(delegate()
                {
                    controlPanel.IsEnabled = false;
                    try
                    {
                        ((Image)connectButton.Content).Source = new BitmapImage(new Uri("disconnect.png", UriKind.Relative));
                        manifestBox.Text = (curManifest == null ? "Disconnected" : curManifest + " (disconnected)");
                    }
                    catch (Exception e) { Console.WriteLine(e); }
                }));
        }

        private void connect()
        {
            if (robot == null && curManifest != null)
            {
                Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                    new ThreadStart(delegate()
                    {
                        manifestBox.Text = "Connecting to robot...";
                    }));
                robot = new Robot(curManifest);
                controlPanel.SetRobot(robot);
                Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                    new ThreadStart(delegate()
                    {
                        controlPanel.IsEnabled = true;
                        try
                        {
                            ((Image)connectButton.Content).Source = new BitmapImage(new Uri("connected.png", UriKind.Relative));
                            manifestBox.Text = (curManifest == null ? "" : curManifest);
                        }
                        catch (Exception e) { Console.WriteLine(e); }
                    }));
            }
        }

        private void cycle()
        {
            disconnect();
            connect();
        }

        private void toggle()
        {
            if (robot == null)
                connect();
            else
                disconnect();
        }

        private void BrowseManifest(object sender, RoutedEventArgs e)
        {
            var dlg = new OpenFileDialog();
            if (curManifest != null && curManifest.Length > 0)
                dlg.FileName = curManifest;
            dlg.DefaultExt = ".manifest.xml";
            dlg.Filter = "DSS Manifest (.manifest.xml)|*.manifest.xml";
            Nullable<bool> result = dlg.ShowDialog(this);
            if (result == true)
            {
                lock (connectionThreadLock)
                {
                    curManifest = dlg.FileName;
                    if (connectionThread == null || connectionThread.IsAlive == false)
                    {
                        connectionThread = new Thread(new ThreadStart(delegate() { cycle(); }));
                        connectionThread.Start();
                    }
                }
            }
        }

        private void ToggleConnect(object sender, RoutedEventArgs e)
        {
            if (curManifest == null || curManifest.Length <= 0)
                BrowseManifest(sender, e);

            lock (connectionThreadLock)
            {
                if (connectionThread == null || connectionThread.IsAlive == false)
                {
                    connectionThread = new Thread(new ThreadStart(delegate() { toggle(); }));
                    connectionThread.Start();
                }
            }
        }

        private void OnInitialized(object sender, EventArgs e)
        {
            commandWindow.StartScripting();
        }
    }
}
