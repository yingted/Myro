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
using System.Threading;
using System.Drawing;
using Myro.Utilities;

namespace Myro.GUI.WPFControls
{
    /// <summary>
    /// Interaction logic for WebcamDisplay.xaml
    /// </summary>
    public partial class WebcamDisplay : UserControl
    {
        /// <summary>
        /// Set whether the Webcam display will query and update the image
        /// </summary>
        bool isActive = false;
        public bool IsActive
        {
            get
            {
                return isActive;
            }
            set
            {
                isActive = value;
                if (isActive)
                    PlayButton.Content = PlayButton.Resources["Stop"];
                else
                    PlayButton.Content = PlayButton.Resources["Play"];
            }
        }

        MyroImageType curType;
        bool shouldExit = false;
        Thread updateThread = null;
        int delayMs = 500;

        public WebcamDisplay()
        {
            InitializeComponent();
        }

        public void Dispose()
        {
            shouldExit = true;
            if (updateThread != null)
                updateThread.Join();
        }

        public void StartUpdate()
        {
            foreach (var it in MyroImageType.KnownImageTypes)
            {
                ImageTypesBox.Items.Add(new ComboBoxItem()
                {
                    Content = "\"" + it.ShortName + "\": " + it.FriendlyName,
                    Tag = it
                });
            }
            if (ImageTypesBox.Items.Count > 0)
                ImageTypesBox.SelectedIndex = 0;
            updateThread = new Thread(new ThreadStart(updateLoop));
            updateThread.Start();
        }

        private void OnInitialized(object sender, EventArgs e)
        {
        }

        private void OnImageTypeChange(object sender, SelectionChangedEventArgs e)
        {
            ComboBoxItem item = ImageTypesBox.SelectedItem as ComboBoxItem;
            // This could be null because nothing is selected, or because
            // of a casting error (the latter should never happen).
            if (item != null)
            {
                curType = (MyroImageType)item.Tag;
            }
        }


        private void updateLoop()
        {
            while (!shouldExit)
            {
                if (isActive && curType != null)
                {
                    //try
                    //{
                        Bitmap img = Robot.TakeBitmap(curType);
                        Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                            new ThreadStart(delegate()
                        {
                            CamImage.Source = System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(
                                img.GetHbitmap(),
                                IntPtr.Zero,
                                Int32Rect.Empty,
                                System.Windows.Media.Imaging.BitmapSizeOptions.FromEmptyOptions());
                        }));
                //    }
                //    catch (Exception e)
                //    {
                //        Console.WriteLine(e);
                //    }
                }
                Thread.Sleep(delayMs);
            }
        }

        private void OnPlayClick(object sender, RoutedEventArgs e)
        {
            if (IsActive)
                IsActive = false;
            else
                IsActive = true;
        }

    }
}
