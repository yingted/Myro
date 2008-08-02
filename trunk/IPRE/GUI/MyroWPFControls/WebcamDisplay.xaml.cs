// Copyright (c) Microsoft Corporation.  All rights reserved.

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
                {
                    SnapshotButton.IsEnabled = false;
                    var img = SnapshotButton.Content as UIElement;
                    if(img != null) img.Opacity = 0.25;
                    PlayButton.Content = PlayButton.Resources["Stop"];
                }
                else
                {
                    SnapshotButton.IsEnabled = true;
                    var img = SnapshotButton.Content as UIElement;
                    if(img != null) img.Opacity = 1.0;
                    PlayButton.Content = PlayButton.Resources["Play"];
                }
            }
        }

        MyroImageType curType;
        bool shouldExit = false;
        Thread updateThread = null;
        Thread takePictureThread = null;
        int delayMs = 30;
        int setDarknessRequest = -1;

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
                if (IsActive && curType != null)
                {
                    try
                    {
                        if (setDarknessRequest >= 0)
                        {
                            if (setDarknessRequest == 256)
                                Robot.autoCamera();
                            else
                                Robot.darkenCamera((byte)setDarknessRequest);
                            setDarknessRequest = -1;
                        }
                        takePictureHelper();
                    }
                    catch (Exception e)
                    {
                        Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                            new ThreadStart(delegate() { IsActive = false; GUIUtilities.ReportUnexpectedException(e); }));
                    }
                }
                Thread.Sleep(delayMs);
            }
        }

        private void takePictureHelper()
        {
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
        }

        private void OnPlayClick(object sender, RoutedEventArgs e)
        {
            if (IsActive)
                IsActive = false;
            else
                IsActive = true;
        }

        private void OnSnapClick(object sender, RoutedEventArgs e)
        {
            if (takePictureThread == null)
            {
                SnapshotButton.IsEnabled = false;
                takePictureThread = new Thread(new ThreadStart(delegate()
                    {
                        try
                        {
                            takePictureHelper();
                        }
                        catch (Exception err)
                        {
                            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                                new ThreadStart(delegate() { IsActive = false; GUIUtilities.ReportUnexpectedException(err); }));
                        }
                        finally
                        {
                            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                                new ThreadStart(delegate() { SnapshotButton.IsEnabled = true; }));
                            takePictureThread = null;
                        }
                    }));
                takePictureThread.Start();
            }
        }

        private void OnDarkValueChange(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            DarkLabel.Content = ((int)DarkSlider.Value).ToString();
            setDarknessRequest = (byte)(Math.Max(0, Math.Min(255, DarkSlider.Value)));
        }

        private void OnDarkMouseLost(object sender, MouseEventArgs e)
        {
        }

        private void OnDarkChecked(object sender, RoutedEventArgs e)
        {
            DarkSlider.IsEnabled = true;
            DarkLabel.IsEnabled = true;
            setDarknessRequest = (byte)(Math.Max(0, Math.Min(255, DarkSlider.Value)));
        }

        private void OnDarkUnchecked(object sender, RoutedEventArgs e)
        {
            DarkSlider.IsEnabled = false;
            DarkLabel.IsEnabled = false;
            setDarknessRequest = 256;
        }

    }
}
