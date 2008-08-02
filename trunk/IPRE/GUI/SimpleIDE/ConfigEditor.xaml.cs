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
using System.Windows.Shapes;
using Myro.Utilities;
using Microsoft.Win32;
using Myro.GUI.WPFControls;

namespace Myro.GUI.SimpleIDE
{
    /// <summary>
    /// Interaction logic for ConfigEditor.xaml
    /// </summary>
    public partial class ConfigEditor : Window
    {
        MyroConfigFinder configFinder;
        List<MyroConfigFiles> configFiles;
        MyroConfigFiles currentConfigFiles;
        string modifiedIconPath = null;
        bool currentModified = false;

        public ConfigEditor()
        {
            InitializeComponent();
        }


        #region Event handlers

        private void OnInitialized(object sender, EventArgs e)
        {
        }

        private void OnLoaded(object sender, RoutedEventArgs e)
        {
            try
            {
                configFinder = new MyroConfigFinder(Params.ConfigPath);
                rebuildConfigList();
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
                this.Close();
            }
        }

        private void OnSelectConfig(object sender, SelectionChangedEventArgs e)
        {
            try
            {
                bool cancelled;
                if (currentModified && ConfigList.SelectedIndex != configFiles.IndexOf(currentConfigFiles))
                {
                    if (promptApplyChanges() == true)
                        cancelled = false;
                    else
                    {
                        ConfigList.SelectedIndex = configFiles.IndexOf(currentConfigFiles);
                        cancelled = true;
                    }
                }
                else
                    cancelled = false;

                if (!currentModified && !cancelled)
                    if (ConfigList.SelectedIndex >= 0 && ConfigList.SelectedIndex < configFiles.Count)
                    {
                        currentConfigFiles = configFiles[ConfigList.SelectedIndex];
                        fillInConfig(currentConfigFiles);
                    }
                    else
                    {
                        currentConfigFiles = null;
                        fillInConfig(null);
                    }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
                this.Close();
            }
        }

        private void OnClosing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            try
            {
                if (currentModified && promptApplyChanges() == false)
                    e.Cancel = true;
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
                this.Close();
            }
        }

        private void OnIconClick(object sender, MouseButtonEventArgs e)
        {
            try
            {
                if (currentConfigFiles != null)
                {
                    var dlg = new OpenFileDialog();
                    if (currentConfigFiles.IconFilePath != null)
                        dlg.FileName = currentConfigFiles.IconFilePath;
                    if (dlg.ShowDialog(this) == true)
                    {
                        try
                        {
                            setIcon(new BitmapImage(new Uri("file://" + dlg.FileName)));
                            modifiedIconPath = dlg.FileName;
                            currentModified = true;
                            setModifiedStyle();
                        }
                        catch (NotSupportedException)
                        {
                            MessageBox.Show(this, "We cannot understand this image type.  Try another image type such as png, bmp, gif, jpeg, etc.", "Image type not supported", MessageBoxButton.OK, MessageBoxImage.Exclamation, MessageBoxResult.OK);
                        }
                    }
                }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnSave(object sender, RoutedEventArgs e)
        {
            try
            {
                saveChanges();
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnRevert(object sender, RoutedEventArgs e)
        {
            try
            {
                revertChanges();
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnTextChanged(object sender, TextChangedEventArgs e)
        {
            try
            {
                if (currentConfigFiles != null)
                    if (
                        (sender == FriendlyNameBox && (!currentConfigFiles.MyroConfiguration.FriendlyName.Equals(FriendlyNameBox.Text))) ||
                        (sender == HttpPortBox && (!currentConfigFiles.MyroConfiguration.HttpPort.ToString().Equals(HttpPortBox.Text))) ||
                        (sender == DsspPortBox && (!currentConfigFiles.MyroConfiguration.DsspPort.ToString().Equals(DsspPortBox.Text))))
                    {
                        currentModified = true;
                        setModifiedStyle();
                    }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        #endregion


        #region Helper methods

        private void rebuildConfigList()
        {
            int oldIndex = ConfigList.SelectedIndex;
            ConfigList.Items.Clear();
            configFiles = configFinder.FindConfigFiles();
            foreach (var c in configFiles)
                ConfigList.Items.Add(MyroConfigGUI.MakeListItem(c));
            if (oldIndex < ConfigList.Items.Count)
                ConfigList.SelectedIndex = oldIndex;
        }

        private void fillInConfig(MyroConfigFiles myroConfig)
        {
            if (myroConfig == null)
            {
                IconBox.Source = null;
                IconRectangle.Visibility = Visibility.Visible;
                FriendlyNameBox.Text = "";
                HttpPortBox.Text = Params.DefaultHttpPort.ToString();
                DsspPortBox.Text = Params.DefaultDsspPort.ToString();
            }
            else
            {
                if (myroConfig.IconFilePath != null)
                {
                    setIcon(new BitmapImage(new Uri("file://" + myroConfig.IconFilePath)));
                }
                else
                {
                    setIcon(null);
                }

                ManifestBox.Text = myroConfig.ManifestFilePath;
                FriendlyNameBox.Text = myroConfig.MyroConfiguration.FriendlyName;
                HttpPortBox.Text = myroConfig.MyroConfiguration.HttpPort.ToString();
                DsspPortBox.Text = myroConfig.MyroConfiguration.DsspPort.ToString();
            }
        }

        private void setIcon(BitmapImage image)
        {
            if (image == null)
            {
                IconBox.Source = null;
                IconRectangle.Opacity = 1.0;
                IconText.Visibility = Visibility.Visible;
            }
            else
            {
                IconRectangle.Opacity = 0.0;
                IconText.Visibility = Visibility.Hidden;
                IconBox.Source = image;
            }
        }

        public void setModifiedStyle()
        {
            //int index = configFiles.IndexOf(currentConfigFiles);
            //if (index >= 0 && index < ConfigList.Items.Count)
            //{
            //    Grid item = (Grid)ConfigList.Items[index];
            //    if (currentModified)
            //        item.Background = new SolidColorBrush(Color.FromRgb(0xF3, 0x96, 0x88));
            //    else
            //        item.Background = Brushes.Transparent;
            //}
        }

        private bool promptApplyChanges()
        {
            if (currentConfigFiles != null)
            {
                switch (MessageBox.Show(this, Strings.SaveConfigPrompt, "Myro",
                    MessageBoxButton.YesNoCancel, MessageBoxImage.Question, MessageBoxResult.Cancel))
                {
                    case MessageBoxResult.Yes:
                        saveChanges();
                        return true;
                    case MessageBoxResult.No:
                        revertChanges();
                        return true;
                    case MessageBoxResult.Cancel:
                        return false;
                    default:
                        return true;
                }
            }
            else
                return true;
        }

        private void saveChanges()
        {
            if (currentConfigFiles != null)
            {
                // Create new config based on controls' text
                currentConfigFiles.MyroConfiguration = new MyroRobotConfiguration()
                {
                    FriendlyName = FriendlyNameBox.Text,
                    HttpPort = int.Parse(HttpPortBox.Text),
                    DsspPort = int.Parse(DsspPortBox.Text)
                };
                if (modifiedIconPath != null)
                {
                    configFinder.WriteImage(currentConfigFiles, modifiedIconPath);
                    modifiedIconPath = null;
                }
                configFinder.WriteConfig(currentConfigFiles);
                currentModified = false;
            }
            else
            {
                modifiedIconPath = null;
                currentModified = false;
            }
            rebuildConfigList();
            setModifiedStyle();
        }

        private void revertChanges()
        {
            fillInConfig(currentConfigFiles);
            currentModified = false;
            modifiedIconPath = null;
            setModifiedStyle();
        }

        #endregion
    }
}
