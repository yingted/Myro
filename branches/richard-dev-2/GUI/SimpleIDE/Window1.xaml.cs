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
using System.Windows.Media.Effects;
using Microsoft.Win32;
using System.Threading;

using Myro;

namespace Myro.GUI.SimpleIDE
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        string curManifest = null;
        bool connected = false;
        //Robot robot = null;
        Object connectedLock = new Object();
        Thread connectionThread = null;
        Object connectionThreadLock = new Object();
        Editor editor = null;

        public Window1()
        {
            InitializeComponent();
        }

        private void OnClosing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            try
            {
                if (editor.RequestCloseAll() == false)
                    e.Cancel = true;
            }
            catch (Exception)
            {
                e.Cancel = true;
            }
        }

        private void OnClosed(object sender, EventArgs e)
        {
            controlPanel.Dispose();
            commandWindow.Dispose();
            Robot.Shutdown();
        }

        //private void disconnect()
        //{
        //    Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
        //        new ThreadStart(delegate()
        //        {
        //            manifestBox.Text = "Disconnecting from robot...";
        //        }));
        //    controlPanel.SetRobot(null);
        //    if (robot != null)
        //        robot.Shutdown();
        //    robot = null;
        //    Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
        //        new ThreadStart(delegate()
        //        {
        //            controlPanel.IsEnabled = false;
        //            try
        //            {
        //                ((Image)connectButton.Content).Source = new BitmapImage(new Uri("disconnect.png", UriKind.Relative));
        //                manifestBox.Text = (curManifest == null ? "Disconnected" : curManifest + " (disconnected)");
        //            }
        //            catch (Exception e) { Console.WriteLine(e); }
        //        }));
        //}

        private void connect()
        {
            if (connected == false && curManifest != null)
            {
                Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                    new ThreadStart(delegate()
                    {
                        manifestBox.Text = "Connecting to robot...";
                        commandWindow.ExecuteCommand("from myro import *");
                        commandWindow.LogText("> init('" + curManifest + "')\n", Colors.MediumBlue);
                    }));
                Robot.Init(curManifest);
                controlPanel.SetRobot();
                Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                    new ThreadStart(delegate()
                    {
                        controlPanel.IsEnabled = true;
                        try
                        {
                            //((Image)connectButton.Content).Source = new BitmapImage(new Uri("connected.png", UriKind.Relative));
                            manifestBox.Text = (curManifest == null ? "" : curManifest);
                        }
                        catch (Exception e) { Console.WriteLine(e); }
                    }));
            }
        }

        //private void cycle()
        //{
        //    disconnect();
        //    connect();
        //}

        //private void toggle()
        //{
        //    if (robot == null)
        //        connect();
        //    else
        //        disconnect();
        //}

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
                        connectionThread = new Thread(new ThreadStart(delegate() { connect(); }));
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
                    connectionThread = new Thread(new ThreadStart(delegate() { connect(); }));
                    connectionThread.Start();
                }
            }
        }

        private void OnInitialized(object sender, EventArgs e)
        {
            commandWindow.StartScripting();
            commandWindow.PythonExecuting +=
                delegate(object source, EventArgs e2)
                {
                    Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal,
                        new ThreadStart(delegate() { runButton.BitmapEffect = new OuterGlowBitmapEffect() { GlowColor = Colors.Yellow, GlowSize = 5 }; }));
                };
            commandWindow.PythonFinished +=
                delegate(object source, EventArgs e2)
                {
                    Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal,
                        new ThreadStart(delegate() { runButton.BitmapEffect = null; }));
                };
            editor = new Editor(this);
            editor.InsertedEditor += OnEditorInserted;
            editor.RemovedEditor += OnEditorRemoved;
            editor.ActivatedEditor += OnEditorActivated;
            editor.ModifiedChanged += OnEditorNameChanged;
            editor.NameChanged += OnEditorNameChanged;
            SetButtonsEnabled();
        }

        private void OnNew(object sender, RoutedEventArgs e)
        {
            try { editor.RequestNewDocument(); }
            catch (Exception) { }
        }

        private void OnSave(object sender, RoutedEventArgs e)
        {
            var doc = GetCurrentDocument();
            if (doc != null)
                try { editor.RequestSaveDocument(doc); }
                catch (Exception) { }
        }

        private void OnSaveAs(object sender, RoutedEventArgs e)
        {
            var doc = GetCurrentDocument();
            if(doc != null)
                try { editor.RequestSaveAs(doc); }
                catch (Exception) { }
        }

        private void OnOpen(object sender, RoutedEventArgs e)
        {
            try { editor.RequestOpen(); }
            catch (Exception) { }
        }

        private void OnSaveAll(object sender, RoutedEventArgs e)
        {
            try { editor.RequestSaveAll(); }
            catch (Exception) { }
        }

        private void OnExit(object sender, RoutedEventArgs e)
        {
            this.Close();
        }

        private void OnRun(object sender, RoutedEventArgs e)
        {
            var editor = GetCurrentEditor();
            if (editor != null)
                commandWindow.ExecuteCommandSilently(editor.Text);
        }

        private int FindEditor(Editor.EditorDocument doc)
        {
            for (int i = 0; i < mainTabs.Items.Count; i++)
                if (((TabItem)mainTabs.Items[i]).Content == doc.EditorControl)
                    return i;
            throw new ArgumentException("Editor document not found in tabs");
        }

        private Editor.EditorDocument GetCurrentDocument()
        {
            if (mainTabs.SelectedIndex >= 0)
            {
                var content = GetCurrentEditor();
                if (content != null)
                    foreach (var doc in editor.Documents)
                        if (content == doc.EditorControl)
                            return doc;
                return null;
            }
            else
                return null;
        }

        private TextBox GetCurrentEditor()
        {
            if (mainTabs.SelectedIndex >= 0)
                // This returns the textbox if the currently-selected tab contains
                // one, or null if it doesn't.
                return ((TabItem)mainTabs.Items[mainTabs.SelectedIndex]).Content as TextBox;
            else
                return null;
        }

        private void OnEditorInserted(object sender, Editor.EditorEventArgs e)
        {
            var item = new TabItem()
            {
                Header = e.Document.FileName,
                Content = e.Document.EditorControl
            };
            item.GotFocus +=
                delegate(object sender2, RoutedEventArgs e2)
                {
                    FocusManager.SetFocusedElement(mainTabs, e.Document.EditorControl);
                };
            mainTabs.Items.Add(item);
            mainTabs.SelectedIndex = mainTabs.Items.Count - 1;
        }

        private void OnEditorRemoved(object sender, Editor.EditorEventArgs e)
        {
            mainTabs.Items.RemoveAt(FindEditor(e.Document));
        }

        private void OnEditorActivated(object sender, Editor.EditorEventArgs e)
        {
            mainTabs.SelectedIndex = FindEditor(e.Document);
            //if (mainTabs.SelectedItem != null && GetCurrentEditor() != null)
            //    FocusManager.SetFocusedElement((TabItem)mainTabs.SelectedItem, GetCurrentEditor());
        }

        private void OnEditorNameChanged(object sender, Editor.EditorEventArgs e)
        {
            if (e.Document.IsModified)
                ((TabItem)mainTabs.Items[FindEditor(e.Document)]).Header = e.Document.FileName + " *";
            else
                ((TabItem)mainTabs.Items[FindEditor(e.Document)]).Header = e.Document.FileName;
        }

        private void SetButtonsEnabled()
        {
            if (GetCurrentEditor() == null)
                saveItem.IsEnabled =
                    saveAllItem.IsEnabled =
                    saveAsItem.IsEnabled =
                    saveButton.IsEnabled =
                    saveAllButton.IsEnabled =
                    copyItem.IsEnabled =
                    cutItem.IsEnabled =
                    pasteItem.IsEnabled =
                    copyButton.IsEnabled =
                    cutButton.IsEnabled =
                    pasteButton.IsEnabled =
                    false;
            else
                saveItem.IsEnabled =
                    saveAllItem.IsEnabled =
                    saveAsItem.IsEnabled =
                    saveButton.IsEnabled =
                    saveAllButton.IsEnabled =
                    copyItem.IsEnabled =
                    cutItem.IsEnabled =
                    pasteItem.IsEnabled =
                    copyButton.IsEnabled =
                    cutButton.IsEnabled =
                    pasteButton.IsEnabled =
                    true;
        }

        private void OnTabChanged(object sender, SelectionChangedEventArgs e)
        {
            SetButtonsEnabled();
            if (mainTabs.SelectedItem != null && GetCurrentEditor() != null)
                FocusManager.SetFocusedElement((TabItem)mainTabs.SelectedItem, GetCurrentEditor());
        }

        private void OnCut(object sender, RoutedEventArgs e)
        {
            var editor = GetCurrentEditor();
            if (editor != null)
                editor.Cut();
        }

        private void OnCopy(object sender, RoutedEventArgs e)
        {
            var editor = GetCurrentEditor();
            if (editor != null)
                editor.Copy();
        }

        private void OnPaste(object sender, RoutedEventArgs e)
        {
            var editor = GetCurrentEditor();
            if (editor != null)
                editor.Paste();
        }
    }
}
