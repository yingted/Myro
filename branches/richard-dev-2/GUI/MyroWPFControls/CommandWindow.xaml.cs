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
using System.IO;
using System.IO.Pipes;
using System.Threading;

using IronPython.Hosting;
using IronPython.Compiler;

namespace Myro.GUI.WPFControls
{
    /// <summary>
    /// Interaction logic for CommandWindow.xaml
    /// </summary>
    public partial class CommandWindow : UserControl
    {
        PythonEngine pe;
        Stream stdout;
        AnonymousPipeClientStream stdoutpipe;
        Thread readThread;

        public CommandWindow()
        {
            InitializeComponent();
        }

        public void Dispose()
        {
            if (stdout != null)
            {
                stdout.Close();
                stdoutpipe.Close();
            }
            if (readThread != null && readThread.IsAlive)
            {
                readThread.Join();
            }
        }

        public void StartScripting()
        {
            pe = new PythonEngine();

            var s = new AnonymousPipeServerStream(PipeDirection.In);
            stdout = s;
            stdoutpipe = new AnonymousPipeClientStream(PipeDirection.Out, s.ClientSafePipeHandle);
            new StreamWriter(stdoutpipe).WriteLine("Test");
            readThread = new Thread(new ThreadStart(readLoop));
            readThread.Start();

            pe.SetStandardOutput(stdoutpipe);
            //pe.DefaultModule = pe.CreateModule("__main__", new Dictionary<string,object>(), true);
            //pe.Import("site");
        }

        private void OnTextInput(object sender, TextCompositionEventArgs e)
        {
            //foreach (char c in e.Text)
            //    Console.WriteLine((byte)c);
            if (e.Text.EndsWith("\n") || e.Text.EndsWith("\r"))
            {
                historyBlock.AppendText("> " + commandLineBox.Text + "\n");
                try
                {
                    pe.Execute(commandLineBox.Text);
                }
                catch (Exception err)
                {
                    if (err.Message != null && err.Message.Length > 0)
                        historyBlock.AppendText("*** " + err.Message + "\n");
                    else
                        historyBlock.AppendText(err.ToString() + "\n");
                }
                //historyBlock.SelectionStart = historyBlock.Text.Length - 1;
                historyScroller.ScrollToBottom();
                commandLineBox.Text = "";
            }
        }

        private void OnHistoryChanged(object sender, TextChangedEventArgs e)
        {
            historyBlock.ScrollToEnd();
        }

        private void readLoop()
        {
            bool shouldStay = true;
            char[] buffer = new char[4096];
            var reader = new StreamReader(stdout, Encoding.UTF8);
            while (shouldStay)
            {
                int read = reader.Read(buffer, 0, 4096);
                if (read > 0)
                    Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal, new ThreadStart(
                        delegate() { historyBlock.AppendText(new String(buffer, 0, read)); }));
                else
                    shouldStay = false;
            }
        }
    }
}
