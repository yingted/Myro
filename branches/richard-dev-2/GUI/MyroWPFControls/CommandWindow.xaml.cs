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
        Stream stderr;
        AnonymousPipeClientStream stderrpipe;
        Thread readThreadOut;
        Thread readThreadErr;

        Paragraph paragraph;
        bool lastNewlineTrimmed = false;

        public static readonly Color TextColor = Colors.Black;
        public static readonly Color ErrColor = Colors.Crimson;

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
                stderr.Close();
                stderrpipe.Close();
            }
            if (readThreadOut != null && readThreadOut.IsAlive)
            {
                readThreadOut.Join();
            }
            if (readThreadErr != null && readThreadErr.IsAlive)
            {
                readThreadErr.Join();
            }
        }

        public void StartScripting()
        {
            pe = new PythonEngine();
            pe.Sys.path.Add("C:\\Microsoft Robotics Dev Studio 2008\\bin");
            pe.Sys.path.Add("C:\\Users\\t-richr\\Myro-dev\\richard-dev-2\\Frontend\\Python");
            pe.Import("site");

            var s = new AnonymousPipeServerStream(PipeDirection.In);
            stdout = s;
            stdoutpipe = new AnonymousPipeClientStream(PipeDirection.Out, s.ClientSafePipeHandle);
            readThreadOut = new Thread(new ThreadStart(delegate() { readLoop(stdout, Colors.Black); }));
            readThreadOut.Start();

            var e = new AnonymousPipeServerStream(PipeDirection.In);
            stderr = e;
            stderrpipe = new AnonymousPipeClientStream(PipeDirection.Out, e.ClientSafePipeHandle);
            readThreadErr = new Thread(new ThreadStart(delegate() { readLoop(stderr, Colors.Crimson); }));
            readThreadErr.Start();

            pe.SetStandardOutput(stdoutpipe);
            pe.SetStandardError(stderrpipe);

            historyBlock.Document.PageWidth = 10000;
            //historyBlock.ViewportWidth = historyBlock.Document.PageWidth;
            historyBlock.Document.Blocks.Clear();
            paragraph = new Paragraph();
            historyBlock.Document.Blocks.Add(paragraph);
            historyBlock.IsEnabled = true;
        }

        public void LogText(string text, Color color)
        {
            int toTrim;
            if (text.Length >= 1)
                if (text[text.Length - 1] == '\n')
                    if (text.Length >= 2 && text[text.Length - 2] == '\r')
                        toTrim = 2;
                    else
                        toTrim = 1;
                else
                    toTrim = 0;
            else
                toTrim = 0;

            string toAdd;
            if (toTrim > 0)
            {
                if (lastNewlineTrimmed)
                    toAdd = "\n" + text.Substring(0, text.Length - toTrim);
                else
                    toAdd = text.Substring(0, text.Length - toTrim);
                lastNewlineTrimmed = true;
            }
            else
            {
                if (lastNewlineTrimmed)
                    toAdd = "\n" + text;
                else
                    toAdd = text;
                lastNewlineTrimmed = false;
            }
            Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal, new ThreadStart(
                delegate() { paragraph.Inlines.Add(new Run() { Foreground = new SolidColorBrush(color), Text = toAdd }); }));
        }

        public void LogText(string text)
        {
            LogText(text, TextColor);
        }

        public void LogError(string text)
        {
            LogText(text, ErrColor);
        }

        public void ExecuteCommand(string command)
        {
            LogText("> " + command + "\n");
            try
            {
                pe.Execute(command);
            }
            catch (Exception err)
            {
                if (err.Message != null && err.Message.Length > 0)
                    LogText(err.Message + "\n", ErrColor);
                else
                    LogText(err.ToString() + "\n", ErrColor);
            }
        }

        private void OnTextInput(object sender, TextCompositionEventArgs e)
        {
            //foreach (char c in e.Text)
            //    Console.WriteLine((byte)c);
            if (e.Text.EndsWith("\n") || e.Text.EndsWith("\r"))
            {
                ExecuteCommand(commandLineBox.Text);
                //historyBlock.SelectionStart = historyBlock.Text.Length - 1;
                //historyScroller.ScrollToBottom();
                commandLineBox.Text = "";
            }
        }

        private void OnHistoryChanged(object sender, TextChangedEventArgs e)
        {
            historyBlock.ScrollToEnd();
        }

        private void readLoop(Stream stream, Color color)
        {
            bool shouldStay = true;
            char[] buffer = new char[4096];
            var reader = new StreamReader(stream, true);
            while (shouldStay)
            {
                int read = reader.Read(buffer, 0, 4096);
                if (read > 0)
                    LogText(new String(buffer, 0, read), color);
                else
                    shouldStay = false;
            }
        }
    }
}
