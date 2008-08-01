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
using System.IO;
using System.IO.Pipes;
using System.Threading;
using Microsoft.Ccr.Core;
using System.Reflection;

using Myro.Utilities;

using IronPython.Hosting;
using IronPython.Compiler;

namespace Myro.GUI.WPFControls
{
    /// <summary>
    /// Interaction logic for CommandWindow.xaml
    /// </summary>
    public partial class CommandWindow : UserControl
    {
        /// <summary>
        /// This event is raised when a Python command begins to execute.
        /// The GUI uses this event to set the "busy" status indicator.
        /// </summary>
        public event EventHandler PythonExecuting;
        /// <summary>
        /// This event is raised when a Python command finishes executing.
        /// The GUI uses this event to set the "busy" status indicator.
        /// </summary>
        public event EventHandler PythonFinished;

        #region Private variables

        PythonEngine pe;
        Stream stdout;
        AnonymousPipeClientStream stdoutpipe;
        Stream stderr;
        AnonymousPipeClientStream stderrpipe;
        Thread readThreadOut;
        Thread readThreadErr;

        struct Command
        {
            public bool IsInteractive;
            public string CommandString;
        }
        Port<Command> commandQueue;
        DispatcherQueue commandDispatcherQueue;

        Paragraph paragraph;
        bool lastNewlineTrimmed = false;

        LinkedList<string> commandHistory = new LinkedList<string>();
        LinkedListNode<string> currentHistPos = null;

        #endregion

        /// <summary>
        /// The color of normal text in the history buffer
        /// </summary>
        public static readonly Color TextColor = Colors.Black;
        /// <summary>
        /// The color of error text in the history buffer
        /// </summary>
        public static readonly Color ErrColor = Colors.Crimson;

        /// <summary>
        /// Constructor.  You must call Dispose on this class on exit.
        /// </summary>
        public CommandWindow()
        {
            InitializeComponent();
        }

        /// <summary>
        /// This must be called to stop threads and the DispatcherQueue.
        /// </summary>
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
                readThreadOut.Join();
            if (readThreadErr != null && readThreadErr.IsAlive)
                readThreadErr.Join();
            if (commandDispatcherQueue != null)
                commandDispatcherQueue.Dispose();
        }

        /// <summary>
        /// Initialize the control and start the Python engine.  This is not in the constructor or
        /// OnLoaded because it breaks the VS designer.  The main GUI window calls this.
        /// </summary>
        public void StartScripting()
        {
            pe = new PythonEngine();
            pe.Sys.path.Add(Myro.Utilities.Params.BinPath);
            //pe.Sys.path.Add(Myro.Utilities.Params.PythonPath);
            //pe.Sys.path.Add("C:\\Users\\t-richr\\Myro-dev\\richard-dev-2\\Frontend\\Python");
            //pe.Import("site");
            pe.Sys.path.Add(System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location));

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

            commandQueue = new Port<Command>();
            commandDispatcherQueue = new DispatcherQueue("Python command queue", new Dispatcher(1, "Python command queue"));
            Arbiter.Activate(commandDispatcherQueue, Arbiter.Receive(true, commandQueue, commandHandler));

            pe.SetStandardOutput(stdoutpipe);
            pe.SetStandardError(stderrpipe);
            //Console.SetOut(new StreamWriter(stdoutpipe));
            //Console.SetError(new StreamWriter(stderrpipe));

            //Console.OpenStandardOutput();
            //Console.OpenStandardError();

            historyBlock.Document.PageWidth = historyBlock.ViewportWidth;
            historyBlock.Document.Blocks.Clear();
            paragraph = new Paragraph();
            historyBlock.Document.Blocks.Add(paragraph);
            historyBlock.IsEnabled = true;
        }

        /// <summary>
        /// Add text to the history buffer, with a certain color.
        /// </summary>
        /// <param name="text"></param>
        /// <param name="color"></param>
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
            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal, new ThreadStart(
                delegate() { paragraph.Inlines.Add(new Run() { Foreground = new SolidColorBrush(color), Text = toAdd }); }));
        }

        /// <summary>
        /// Add text to the history buffer with the default color (black)
        /// </summary>
        /// <param name="text"></param>
        public void LogText(string text)
        {
            LogText(text, TextColor);
        }

        /// <summary>
        /// Add text to the history buffer with the default error color (red)
        /// </summary>
        /// <param name="text"></param>
        public void LogError(string text)
        {
            LogText(text, ErrColor);
        }

        /// <summary>
        /// Execute a Python command in "interactive" mode, meaning the result of
        /// the command will appear in the history buffer as well as the command
        /// itself (prepended with "> ").
        /// </summary>
        /// <param name="command"></param>
        public void ExecuteCommandInteractive(string command)
        {
            LogText("> " + command + "\n");
            commandQueue.Post(new Command() { IsInteractive = true, CommandString = command });
        }

        /// <summary>
        /// Execute a Python command silently, that is, it will not be printed
        /// in the history buffer, and the result will be ignored instead of
        /// printed.
        /// </summary>
        /// <param name="command"></param>
        public void ExecuteCommandSilently(string command)
        {
            commandQueue.Post(new Command() { IsInteractive = false, CommandString = command });
        }

        /// <summary>
        /// Handler for text input in the command line.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnTextInput(object sender, TextCompositionEventArgs e)
        {
            if (e.Text.EndsWith("\n") || e.Text.EndsWith("\r"))
            {
                ExecuteCommandInteractive(commandLineBox.Text);

                // If the user went up in the history and came back down to the in-progress command,
                // update the in-progress command in the history, otherwise, add a new history item.
                if (currentHistPos != null && currentHistPos == commandHistory.First)
                    currentHistPos.Value = commandLineBox.Text;
                else
                    commandHistory.AddFirst(commandLineBox.Text);
                currentHistPos = null;
                commandLineBox.Text = "";
            }
        }

        /// <summary>
        /// Passes typing in the history buffer to the command line box, and transferrs
        /// focus as well.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void historyBlock_PreviewTextInput(object sender, TextCompositionEventArgs e)
        {
            commandLineBox.Focus();
            commandLineBox.RaiseEvent(e);
        }

        /// <summary>
        /// Scrolls the history buffer to the end when it is modified.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnHistoryChanged(object sender, TextChangedEventArgs e)
        {
            historyBlock.ScrollToEnd();
        }

        /// <summary>
        /// Entry point for the threads that read from stdout and stderr of
        /// the Python engine.
        /// </summary>
        /// <param name="stream"></param>
        /// <param name="color"></param>
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

        /// <summary>
        /// Handler for the Command port.  ExecuteCommandInteractive and ExecuteCommandSilently
        /// post commands to this port.
        /// </summary>
        /// <param name="cmd"></param>
        private void commandHandler(Command cmd)
        {
            PythonExecuting.Invoke(this, new EventArgs());
            Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal, new ThreadStart(
                delegate() { commandLineBox.IsEnabled = false; }));
            try
            {
                if (cmd.IsInteractive)
                    pe.ExecuteToConsole(cmd.CommandString);
                else
                    pe.Execute(cmd.CommandString);
            }
            catch (Exception err)
            {
                LogText(Strings.FromExceptionMessage(err) + "\n", ErrColor);
            }
            Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal, new ThreadStart(
                delegate() { commandLineBox.IsEnabled = true; commandLineBox.Focus(); }));
            PythonFinished.Invoke(this, new EventArgs());
        }

        /// <summary>
        /// This attempts to prevent the horizontal scroll bar from showing up
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnSizeChanged(object sender, SizeChangedEventArgs e)
        {
            historyBlock.Document.PageWidth = historyBlock.ViewportWidth;
        }

        /// <summary>
        /// This attempts to prevent the horizontal scroll bar from showing up
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnLayoutUpdated(object sender, EventArgs e)
        {
            historyBlock.Document.PageWidth = historyBlock.ViewportWidth;
        }

        /// <summary>
        /// Key handler for the command line box, for navigating history.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Up)
            {
                // If going up through the history for the first time since entering
                // a complete command, save the partially-typed (or empty) command
                // in the history.
                if (currentHistPos == null)
                    currentHistPos = commandHistory.AddFirst(commandLineBox.Text);
                else if (currentHistPos == commandHistory.First)
                    currentHistPos.Value = commandLineBox.Text;
                if (currentHistPos.Next != null)
                {
                    currentHistPos = currentHistPos.Next;
                    commandLineBox.Text = currentHistPos.Value;
                    commandLineBox.SelectionStart = commandLineBox.Text.Length;
                }
            }
            else if (e.Key == Key.Down)
            {
                if (currentHistPos != null && currentHistPos.Previous != null)
                {
                    currentHistPos = currentHistPos.Previous;
                    commandLineBox.Text = currentHistPos.Value;
                    commandLineBox.SelectionStart = commandLineBox.Text.Length;
                }
            }
        }
    }
}
