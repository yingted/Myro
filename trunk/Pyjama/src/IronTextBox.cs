//---------------------------------------------------------------------------------
//IronTextBox.cs - version 2.0.2.0b
// TextBox control based class designed to be used with Microsoft's IronPython.
// Maybe useful for testing Python scripts with IronPython. 
//WHAT'S NEW: 
//      -Updated License from GNU to Expat/MIT
//      -Tested with IronPython 2.03B
//TO DO:
//      -Fix raw_input support: "s = raw_input('--> ')"
//      -Multiple arg support for "paths" command. eg. "paths -misc -python24"
//      -Intellisense ToolTip
//
//BY DOWNLOADING AND USING, YOU AGREE TO THE FOLLOWING TERMS:
//Copyright (c) 2006-2008 by Joseph P. Socoloski III
//LICENSE
//Permission is hereby granted, free of charge, to any person obtaining a copy
//of this software and associated documentation files (the "Software"), to deal
//in the Software without restriction, including without limitation the rights
//to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//furnished to do so, subject to the following conditions:
//
//The above copyright notice and this permission notice shall be included in
//all copies or substantial portions of the Software.
//the MIT License, given here: <http://www.opensource.org/licenses/mit-license.php> 
//---------------------------------------------------------------------------------

using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Text;
using System.Windows.Forms;
using System.ComponentModel;//ToolboxItem
using System.Drawing;       //ToolboxBitmap
using System.Threading;

using IronPython.Runtime;   //PythonDictionary
using IronPython.Hosting;   //PythonEngine
using IronRuby.Hosting;
using Microsoft.Scripting;  //ScriptDomainManager
using Microsoft.Scripting.Hosting;

namespace UIIronTextBox
{
    delegate void StringParameterDelegate(string value);

    [ToolboxItem(true)]
    [ToolboxBitmap(typeof(IronTextBox))]
    [DesignerAttribute(typeof(IronTextBoxControl))]
    public class IronTextBox : RichTextBox
    {
        /// <summary>
        /// Default prompt text.
        /// </summary>
        private string prompt = ">>> ";

        /// <summary>
        /// Used for storing commands.
        /// </summary>
        public UIIronTextBox.CommandHistory commandHistory = new CommandHistory();

        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.Container components = null;

        /// <summary>
        /// Holds temporary defFunc lines.
        /// </summary>
        public System.Text.StringBuilder defStmtBuilder = new System.Text.StringBuilder();

        /// <summary>
        /// StringCollection of all MiscDirs
        /// </summary>
        public static StringCollection scMisc = new StringCollection();

        /// <summary>
        /// StringCollection of all Python24Dirs
        /// </summary>
        public static StringCollection scPython24 = new StringCollection();

        /// <summary>
        /// StringCollection of all IronPythonDirs
        /// </summary>
        public static StringCollection scIronPython = new StringCollection();

        /// <summary>
        /// Intellisense ToolTip.
        /// </summary>
        System.Windows.Forms.ToolTip intellisense = new System.Windows.Forms.ToolTip();

        /// <summary>
        /// True if currently processing raw_text()
        /// </summary>
        public static Boolean IsRawInput = false;

        /// <summary>
        /// Hold raw_input prompt by user
        /// </summary>
        public string rawprompt = "";

        internal IronTextBox()
        {
            InitializeComponent();
            this.WordWrap = true;
            this.ReadOnly = true;
            // Set up the delays for the ToolTip.
            intellisense.AutoPopDelay = 1000;
            intellisense.InitialDelay = 100;
            intellisense.ReshowDelay = 100;
            // Force the ToolTip text to be displayed whether or not the form is active.
            intellisense.ShowAlways = true;
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
        }

        /// <summary>
        /// Overridden to protect against deletion of contents
        /// cutting the text and deleting it from the context menu
        /// </summary>
        /// <param name="m"></param>
        protected override void WndProc(ref Message m)
        {
            // This happens before key press handler
            // Need to pass to base to have message handled
            //System.Console.WriteLine("Window message received: " + m.Msg.ToString());
            switch (m.Msg)
            {
                case 0x0302: //WM_PASTE
                case 0x0300: //WM_CUT
                case 0x000C: //WM_SETTEXT
                    break;
                case 0x0303: //WM_CLEAR
                    return;
            }
            base.WndProc(ref m);
        }

        private void InitializeComponent()
        {
            this.SuspendLayout();
            // 
            // consoleTextBox
            // 
            this.Dock = DockStyle.Fill;
            this.Location = new Point(0, 0);
            this.MaxLength = 0;
            this.Multiline = true;
            this.AcceptsTab = true;
            this.ScrollBars = System.Windows.Forms.RichTextBoxScrollBars.Both;
            this.Size = new Size(400, 176);
            this.TabIndex = 0;
            this.Name = "IronTextBox";
            this.Size = new Size(400, 176);
            this.ResumeLayout(false);
        }

        public void printPrompt()
        {
            //System.Console.WriteLine("[print prompt]");
            string currentText = this.Text;
            //add newline if it needs one
            if ((currentText.Length != 0) && (currentText[currentText.Length - 1] != '\n'))
                printLine();
            //add the prompt
            this.AddText(Prompt);
            this.Select(this.TextLength - prompt.Length, prompt.Length);
            this.SelectionColor = Color.Yellow;
            //this.Select(this.TextLength, 0); // clears the selection
            //MoveCaretToEndOfText();
            this.SelectionStart = this.TextLength;
            this.SelectionColor = Color.White;
            this.AddText("\n>>> ");
        }

        public void printResult(string text)
        {
            string currentText = this.Text;
            //add newline if it needs one
            if ((currentText.Length != 0) && (currentText[currentText.Length - 1] != '\n'))
                printLine();
            //add the prompt
            this.AddText(text);
            this.Select(this.TextLength - text.Length, text.Length);
            this.SelectionColor = Color.Green;
            //this.Select(this.TextLength, 0); // clears the selection
            //MoveCaretToEndOfText();
            this.SelectionStart = this.TextLength;
            this.SelectionColor = Color.White;
        }

        public void printTextOnNewline(string text)
        {
            if (InvokeRequired)
            {
                // We're not in the UI thread, so we need to call BeginInvoke
                BeginInvoke(new StringParameterDelegate(printTextOnNewline),
                new object[] { text });
                return;
            }
            //System.Console.WriteLine("newline?");
            string currentText = this.Text;
            //add newline if it needs one
            if (currentText.Length != 0)
            {
                if (currentText[currentText.Length - 1] != '\n' &&
                currentText[currentText.Length - 1] != '\r')
                {
                    printLine();
                }
            }
            //add the prompt
            this.AddText(text);
        }

        public void printLine()
        {
            this.AddText(System.Environment.NewLine);
        }

        public string GetTextAtPrompt()
        {
            if (GetCurrentLineText() != "")
                return GetCurrentLineText().Substring(prompt.Length);
            else
            {
                string mystring = (string)this.Lines.GetValue(this.Lines.Length - 2);
                return mystring.Substring(prompt.Length);
            }
        }

        public void AddcommandHistory(string currentCommand)
        {
            commandHistory.Add(currentCommand);
        }

        public string GetCurrentLineText()
        {
            if (this.Lines.Length > 0)
            {
                return (string)this.Lines.GetValue(this.Lines.GetLength(0) - 1);
            }
            else
                return "";
        }

        private bool IsCaretAtCurrentLine()
        {
            return this.TextLength - this.SelectionStart <= GetCurrentLineText().Length;
        }

        public void AddText(string text)
        {
            // Don't disable... doesn't update scrollbars, etc if it needs to.
            //this.Enabled = false;
            //System.Console.WriteLine("AddText: '{0}'", text);
            if (InvokeRequired)
            {
                // We're not in the UI thread, so we need to call BeginInvoke
                BeginInvoke(new StringParameterDelegate(AddText),
                new object[] { text });
                return;
            }
            this.AppendText(text);
            //this.Enabled = true;
            MoveCaretToEndOfText();
            //this.Focus();
        }

        public string StringCollecttostring(System.Collections.Specialized.StringCollection inCol)
        {
            string value = "";
            System.Collections.Specialized.StringEnumerator myEnumerator = inCol.GetEnumerator();
            while (myEnumerator.MoveNext())
            {
                value += myEnumerator.Current;
            }

            return value;
        }

        private void MoveCaretToEndOfText()
        {
            this.SelectionStart = this.TextLength;
            this.Select(this.TextLength, 0);
            this.SelectionStart = this.TextLength;
            this.ScrollToCaret();
        }

        public bool IsCaretJustBeforePrompt()
        {
            return IsCaretAtCurrentLine() && GetCurrentCaretColumnPosition() == prompt.Length;
        }

        public int GetCurrentCaretColumnPosition()
        {
            string currentLine = GetCurrentLineText();
            int currentCaretPosition = this.SelectionStart;
            return (currentCaretPosition - this.TextLength + currentLine.Length);
        }

        public int GetCurrentLinePosition()
        {
            // Zero-based
            return (this.GetLineFromCharIndex(this.SelectionStart));
        }

        public void SetPromptText(string val)
        {
            prompt = val;
        }

        public string Prompt
        {
            get { return prompt; }
            set { SetPromptText(value); }
        }

        public string[] GetCommandHistory()
        {
            return commandHistory.GetCommandHistory();
        }

        public void WriteText(string text)
        {
            this.AddText(text);
        }

        public string CreateIndentstring(int indentsize)
        {
            string r = "";
            for (int i = 0; i < indentsize; i++)
            {
                r += " ";
            }
            return r;
        }
    }

    public class PyjamaModule
    {
        public Control CurrentControl = null; // Is there a GUI running? Handle for Invoke
        public bool Threaded = true;     // Is there a Thread running? 
        private Control _override = null;

        public PyjamaModule(Control control, bool thread)
        {
            this.CurrentControl = control;
            this.Threaded = thread;
        }

        public Control TopLevelControl
        {
            get
            {
                if (_override != null)
                {
                    return _override;
                }
                else
                {
                    return this.CurrentControl.TopLevelControl;
                }
            }
            set { _override = value; }
        }
    }

    public class IronTextBoxControl : UserControl
    {
	ScriptRuntimeSetup scriptRuntimeSetup = null;
        public ScriptRuntime environment;
        public ScriptEngine engine;
        public ScriptScope scope;
        public IronTextBox consoleTextBox;
        public event EventCommandEntered CommandEntered;
        private Container components = null;
        public PyjamaModule pyjamaModule = null;
        public Thread backgroundThread = null;
        private string resultMessage = null;

	public void RestartShell() {
	    RestartShell(false);
	}

	public void RestartShell(bool displayRestart) {
            environment = new ScriptRuntime(scriptRuntimeSetup);
            scope = environment.CreateScope();
            engine = environment.GetEngine("py");
            if (System.Environment.OSVersion.Platform == System.PlatformID.Unix)
            {
                engine.SetSearchPaths(new string[] {
                    "/home/dblank/Myro/Pyjama/python",
                    "/usr/lib/python2.5",
                    "/usr/lib/python2.5/site-packages"});
            }
            else
            {
                engine.SetSearchPaths(new string[] {
                    Environment.GetFolderPath(System.Environment.SpecialFolder.DesktopDirectory) 
                        + @"\Myro\Pyjama\python",
                    @"C:\Python25\Lib",
                    @"C:\Python25\site-packages"
                    });
            }
            // Load mscorlib.dll:
            engine.Runtime.LoadAssembly(typeof(string).Assembly);
            // Load Languages so that Host System can find DLLs:
            engine.Runtime.LoadAssembly(typeof(IronPython.Hosting.Python).Assembly);
            engine.Runtime.LoadAssembly(typeof(IronRuby.Hosting.RubyCommandLine).Assembly);
            engine.Runtime.LoadAssembly(typeof(IronRuby.StandardLibrary.BigDecimal.Fraction).Assembly);
            //Load System.dll
            engine.Runtime.LoadAssembly(typeof(System.Diagnostics.Debug).Assembly);
	    if (displayRestart)
		DisplayError("========= Restart =========");
	}

        public StringBuilder defBuilder
        {
            get { return consoleTextBox.defStmtBuilder; }
            set
            {
                if (consoleTextBox != null)
                    consoleTextBox.defStmtBuilder = value;
            }
        }

        /// <summary>
        /// Returns the string array of the command history.
        /// </summary>
        /// <returns></returns>
        public string[] GetCommandHistory()
        {
            return consoleTextBox.GetCommandHistory();
        }

        /// <summary>
        /// Gets and sets console text ForeColor. 
        /// </summary>
        public Color ConsoleTextForeColor
        {
            get { return consoleTextBox != null ? consoleTextBox.ForeColor : Color.White; }
            set
            {
                if (consoleTextBox != null)
                    consoleTextBox.ForeColor = value;
            }
        }

        /// <summary>
        /// Gets and sets console text BackColor. 
        /// </summary>
        public Color ConsoleTextBackColor
        {
            get { return consoleTextBox != null ? consoleTextBox.BackColor : Color.Black; }
            set
            {
                if (consoleTextBox != null)
                    consoleTextBox.BackColor = value;
            }
        }

        /// <summary>
        /// Gets and sets console Font. 
        /// </summary>
        public Font ConsoleTextFont
        {
            get { return consoleTextBox != null ? consoleTextBox.Font : new Font("DejaVu Sans Mono", 10); }
            set
            {
                if (consoleTextBox != null)
                    consoleTextBox.Font = value;
            }
        }

        /// <summary>
        /// Gets and sets string to be used for the Prompt.
        /// </summary>
        public string Prompt
        {
            get { return consoleTextBox.Prompt; }
            set { consoleTextBox.Prompt = value; }
        }

        public IronTextBoxControl()
        {
            InitializeComponent();
            scriptRuntimeSetup = new ScriptRuntimeSetup();
            scriptRuntimeSetup.LanguageSetups.Add(
                new LanguageSetup("IronPython.Runtime.PythonContext, IronPython",
                    "IronPython",
                    new[] { "IronPython", "Python", "py" },
                    new[] { ".py" }));
            scriptRuntimeSetup.LanguageSetups.Add(
                new LanguageSetup("IronRuby.Runtime.RubyContext, IronRuby",
                    "IronRupy",
                    new[] { "IronRuby", "Ruby", "rb" },
                    new[] { ".rb" }));
	    RestartShell();
            //IronTextBox's CommandEntered event
            CommandEntered += new UIIronTextBox.EventCommandEntered(irontextboxControl_CommandEntered);
        }

        void irontextboxControl_CommandEntered(object sender, UIIronTextBox.CommandEnteredEventArgs e)
        {
            string command = e.Command.TrimEnd();
            DoCommand(command);
        }

        public void DoFile(string filename)
        {
            if (backgroundThread != null && backgroundThread.IsAlive)
            {
                DisplayError("Cannot run script; I'm processing...");
                return;
            }
            if (pyjamaModule == null)
            {
                pyjamaModule = new PyjamaModule(this, true);
                environment.Globals.SetVariable("pyjama", pyjamaModule);
            }
            // FIXME: uses cached version; how to force reload?
            // look through sys.modules, if module.__file__ matches, then delete it, and reload it
            // from this file with same name as before

            //ScriptScope sys = engine.GetSysModule(); // sys
            //sys.GetVariable("__file__");
            //? Microsoft.Scripting.SourceLocation;
            /*
            IronPython.Runtime.PythonDictionary dict = (IronPython.Runtime.PythonDictionary)sys.GetVariable("modules");
            List list = dict.items();
            foreach (PythonTuple name_module in list)
            {
                string name = (string)name_module[0];
                System.Console.WriteLine(name);
                Microsoft.Scripting.Runtime.Scope modScope = (Microsoft.Scripting.Runtime.Scope) name_module[1];
                System.Console.WriteLine("   File: {0}",  modScope.ContainsName(Symbol("__file__")));
            }
            */
            //foreach (KeyValuePair<string, object> pair in sys.GetItems()) {
            //    System.Console.WriteLine(pair);
            //}
            if (pyjamaModule.Threaded)
            {
                ThreadStart starter = delegate
                {
                    Execute(filename, SourceCodeKind.File);
                };
                backgroundThread = new Thread(new ThreadStart(starter));
                backgroundThread.IsBackground = true;
                // FIXME: set cursor to stay this way till done
                this.TopLevelControl.Cursor = Cursors.WaitCursor;
                backgroundThread.Start();
            }
            else
            {
                Execute(filename, SourceCodeKind.File);
            }
        }

        public void SetRunButton(string text)
        {
            if (InvokeRequired)
            {
                BeginInvoke(new StringParameterDelegate(SetRunButton),
                            new object[] { text });
                return;
            }
            Pyjama.PyjamaForm pjform = (Pyjama.PyjamaForm)this.TopLevelControl;
            pjform.runButton.Text = text;
        }

        internal void CancelCommand()
        {
            if (backgroundThread != null && backgroundThread.IsAlive)
            {
                //engine.Runtime.Host.PlatformAdaptationLayer?;
                backgroundThread.Abort();
                backgroundThread.Join();
                DisplayError("Script aborted!");
            }
            SetRunButton("Run!");
        }

        public void DoCommand(string command)
        {
            if (backgroundThread != null && backgroundThread.IsAlive)
            {
                DisplayError("Cannot evaluate script; I'm processing...");
                return;
            }
            if (pyjamaModule == null)
            {
                pyjamaModule = new PyjamaModule(this, true);
                environment.Globals.SetVariable("pyjama", pyjamaModule);
            }
            command = command.Trim();
            if (command != "")
            {
                bool first = true;
                foreach (string line in command.Split('\n'))
                {
                    if (first)
                        this.consoleTextBox.AddText(line + "\n");
                    else
                        this.consoleTextBox.printTextOnNewline("... " + line + "\n");
                    first = false;
                }
                this.consoleTextBox.AddcommandHistory(command);
                // Interactive Meta Commands
                if (command == "#clear")
                {
                    this.Clear();
                }
                else if (command == "#help")
                {
                    this.WriteText(GetHelpText());
                }
                else if (command == "#python")
                {
                    engine = environment.GetEngine("py");
                    Prompt = "---- Python Mode ----";
                    WriteText(Prompt + "\n");
                }
                else if (command == "#ruby")
                {
                    engine = environment.GetEngine("rb");
                    Prompt = "---- Ruby Mode ----";
                    WriteText(Prompt + "\n");
                }
                else // Command
                {
                    if (pyjamaModule.Threaded)
                    {
                        ThreadStart starter = delegate { Execute(command, SourceCodeKind.InteractiveCode); };
                        backgroundThread = new Thread(new ThreadStart(starter));
                        backgroundThread.IsBackground = true;
                        // FIXME: set cursor to stay this way till done
                        this.TopLevelControl.Cursor = Cursors.WaitCursor;
                        backgroundThread.Start();
                    }
                    else
                    {
                        Execute(command, SourceCodeKind.InteractiveCode);
                    }
                }
            }
        }

        class GUIStream : Stream
        {
            private IronTextBox consoleTextBox = null;
            public GUIStream(IronTextBox ctb)
            {
                consoleTextBox = ctb;
            }
            public override bool CanRead
            {
                get { return true; }
            }
            public override bool CanSeek
            {
                get { return false; }
            }
            public override long Seek(long offset, SeekOrigin origin)
            {
                throw new NotImplementedException();
            }
            private bool ready = true;

            public override int Read(byte[] buffer, int offset, int count)
            {
                //System.Console.WriteLine("buffer='{0}', offset={1}, count={2}",
                //			buffer, offset, count);
                if (ready)
                {
                    // FIXME: get input from GUI
                    // set mode to input
                    // set button to enter
                    // don't execute, but grab text for this
                    // put in buffer
                    // return length
                    ready = false;
                    System.Console.Write("Pyjama Input> ");
                    string text = System.Console.ReadLine();
                    int pos = 0;
                    foreach (char c in text)
                    {
                        buffer[pos++] = (byte)c;
                    }
                    return pos;
                }
                return 0;
            }
            public override void SetLength(long value)
            {
                throw new NotImplementedException();
            }
            public override bool CanWrite
            {
                get { return true; }
            }
            public override long Length
            {
                get { return 0; }
            }
            public override long Position
            {
                get { return 0; }
                set { }
            }
            public override void Flush()
            {
                // no op; nothing to do?
            }

            public override void Write(byte[] buffer, int offset, int count)
            {
                string text = "";
                for (int pos = offset; pos < count; pos++)
                {
                    text += (char)buffer[pos];
                }
                consoleTextBox.AddText(text);
            }

        }

        public void Execute(string command, SourceCodeKind sctype)
        {
            SetRunButton("Stop!");
            ExceptionOperations eo;
            eo = engine.GetService<ExceptionOperations>();
            bool error = false;
            string err_message = null;
            resultMessage = null;
            GUIStream ms = new GUIStream(consoleTextBox);
            engine.Runtime.IO.SetInput(ms,
                       new StreamReader(ms),
                       Encoding.GetEncoding("utf-8"));
            engine.Runtime.IO.SetOutput(ms, new StreamWriter(ms));
            engine.Runtime.IO.SetErrorOutput(ms, new StreamWriter(ms));
            ScriptSource source = null;
            // Compile:
            command = command.Trim();
            if (sctype == SourceCodeKind.File)
            {
                // FIXME: surely a better way to add directories to engine?
                string directory = Path.GetDirectoryName(command);
                ICollection<string> dirs = engine.GetSearchPaths();
                if (!dirs.Contains(directory))
                {
                    string[] dir_array = new string[dirs.Count + 1];
                    // put the new one first, to override any other similar named file
                    dir_array.SetValue(directory, 0);
                    dirs.CopyTo(dir_array, 1);
                    //dir_array[dir_array.Length - 1] = directory;
                    engine.SetSearchPaths(dir_array);
                }
                System.Environment.CurrentDirectory = directory;
                //engine.Runtime.Host.PlatformAdaptationLayer
                //source = engine.CreateScriptSourceFromFile(command, Encoding.GetEncoding("utf-8"));
                try
                {
                    engine.ExecuteFile(command, scope);
                    resultMessage = string.Format("=> Loaded '{0}'\n", command);
                }
                catch (Exception err4) // If fails, something is wrong!
                {
                    error = true;
                    err_message = eo.FormatException(err4);
                }
            }
            else // Not a file; either a experssion, statement, or set of statements
            {
                source = engine.CreateScriptSourceFromString(command, sctype);
                try
                {
                    source.Compile(); // if it fails, it could be Statements 
                    // Compiled successfully! Execute
                    try
                    {
                        source.Execute(scope);
                    }
                    catch (Exception err3) // If fails, something is wrong!
                    {
                        try
                        {
                            err_message = eo.FormatException(err3);
                        }
                        catch
                        {
                            err_message = "An error occurred, and then an error in formatting the error occurred.";
                        }
                        error = true;
                    }
                }
                catch (Exception err)
                {
                    try
                    {
                        err_message = eo.FormatException(err);
                    }
                    catch
                    {
                        err_message = "An error occurred, and then an error in formatting the error occurred.";
                    }
                    error = true;
                    // Let's try again, as statements:
                    source = engine.CreateScriptSourceFromString(command, SourceCodeKind.Statements);
                    try
                    {
                        source.Execute(scope);
                        // It worked! Clear the error
                        err_message = null;
                        error = false;
                    }
                    catch (Exception err2)
                    {
                        // Fail!
                        err_message = eo.FormatException(err2);
                        error = true;
                    }
                }
            }
            if (error)
            {
                DisplayError(err_message);
            }
            else
            {
                ScrollToBottom();
            }
            Invoke(new MethodInvoker(FinishCommand));
            SetRunButton("Run!");
            return;
        }

        private void ScrollToBottom()
        {
            if (InvokeRequired)
            {
                // We're not in the UI thread, so we need to call BeginInvoke
                Invoke(new MethodInvoker(ScrollToBottom));
                return;
            }
            consoleTextBox.Select(consoleTextBox.TextLength, 0);
            consoleTextBox.SelectionStart = consoleTextBox.TextLength;
            consoleTextBox.ScrollToCaret();
        }

        private void FinishCommand()
        {
            // Put the text cursor in the commandtextBox
            // Put the mouse cursor back
            if (resultMessage != null)
                consoleTextBox.printTextOnNewline(resultMessage);
            consoleTextBox.printTextOnNewline(">>> ");
            this.TopLevelControl.Cursor = Cursors.Default;
            consoleTextBox.Select(consoleTextBox.TextLength, 0);
            consoleTextBox.SelectionStart = consoleTextBox.TextLength;
            consoleTextBox.ScrollToCaret();
            ((Pyjama.PyjamaForm)this.TopLevelControl).SelectCommandShell();
        }

        private void DisplayError(string err_message)
        {
            if (InvokeRequired)
            {
                // We're not in the UI thread, so we need to call BeginInvoke
                BeginInvoke(new StringParameterDelegate(DisplayError),
                new object[] { err_message });
                return;
            }
            // Must be on the UI thread if we've got this far

            //consoleTextBox.SelectionStart = consoleTextBox.TextLength;
            err_message = err_message.Replace("\r\n", "\n");
            if (!err_message.EndsWith("\n"))
                err_message += "\n";
            consoleTextBox.printTextOnNewline(err_message);
            consoleTextBox.Select(consoleTextBox.TextLength - err_message.Length, err_message.Length);
            consoleTextBox.SelectionColor = Color.Red;
            consoleTextBox.SelectionStart = consoleTextBox.TextLength;
            consoleTextBox.ScrollToCaret();
            //consoleTextBox.SelectionStart = consoleTextBox.TextLength;
            consoleTextBox.Select(consoleTextBox.TextLength, 0);
            consoleTextBox.SelectionColor = Color.White;
            consoleTextBox.printTextOnNewline(">>> ");
        }

        /// <summary>
        /// Displays information about IronTextBox and user's IronPython version.
        /// </summary>
        /// <returns>Returns string information about IronTextBox and user's IronPython version.</returns>
        public string GetHelpText()
        {
            string helpText;
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.Append("*********************");
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append("**   Pyjama Help   **");
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append("*********************");
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append("DLR version " + engine.LanguageVersion);
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append("Meta Commands Available:");
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append(" #clear - Clears the screen");
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append(" #python - Switch to Python");
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append(" #ruby - Switch to Ruby");
            stringBuilder.Append(System.Environment.NewLine);
            stringBuilder.Append(System.Environment.NewLine);
            helpText = stringBuilder.ToString();
            return helpText;
        }

        private void InitializeComponent()
        {
            this.consoleTextBox = new UIIronTextBox.IronTextBox();
            this.SuspendLayout();
            // 
            // consoleTextBox
            // 
            //    this.consoleTextBox.AcceptsReturn = true;
            this.consoleTextBox.AcceptsTab = true;
            this.consoleTextBox.BackColor = Color.Black;
            this.consoleTextBox.ForeColor = Color.White;
            this.consoleTextBox.Dock = DockStyle.Fill;
            this.consoleTextBox.Location = new Point(0, 0);
            this.consoleTextBox.Multiline = true;
            this.consoleTextBox.Name = "consoleTextBox";
            this.consoleTextBox.Prompt = "---- Python Mode ----";
            this.consoleTextBox.ScrollBars = RichTextBoxScrollBars.Vertical;
            this.consoleTextBox.Font = new Font("DejaVu Sans Mono", 10);
            this.consoleTextBox.Size = new Size(232, 216);
            this.consoleTextBox.TabIndex = 0;
            // 
            // IronTextBoxControl
            // 
            this.Controls.Add(this.consoleTextBox);
            this.Name = "IronTextBoxControl";
            this.Size = new Size(232, 216);
            this.ResumeLayout(false);
            this.consoleTextBox.Text = "Pyjama Output:";
            this.consoleTextBox.printPrompt();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        internal void FireCommandEntered(string command)
        {
            OnCommandEntered(command);
        }

        /// <summary>
        /// Creates new EventCommandEntered event.
        /// </summary>
        /// <param name="command">Command line string.</param>
        protected virtual void OnCommandEntered(string command)
        {
            if (CommandEntered != null)
                CommandEntered(command, new CommandEnteredEventArgs(command));
        }

        /// <summary>
        /// Clear the current text in the IronTextBox.
        /// </summary>
        public void Clear()
        {
            consoleTextBox.Clear();
        }

        /// <summary>
        /// Send text to the IronTextBox.
        /// </summary>
        /// <param name="text"></param>
        public void WriteText(string text)
        {
            if (InvokeRequired)
            {
                // We're not in the UI thread, so we need to call BeginInvoke
                BeginInvoke(new StringParameterDelegate(WriteText), new object[] { text });
                return;
            }
            consoleTextBox.WriteText(text);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="line"></param>
        /// <returns>true if line begins with #, or begins with """ and endwith """</returns>
        public bool IsSingleCommentLine(string line)
        {
            //Trim the end of the line because sometimes whitespace after """
            line = line.TrimEnd();

            if (line.StartsWith("#") || line.StartsWith("    #") || line.StartsWith("        #") || line.StartsWith("            #") && line != "")
            {
                return true;
            }
            else if (line.StartsWith("\"\"\"") || line.StartsWith("    \"\"\"") || line.StartsWith("        \"\"\"") || line.StartsWith("            \"\"\""))
            {
                if (line.TrimEnd().EndsWith("\"\"\"") && line.IndexOf("\"\"\"") != line.Length - 3)
                {
                    return true;
                }
                else
                    return false;
            }
            else
                return false;
        }

        /// <summary>
        /// Returns aa ArrayList from a StringCollection  
        /// </summary>
        /// <param name="StringColin">Incoming StringCollection.</param>
        public ArrayList Convert_StringCollectiontoArrayList(StringCollection StringColin)
        {
            ArrayList newArrayList = new ArrayList();

            StringEnumerator myEnumerator = StringColin.GetEnumerator();
            while (myEnumerator.MoveNext())
                newArrayList.Add(myEnumerator.Current.ToString());

            return newArrayList;
        }
    }

    public class CommandHistory
    {
        private int currentPosn;
        private string lastCommand;
        private ArrayList commandHistory = new ArrayList();

        public CommandHistory()
        {
        }

        internal void Add(string command)
        {
            // DOS style:
            /*
                if (command != lastCommand)
                {
                    commandHistory.Add(command);
                    lastCommand = command;
                    currentPosn = commandHistory.Count;
                }
            */
            // readline style:
            commandHistory.Add(command);
            lastCommand = command;
            currentPosn = commandHistory.Count;
        }

        internal bool DoesPreviousCommandExist()
        {
            return currentPosn > 0;
        }

        internal bool DoesNextCommandExist()
        {
            return currentPosn < commandHistory.Count - 1;
        }

        internal string GetPreviousCommand()
        {
            lastCommand = (string)commandHistory[--currentPosn];
            lastCommand = (string)commandHistory[currentPosn];
            return lastCommand;
        }

        internal string GetNextCommand()
        {
            lastCommand = (string)commandHistory[++currentPosn];
            return LastCommand;
        }

        internal string LastCommand
        {
            get { return lastCommand; }
        }

        internal string[] GetCommandHistory()
        {
            return (string[])commandHistory.ToArray(typeof(string));
        }
    }

    public class CommandEnteredEventArgs : EventArgs
    {
        string command;
        public CommandEnteredEventArgs(string command)
        {
            this.command = command;
        }

        public string Command
        {
            get { return command; }
        }
    }

    public delegate void EventCommandEntered(object sender, CommandEnteredEventArgs e);
}
