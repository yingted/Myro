using System;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
using System.Threading;
using Pyjama.Dialogs;

namespace Pyjama
{
    public partial class PyjamaForm : BaseForm, IMainForm
    {
        private PyjamaFormController _controller;

        public PyjamaForm(string[] args)
        {
            InitializeComponent();
            KeyDown += new KeyEventHandler(mainForm_KeyDown);

            _controller = new PyjamaFormController(this);

            Text = string.Format(Text, ApplicationInformation.Title(), ApplicationInformation.Version());

            SetButtonsStatus();
            ApplyUserSettings(ApplicationOptions.LoadUserSettings(ApplicationOptions.GetIsolatedStorage()));
            //this.ActiveControl = this.outputWindow.output; // Output window gets cursor

            int opened = 0;
            foreach (string filename in args)
            {
                string fullFilename = Path.GetFullPath(filename);
                // FIXME: if file exists, or allow create, if dir exists?
                OpenFile(fullFilename);
                opened++;
            }
            if (opened == 0)
            {
                NewFile();
            }

            this.ActiveControl = docManager.GetCurrentTabTextBox(); // tab text gets cursor
            // FIXME: make a general language changer
            this.languageName.Text = "Python";
            this.columnNumber.Text = "" + 1;
            this.lineNumber.Text = "" + 1;
        }

        void mainForm_KeyDown(object sender, KeyEventArgs e)
        {
            ShortcutKeyDispatcher dispatcher = new ShortcutKeyDispatcher(_controller);
            dispatcher.Dispatch(e);
        }

        private void execute_Click(object sender, System.EventArgs e)
        {
            _controller.ExecuteInThread();
        }

        public CodeBlock GetCodeBlock()
        {
            return docManager.GetCode();
        }

        public bool HasFileOpen
        {
            get { return docManager.HasFileOpen; }
        }

        public void OpenFile(IMainForm main_form, ActiveCodeFile file)
        {
            docManager.OpenFile(main_form, file);
            SetButtonsStatus();
        }

        public void OpenFile(string filename)
        {
            _controller.OpenFile(filename);
            SetButtonsStatus();
        }

        public void NewFile()
        {
            _controller.NewFile();
            SetButtonsStatus();
        }

        public void UpdateGUI(int col, int line)
        {
            //System.Console.WriteLine("UpdateGUI: {0}, {1}", col, line);
            this.columnNumber.Text = "" + col;
            this.lineNumber.Text = "" + line;
        }

        public void OpenFile()
        {
            _controller.NewFile();
            SetButtonsStatus();
        }

        public string CurrentActiveFileLocation()
        {
            return docManager.GetCurrentFile().Location;
        }

        public ActiveCodeFile GetCurrentActiveFile()
        {
            return docManager.GetCurrentFile();
        }

        public void SetSaveInformationForActiveFile(string location)
        {
            docManager.SetSaveInformationForActiveFile(location);
        }

        private void SetButtonsStatus()
        {
            bool enable = HasFileOpen;
            newToolStripButton.Enabled = true;
            newToolStripMenuItem.Enabled = true;
            openToolStripMenuItem.Enabled = true;
            openToolStripButton.Enabled = true;
            cutToolStripMenuItem.Enabled = enable;
            cutToolStripButton.Enabled = enable;
            copyToolStripMenuItem.Enabled = enable;
            copyToolStripButton.Enabled = enable;
            pasteToolStripMenuItem.Enabled = enable;
            pasteToolStripButton.Enabled = enable;
            saveToolStripButton.Enabled = enable;
            saveToolStripMenuItem.Enabled = enable;
            saveAsToolStripMenuItem.Enabled = enable;
            printToolStripMenuItem.Enabled = enable;
            printToolStripButton.Enabled = enable;
            executeToolStripMenuItem.Enabled = enable;
            executeToolStripButton.Enabled = enable;
            undoToolStripMenuItem.Enabled = enable;
            selectAllToolStripMenuItem.Enabled = enable;
        }

        public void Execute(string code)
        {
            // Interface to Shell
            outputWindow.textbox.DoCommand(code);
            commandTextBox.Focus();
            //SelectionStart = this.TextLength;
        }

        public void ExecuteFile(string filename) // FIXME: take language
        {
            outputWindow.textbox.DoFile(filename);
            commandTextBox.Focus();
        }

        public void SelectCommandShell()
        {
            commandTextBox.Focus();
        }


        #region Menu Items

        private void contentsToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            _controller.LaunchHelp();
        }

        private void aboutToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            _controller.DisplayAboutDialog();
        }

        private void exitToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            _controller.Exit();
        }

        private void newToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            Cursor current = Cursor.Current;
            Cursor.Current = Cursors.WaitCursor;
            _controller.NewFile();
            Cursor.Current = current;
        }

        private void saveToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            _controller.Save();
        }

        private void saveAsToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            _controller.SaveAs();
        }

        private void openToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            Cursor current = Cursor.Current;
            Cursor.Current = Cursors.WaitCursor;
            _controller.OpenFile();
            Cursor.Current = current;

        }

        private void undoToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
	    if (commandTextBox.ContainsFocus) {
		commandTextBox.Undo();
	    } else if (outputWindow.ContainsFocus) {
		outputWindow.textbox.consoleTextBox.Undo();
	    } else {
		GetNotNullIDEInput().Undo();
	    }
        }

        private DocumentInput GetNotNullIDEInput()
        {
            DocumentInput input = docManager.GetCurrentInput();
            if (input == null)
                return new DocumentInput(null);
            else
                return input;
        }

        private void cutToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
	    if (commandTextBox.ContainsFocus) {
		commandTextBox.Cut();
	    } else if (outputWindow.ContainsFocus) {
		// because read-only, copy not cut:
		outputWindow.textbox.consoleTextBox.Copy();
	    } else {
		GetNotNullIDEInput().Cut();
	    }
        }

        private void copyToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
	    if (commandTextBox.ContainsFocus) {
		commandTextBox.Copy();
	    } else if (outputWindow.ContainsFocus) {
		outputWindow.textbox.consoleTextBox.Copy();
	    } else {
		GetNotNullIDEInput().Copy();
	    }
        }

        private void pasteToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
	    if (commandTextBox.ContainsFocus) {
		commandTextBox.Paste();
	    } else if (outputWindow.ContainsFocus) {
		//outputWindow.textbox.consoleTextBox.Paste();
		//read-only
	    } else {
		GetNotNullIDEInput().Paste();
	    }
        }

        private void selectAllToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
	    if (commandTextBox.ContainsFocus) {
		commandTextBox.SelectAll();
	    } else if (outputWindow.ContainsFocus) {
		outputWindow.textbox.consoleTextBox.SelectAll();
	    } else {
		GetNotNullIDEInput().SelectAll();
	    }
        }

        private void optionsToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            OptionsDialog dialog = new OptionsDialog();
            if (dialog.ShowDialog() == DialogResult.OK)
            {
                UserSettings settings = CreateUserSettings(dialog);
                ApplicationOptions.SaveUserSettings(ApplicationOptions.GetIsolatedStorage(), settings);
                ApplyUserSettings(settings);
            }
        }

        private void ApplyUserSettings(UserSettings settings)
        {
            if (settings == null)
                return;

            outputWindow.GetOutput().Font = settings.UIFont;
            docManager.FontToUse = settings.UIFont;
        }

        private UserSettings CreateUserSettings(OptionsDialog dialog)
        {
            UserSettings settings = new UserSettings();
            settings.FontName = dialog.SelectedName();
            settings.FontSize = dialog.SelectedSize();

            return settings;
        }


        #endregion

        private void mainForm_FormClosing(object sender, FormClosingEventArgs e)
        {

        }

        private void outputWindow_Load(object sender, EventArgs e)
        {

        }

        private void toolStripStatusLabel1_Click(object sender, EventArgs e)
        {

        }

        private void toolStripStatusLabel3_Click(object sender, EventArgs e)
        {

        }

        private void fileManager1_Load(object sender, EventArgs e)
        {

        }

        private void mainContentSplit_SplitterMoved(object sender, SplitterEventArgs e)
        {

        }

        private void toolStripStatusLabel6_Click(object sender, EventArgs e)
        {

        }

        private void toolStripComboBox1_Click(object sender, EventArgs e)
        {

        }

        private void toolStripSplitButton1_ButtonClick(object sender, EventArgs e)
        {

        }

        private void statusStrip2_ItemClicked(object sender, ToolStripItemClickedEventArgs e)
        {

        }

        private void flowLayoutPanel1_Paint(object sender, PaintEventArgs e)
        {

        }

        private void tableLayoutPanel1_Paint(object sender, PaintEventArgs e)
        {

        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {
        }

        private void shellLanguageSelect1_Click(object sender, EventArgs e)
        {
            // FIXME: make general
            shellLanguageButton.Text = "Python";
            commandLabel.Text = "Python Mode";
            outputWindow.textbox.Prompt = "---- Python Mode ----";
            outputWindow.textbox.engine = outputWindow.textbox.environment.GetEngine("py");
            outputWindow.textbox.consoleTextBox.printPrompt();
            commandTextBox.Focus();
            shellLanguageSelect1.Checked = true;
            shellLanguageSelect2.Checked = false;
            shellLanguageSelect1.Checked = false;
        }

        private void shellLanguageSelect2_Click(object sender, EventArgs e)
        {
            // FIXME: make general
            shellLanguageButton.Text = "Ruby";
            commandLabel.Text = "Ruby Mode";
            outputWindow.textbox.Prompt = "---- Ruby Mode ----";
            outputWindow.textbox.engine = outputWindow.textbox.environment.GetEngine("rb");
            outputWindow.textbox.consoleTextBox.printPrompt();
            commandTextBox.Focus();
            shellLanguageSelect1.Checked = false;
            shellLanguageSelect2.Checked = true;
            shellLanguageSelect1.Checked = false;
        }

        private void shellLanguageSelect3_Click(object sender, EventArgs e)
        {
            // FIXME: make general
            shellLanguageButton.Text = "Scheme";
            commandLabel.Text = "Scheme Mode";
            outputWindow.textbox.Prompt = "---- Scheme Mode ----";
            //outputWindow.textbox.engine = outputWindow.textbox.environment.GetEngine("rb");
            outputWindow.textbox.consoleTextBox.printPrompt();
            commandTextBox.Focus();
            shellLanguageSelect1.Checked = false;
            shellLanguageSelect2.Checked = false;
            shellLanguageSelect1.Checked = true;
        }

        private void runButton_Click(object sender, EventArgs e)
        {
            string code = commandTextBox.Text;
            code = code.Trim();
            outputWindow.textbox.DoCommand(code);
            commandTextBox.Text = "";
            commandTextBox.Focus();
        }

        private void commandTextBox_KeyPress(object sender, KeyPressEventArgs e)
        {
            //System.Console.WriteLine("KeyPress: {0}", (int)e.KeyChar);
            if (((int)e.KeyChar) == 13 && !controlKeyDown)
            {
                outputWindow.textbox.DoCommand(commandTextBox.Text);
                commandTextBox.Text = "";
                commandTextBox.Focus();
                e.Handled = true;
            }
            // For Windows
            if (e.KeyChar == '\t') // tab
            {
                //System.Console.WriteLine("tab!");
                //int start = commandTextBox.SelectionStart;
                //commandTextBox.Text = commandTextBox.Text.Insert(start, "    ");
                //commandTextBox.SelectionStart = start + 4;
                e.Handled = true;
            }
        }

        // Mono bug
        private bool controlKeyDown = false;

        private void commandTextBox_KeyUp(object sender, System.Windows.Forms.KeyEventArgs e)
        {
            //System.Console.WriteLine("KeyUp: {0} {1}", e.KeyCode, e.Control);
            controlKeyDown = false;
        }

        private string tempCommandText = "";

        private void commandTextBox_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
        {
            //System.Console.WriteLine("KeyDown: {0} {1}", e.KeyCode, e.Control);

            System.Console.WriteLine("start temp = '{0}'", tempCommandText);
            if (e.KeyCode == System.Windows.Forms.Keys.Enter && e.Control)
            {
                // Mono bug work-around: doesn't handle the key
                controlKeyDown = true;
                //outputWindow.textbox.DoCommand(commandTextBox.Text);
                //commandTextBox.Text = "";
                //commandTextBox.Focus();
                //e.Handled = true;
            }
            else if (e.KeyCode == System.Windows.Forms.Keys.Enter)
            {
                // clear temp text
                tempCommandText = "";
            }
            else if (e.KeyCode == System.Windows.Forms.Keys.Down)
            {
                // If cursor is on the last line
                int currentLine = commandTextBox.GetLineFromCharIndex(commandTextBox.SelectionStart);
                int lastLine = commandTextBox.GetLineFromCharIndex(commandTextBox.TextLength);
                if (currentLine == lastLine)
                {
                    if (outputWindow.textbox.consoleTextBox.commandHistory.DoesNextCommandExist())
                    {
                        commandTextBox.Text = outputWindow.textbox.consoleTextBox.commandHistory.GetNextCommand();
                    }
                    else 
                    {
                        if (!commandTextBox.Text.Equals(""))
                        {
                            tempCommandText = commandTextBox.Text;
                        }
                        commandTextBox.Text = "";
                    }
                    e.Handled = true;
                }
            }
            else if (e.KeyCode == System.Windows.Forms.Keys.Up)
            {
                // If cursor is on the first line
#if MONO
	        int firstLine = commandTextBox.GetLineFromCharIndex(commandTextBox.SelectionStart) - 1;
#else
                int firstLine = commandTextBox.GetLineFromCharIndex(commandTextBox.SelectionStart);
#endif
                if (firstLine == 0)
                {
                    if (outputWindow.textbox.consoleTextBox.commandHistory.DoesPreviousCommandExist())
                    {
                        if (!outputWindow.textbox.consoleTextBox.commandHistory.DoesNextCommandExist())
                        {
                            if (!commandTextBox.Text.Equals(""))
                            {
                                tempCommandText = commandTextBox.Text;
                            }
                        }
                        commandTextBox.Text = outputWindow.textbox.consoleTextBox.commandHistory.GetPreviousCommand();
                        commandTextBox.Select(commandTextBox.TextLength, 0);
                    }
                    else
                    {
                        if (!tempCommandText.Equals(""))
                            commandTextBox.Text = tempCommandText;
                    }
                    e.Handled = true;
                }
            }
            System.Console.WriteLine("end temp = '{0}'", tempCommandText);
        }

        private void shellToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.SelectCommandShell();
        }

        private void newlineToolStripMenuItem_Click(object sender, EventArgs e)
        {
            // Emit <Ctrl+ENTER>
        }

        private void commandTextBox_TextChanged(object sender, EventArgs e)
        {

        }

        private void commandLabel_Click(object sender, EventArgs e)
        {

        }
    }
}
