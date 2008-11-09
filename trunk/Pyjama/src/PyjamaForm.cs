using System;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
using System.IO.IsolatedStorage;
using System.Threading;
using Pyjama;
using Pyjama.Dialogs;

namespace Pyjama
{
    public partial class PyjamaForm : BaseForm, IMainForm
    {
        private PyjamaFormController _controller;
        bool need_newline = false;
        bool need_prompt = true;
        string prompt = ">>> ";
        Thread execute_thread;

        public PyjamaForm()
        {
            InitializeComponent();
            KeyDown += new KeyEventHandler(mainForm_KeyDown);
            
            _controller = new PyjamaFormController(this);

            Text = string.Format(Text, ApplicationInformation.Title(), ApplicationInformation.Version());

            SetButtonsStatus();
            ApplyUserSettings(ApplicationOptions.LoadUserSettings(ApplicationOptions.GetIsolatedStorage()));
            //this.ActiveControl = this.outputWindow.output; // Output window gets cursor
            this.ActiveControl = docManager.GetCurrentTabTextBox(); // tab text gets cursor
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
            // FIXME:
            // set GUI in thread run state
            // change cursor to busy
            // FIXME:
            // This won't work as you have to do some business with Invoke to coordinate:
            //    execute_thread = new Thread( new ThreadStart(_controller.Execute));
            //    execute_thread.Start();
            // For now, just do it directly:
            _controller.Execute();
        }

        public TextWriter GetOutputStream()
        {
            return new TextBoxWriter(outputWindow.GetOutput());
        }

        public CodeBlock GetCodeBlock()
        {
            return docManager.GetCode();
        }

        public void PrintPrompt()
        {
            if (need_newline)
                PrintConsoleMessage("\r\n");
            if (need_prompt)
                PrintConsoleMessage(prompt, Color.White);
            need_newline = false;
            need_prompt = false;
            outputWindow.output.SelectionStart = outputWindow.output.Text.Length;
            outputWindow.output.ScrollToCaret();
        }

        public void PrintLineConsoleMessage(string message)
        {
            outputWindow.output.Text += message + "\r\n";
            need_newline = false;
            need_prompt = true;
            // FIXME
            //outputWindow.output.SelectionStart = outputWindow.output.Text.Length;
            //outputWindow.output.SelectionLength = message.Length;
            //outputWindow.output.SelectionColor = Color.Red;
            // Make text a color:
            // Move to end:
            outputWindow.output.SelectionStart = outputWindow.output.Text.Length;
            outputWindow.output.ScrollToCaret();
        }

        public void PrintConsoleMessage(string message)
        {
            PrintConsoleMessage(message, Color.White);
        }

        public void PrintConsoleMessage(string message, Color color)
        {
            outputWindow.output.Text += message;
            // if not ending with newline
            need_newline = !(message.EndsWith("\r") | message.EndsWith("\n"));
            need_prompt = ! message.Equals("");
            // FIXME:
            //outputWindow.output.SelectionLength = message.Length;
            //outputWindow.output.SelectionColor = color;
            // Move to end:
            outputWindow.output.SelectionStart = outputWindow.output.Text.Length;
            outputWindow.output.ScrollToCaret();
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

        public void UpdateGUI(int col, int line)
        {
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

        public void ClearOutputStream()
        {
            outputWindow.GetOutput().Clear();
        }

        public void ClearOpenFiles()
        {
            docManager.Clear();
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
            GetNotNullIDEInput().Undo();
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
            GetNotNullIDEInput().Cut();
        }

        private void copyToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            GetNotNullIDEInput().Copy();
        }

        private void pasteToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            GetNotNullIDEInput().Paste();
        }

        private void selectAllToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            GetNotNullIDEInput().SelectAll();
        }

        private void optionsToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            OptionsDialog dialog = new OptionsDialog();
            if(dialog.ShowDialog() == DialogResult.OK)
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
            settings.FontName = dialog.SelectedFont();
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
    }
}
