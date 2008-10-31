using System;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
using System.IO.IsolatedStorage;
using IronEditor.UI.WinForms.Controls;
using IronEditor.UI.WinForms.Dialogs;

namespace IronEditor.UI.WinForms
{
    public partial class mainForm : BaseForm, IMainForm
    {
        private MainFormController _controller;

        public mainForm()
        {
            InitializeComponent();
            KeyDown += new KeyEventHandler(mainForm_KeyDown);
            
            _controller = new MainFormController(this);

            Text = string.Format(Text, ApplicationInformation.Title(), ApplicationInformation.Version());

            SetButtonsStatus();
            ApplyUserSettings(ApplicationOptions.LoadUserSettings(ApplicationOptions.GetIsolatedStorage()));
        }

        void mainForm_KeyDown(object sender, KeyEventArgs e)
        {
            ShortcutKeyDispatcher dispatcher = new ShortcutKeyDispatcher(_controller);
            dispatcher.Dispatch(e);
        }

        private void execute_Click(object sender, System.EventArgs e)
        {
            _controller.Execute();
        }

        public TextWriter GetOutputStream()
        {
            return new TextBoxWriter(outputWindow.GetOutput());
        }

        public CodeBlock GetCodeBlock()
        {
            return fileManager1.GetCode();
        }

        public void PrintConsoleMessage(string message)
        {
            System.Console.WriteLine("Message: " + message);
            outputWindow.output.Text += message + "\r\n";
        }

        public bool HasFileOpen
        {
            get { return fileManager1.HasFileOpen; }
        }

        public void OpenFile(ActiveCodeFile file)
        {
            fileManager1.OpenFile(file);
            SetButtonsStatus();
        }

        public void OpenFile()
        {
            _controller.NewFile();
            SetButtonsStatus();
        }

        public string CurrentActiveFileLocation()
        {
            return fileManager1.GetCurrentFile().Location;
        }

        public void ClearOutputStream()
        {
            outputWindow.GetOutput().Clear();
        }

        public void ClearOpenFiles()
        {
            fileManager1.Clear();
        }

        public ActiveCodeFile GetCurrentActiveFile()
        {
            return fileManager1.GetCurrentFile();
        }

        public void SetSaveInformationForActiveFile(string location)
        {
            fileManager1.SetSaveInformationForActiveFile(location);
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

        private IDEInput GetNotNullIDEInput()
        {
            IDEInput input = fileManager1.GetCurrentInput();
            if (input == null)
                return new IDEInput(null);
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
            fileManager1.FontToUse = settings.UIFont;
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

        private void launchConsoleToolStripButton_Click(object sender, System.EventArgs e)
        {
            _controller.LaunchConsole();
        }

        private void launchConsoleToolStripMenuItem_Click(object sender, System.EventArgs e)
        {
            _controller.LaunchConsole();

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
