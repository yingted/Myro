using System.Windows.Forms;

namespace IronEditor.UI.WinForms.Controls
{
    public class IDETab : TabPage
    {
    
        public IDEInput Input { get; set; }
        public ActiveCodeFile ActiveFile { get; set; }
        public StandardIDETextBox textBox;
        public IMainForm MainForm;

        public IDETab(IMainForm main_form, ActiveCodeFile file)
            : this(main_form, file.FileExtension)
        {
            UpdateFileName(file);
            ActiveFile = file;
        }

        public IDETab(IMainForm main_form, string fileExtension)
        {
            MainForm = main_form;
            Text = "NewFile";
            CreateTextBox(fileExtension);
        }

        public IDETab(IMainForm main_form)
        {
            MainForm = main_form;
            Text = "NewFile";
            CreateTextBox(string.Empty);
        }

        private void CreateTextBox(string fileExtension)
        {
            //if (MonoEnvironment.IsRunningOnMono())
            textBox = new StandardIDETextBox(MainForm);
            //else
            //textBox = new CodeEditorIDETextBox(fileExtension);

            textBox.TextChanged += textBox_TextChanged;
            Input = new IDEInput(textBox);
            Controls.Add(textBox as Control);
        }

        void textBox_TextChanged(object sender, System.EventArgs e)
        {
            ActiveFile.Unsaved = true;
            UpdateSaveStatus();
        }

        internal void UpdateFileName(ActiveCodeFile file)
        {
            //System.Console.WriteLine("IDETab.UpdateFileName to " + file.FileName);
            Text = file.FileName;
        }

        public void UpdateSaveStatus()
        {
            bool endsWith = Text.EndsWith(" *");
            if(endsWith && ActiveFile.Unsaved)
                return;

            if (!endsWith && ActiveFile.Unsaved)
                Text = Text + " *";
            else
                RemoveSaveStatus();
        }

        public void SetInitialText(string text)
        {
            Input.Text = text;
            RemoveSaveStatus();
            
        }

        private void RemoveSaveStatus()
        {
            bool endsWith = Text.EndsWith(" *");
            if(endsWith)
            {
                Text = Text.Remove(Text.Length - 2, 2);                
            }
        }

        private void InitializeComponent()
        {
            this.SuspendLayout();
            this.ResumeLayout(false);

        }
    }
}