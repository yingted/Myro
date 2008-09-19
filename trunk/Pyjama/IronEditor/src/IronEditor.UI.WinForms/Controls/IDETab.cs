using System.Windows.Forms;
using IronEditor.Engine;

namespace IronEditor.UI.WinForms.Controls
{
    public class IDETab : TabPage
    {
    
        public IDEInput Input { get; set; }
        public ActiveCodeFile ActiveFile { get; set; }

        public IDETab(ActiveCodeFile file)
            : this(file.FileExtension)
        {
            UpdateFileName(file);
            ActiveFile = file;
        }

        public IDETab(string fileExtension)
        {
            Text = "NewFile";
            CreateTextBox(fileExtension);
        }

        public IDETab()
        {
            Text = "NewFile";
            CreateTextBox(string.Empty);
        }

        private void CreateTextBox(string fileExtension)
        {
            IIDETextBox textBox;
            
            //if (MonoEnvironment.IsRunningOnMono())
            textBox = new StandardIDETextBox();
            //else
            //textBox = new CodeEditorIDETextBox(fileExtension);

            textBox.TextChanged += textBox_TextChanged;
            Input = new IDEInput(textBox);
            Controls.Add(textBox as Control);
        }

        internal void UpdateFileName(ActiveCodeFile file)
        {
            Text = file.FileName;
        }

        void textBox_TextChanged(object sender, System.EventArgs e)
        {
            ActiveFile.Unsaved = true;
            UpdateSaveStatus();
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