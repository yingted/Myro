using System.Windows.Forms;

namespace Pyjama
{
    public class DocumentPage : TabPage
    {
        public bool status = false;
        public DocumentInput Input { get; set; }
        public ActiveCodeFile ActiveFile { get; set; }
        public Document textBox;
        public IMainForm MainForm;

        public DocumentPage(IMainForm main_form, ActiveCodeFile file)
            : this(main_form, file.FileName) //file.FileExtension)
        {
	    InitializeComponent();
            UpdateFileName(file);
            ActiveFile = file;
            //this.ImageIndex = 0;
        }

        public DocumentPage(IMainForm main_form, string fileExtension)
        {
	    InitializeComponent();
            MainForm = main_form;
            Text = "NewFile";
            //System.Console.WriteLine("File Extension = ", fileExtension);
            CreateTextBox(fileExtension);
            //this.ImageIndex = 0;
        }

        public DocumentPage(IMainForm main_form)
        {
	        InitializeComponent();
            MainForm = main_form;
            Text = "NewFile";
            CreateTextBox(string.Empty); //File ext needs to be passed here
            //this.ImageIndex = 0;
        }

        private void CreateTextBox(string fileExtension)
        {
            //if (MonoEnvironment.IsRunningOnMono())
            //System.Console.WriteLine("FileExtension = ", fileExtension.ToString());
            //textBox = new Document(MainForm); //pass in file extension
            textBox = new Document(MainForm, fileExtension);
            //else
            //textBox = new CodeEditorIDETextBox(fileExtension);
            
            textBox.TextChanged += textBox_TextChanged;
            
            Input = new DocumentInput(textBox);
            Controls.Add(textBox as Control);
        }

        void textBox_TextChanged(object sender, System.EventArgs e)
        {
            ActiveFile.Unsaved = true;
            UpdateSaveStatus();
        }

        internal void UpdateFileName(ActiveCodeFile file)
        {
            //System.Console.WriteLine("DocumentPage.UpdateFileName to " + file.FileName);
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
            textBox.FormatAll();
            RemoveSaveStatus();
        }

        private void RemoveSaveStatus()
        {
            bool endsWith = Text.EndsWith(" *");
            if(endsWith)
            {
                Text = Text.Remove(Text.Length - 2, 2);                
            }
            textBox.FormatAll();
            System.Console.WriteLine("Active File = {0}", ActiveFile.FileName);
        }

        private void InitializeComponent()
        {
            this.SuspendLayout();
            // 
            // DocumentPage
            // 
            this.AllowDrop = true;
            this.ResumeLayout(false);

        }
    }

}