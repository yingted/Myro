using System;
using System.IO;
using System.Windows.Forms;
using IronEditor.Engine;

namespace IronEditor.UI.WinForms.Dialogs
{
    public partial class NewProjectDialog : BaseDialog
    {
        private MainFormController _controller;
        public string ProjectType
        {
            get
            {
                return projectType.SelectedItem.ToString();
            }
        }

        public string ProjectPath
        {
            get
            {
                return projectPath.Text;
            }
        }

        public string SelectedLanguage
        {
            get
            {
                return languagesCombo.SelectedItem.ToString();
            }
        }

        public NewProjectDialog(MainFormController controller)
        {
            InitializeComponent();
            _controller = controller;

            foreach (LanguageSettings language in _controller.GetLanguages())
            {
                languagesCombo.Items.Add(language.Language);
            }

            languagesCombo.SelectedIndex = 0;
            projectType.SelectedIndex = 0;
        }

        private void projectType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (Equals(projectType.SelectedItem.ToString(), "Project"))
                projectSettings.Visible = true;
            else
                projectSettings.Visible = false;
        }

        private void projectBrowse_Click(object sender, EventArgs e)
        {
            FolderBrowserDialog folderBrowserDialog = new FolderBrowserDialog();
            folderBrowserDialog.SelectedPath = Directory.GetCurrentDirectory();
            if (folderBrowserDialog.ShowDialog() == DialogResult.OK)
                projectPath.Text = folderBrowserDialog.SelectedPath;
        }

        private void cancel_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
        }

        private void Create_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.OK;
        }

    }
}
