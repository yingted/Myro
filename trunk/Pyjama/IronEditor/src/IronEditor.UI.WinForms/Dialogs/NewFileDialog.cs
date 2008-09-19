using System;
using System.Windows.Forms;

namespace IronEditor.UI.WinForms.Dialogs
{
    public partial class NewFileDialog : BaseDialog
    {
        public NewFileDialog()
        {
            InitializeComponent();
        }

        public string FileName
        {
            get
            {
                return nameLabel.Text;
            }
            set
            {
                nameLabel.Text = value;
            }
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
