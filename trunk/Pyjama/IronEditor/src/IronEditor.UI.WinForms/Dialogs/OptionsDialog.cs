using System.Windows.Forms;
using IronEditor.UI.WinForms.Dialogs.OptionFrames;

namespace IronEditor.UI.WinForms.Dialogs
{
    public partial class OptionsDialog : BaseDialog
    {
        private FontControl fontControl;
        public float SelectedSize()
        {
            return fontControl.GetSize();
        }

        public string SelectedFont()
        {
            return fontControl.GetFont();
        }

        public OptionsDialog()
        {
            InitializeComponent();

            UserSettings settings = ApplicationOptions.LoadUserSettings(ApplicationOptions.GetIsolatedStorage());
            fontControl = new FontControl(settings);
            fontControl.Dock = DockStyle.Top;


            TreeNode fontNode = new TreeNode();
            fontNode.Text = "Font";
            fontNode.Tag = fontControl;

            optionsTree.Nodes.Add(fontNode);
        }

 

        private void optionsTree_AfterSelect(object sender, TreeViewEventArgs e)
        {
            optionPanel.Controls.Clear();
            optionPanel.Controls.Add(e.Node.Tag as Control);
        }

        private void save_Click(object sender, System.EventArgs e)
        {
            DialogResult = DialogResult.OK;
        }

        private void cancel_Click(object sender, System.EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
        }
    }
}
