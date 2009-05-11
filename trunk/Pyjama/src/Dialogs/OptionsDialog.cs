using System.Windows.Forms;
using Pyjama.Dialogs.OptionFrames;

namespace Pyjama.Dialogs
{
    public partial class OptionsDialog : BaseDialog
    {
        private FontControl fontControl;
        private FontControl shellFontControl;

        public float SelectedSize()
        {
            return fontControl.GetSize();
        }

        public string SelectedName()
        {
            return fontControl.GetFont();
        }

        public float ShellFontSize()
        {
            return shellFontControl.GetSize();
        }

        public string ShellFont()
        {
            return shellFontControl.GetFont();
        }

        public OptionsDialog()
        {
            InitializeComponent();

            UserSettings settings = ApplicationOptions.LoadUserSettings(ApplicationOptions.GetIsolatedStorage());
            fontControl = new FontControl(settings);
            fontControl.Dock = DockStyle.Top;

            shellFontControl = new FontControl(settings);
            shellFontControl.Dock = DockStyle.Top;

            TreeNode fontNode = new TreeNode();
            fontNode.Text = "Editor Font";
            fontNode.Tag = fontControl;

            TreeNode shellFontNode = new TreeNode();
            shellFontNode.Text = "Shell Font";
            shellFontNode.Tag = shellFontControl;

            optionsTree.Nodes.Add(fontNode);
            optionsTree.Nodes.Add(shellFontNode);
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

        private void panel3_Paint(object sender, PaintEventArgs e)
        {

        }
    }
}
