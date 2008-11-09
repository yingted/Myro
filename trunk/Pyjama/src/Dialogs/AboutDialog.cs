using System.Diagnostics;
using System.Windows.Forms;

namespace Pyjama.Dialogs
{
    public partial class AboutDialog : BaseDialog
    {
        public AboutDialog()
        {
            InitializeComponent();
            version.Text = ApplicationInformation.Version();
            Text = string.Format(Text, ApplicationInformation.Title());
            title.Text = string.Format(title.Text, ApplicationInformation.Title());
        }

        private void urlHomepage_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            Process.Start("http://PyjamaProject.org/");
        }
    }
}
