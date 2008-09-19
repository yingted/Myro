using System.Diagnostics;
using System.Windows.Forms;
using IronEditor.Engine;

namespace IronEditor.UI.WinForms.Dialogs
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

        private void blogLink_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            Process.Start("http://blog.BenHall.me.uk");
        }

        private void urlHomepage_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            Process.Start("http://www.IronEditor.net");
        }
    }
}
