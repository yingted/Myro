using System.Windows.Forms;

namespace Pyjama
{
    public partial class Console : UserControl
    {
        public Console()
        {
            InitializeComponent();
        }

        public UIIronTextBox.IronTextBoxControl GetOutput()
        {
            return textbox;
        }

        private void output_TextChanged(object sender, System.EventArgs e)
        {

        }

        private void textbox_Load(object sender, System.EventArgs e)
        {

        }
    }
}
