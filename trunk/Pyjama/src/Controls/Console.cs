using System.Windows.Forms;

namespace Pyjama
{
    public partial class Console : UserControl
    {
        public Console()
        {
            InitializeComponent();
        }

        public RichTextBox GetOutput()
        {
            return output;
        }

        private void controlTitle_Load(object sender, System.EventArgs e)
        {

        }

        private void output_TextChanged(object sender, System.EventArgs e)
        {

        }
    }
}
