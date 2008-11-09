using System.Windows.Forms;

namespace Pyjama
{
    public partial class OutputWindow : UserControl
    {
        public OutputWindow()
        {
            InitializeComponent();
        }

        public TextBox GetOutput()
        {
            return output;
        }

        private void controlTitle_Load(object sender, System.EventArgs e)
        {

        }
    }
}
