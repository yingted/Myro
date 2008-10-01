using System.Windows.Forms;

namespace IronEditor.UI.WinForms.Controls
{
    public class CodeBlock
    {
        public string Language { get; set; }
        public IDEInput Code { get; set; }

        public string GetCodeToExecute()
        {
            if (Code.SelectedText.Length > 0)
                return Code.SelectedText;
            else
                return Code.Text;
        }
    }
}
