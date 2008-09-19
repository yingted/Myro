using System.Windows.Forms;
using IronEditor.UI.WinForms.Controls;

namespace IronEditor.Engine
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
