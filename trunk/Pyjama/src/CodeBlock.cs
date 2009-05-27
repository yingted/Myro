using System.Windows.Forms;

namespace Pyjama
{
    public class CodeBlock
    {
        public string Language { get; set; }
        public DocumentInput Code { get; set; }

        public bool IsSnippet()
        {
            return (Code.SelectedText.Length > 0);
        }

        public string GetCodeToExecute()
        {
            if (Code.SelectedText.Length > 0)
                return Code.SelectedText;
            else
                return Code.Text;
        }
    }
}
