using System.IO;
using System.Text;
using System.Windows.Forms;

namespace IronEditor.UI.WinForms
{
    internal sealed class TextBoxWriter : TextWriter
    {
        private TextBox _textBox;

        internal TextBoxWriter(TextBox textBox)
        {
            _textBox = textBox;
        }

        public override Encoding Encoding
        {
            get { return Encoding.Default; }
        }

        public override void Write(string value)
        {
            //if (value != NewLine)
            //    _textBox.AppendText(">");

            _textBox.AppendText(value.Replace("\n", NewLine));
        }

        public override void WriteLine(string value)
        {
            Write(value);
            Write(NewLine);
        }
    }
}