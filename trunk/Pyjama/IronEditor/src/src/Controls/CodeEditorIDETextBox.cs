using System.Windows.Forms;
using Fireball.Windows.Forms;

namespace IronEditor.UI.WinForms.Controls
{
    public partial class CodeEditorIDETextBox : UserControl, IIDETextBox
    {
        public new event IDETextBoxEvent.TextChangedHandler TextChanged;

        private CodeEditorControl codeEditorControl;
        public CodeEditorIDETextBox(string fileExtension)
        {
            InitializeComponent();
            Dock = DockStyle.Fill;

            codeEditorControl = new CodeEditorControl();

            switch (fileExtension)
            {
                case ".cs":
                    codeEditorControl.Document.SyntaxFile = "SyntaxFiles\\CSharp.syn";
                    break;
                case ".py":
                    codeEditorControl.Document.SyntaxFile = "SyntaxFiles\\IronPython.syn";
                    break;
                default:
                    codeEditorControl.Document.SyntaxFile = "SyntaxFiles\\AutoIt.syn";
                    break;
            }



            codeEditorControl.Dock = DockStyle.Fill;
            codeEditorControl.Document.ModifiedChanged += Document_ModifiedChanged;
            Controls.Add(codeEditorControl);
        }

        public void Document_ModifiedChanged(object sender, System.EventArgs e)
        {
            if (TextChanged != null)
                TextChanged(this, e);
        }

        public string Code
        {
            get { return codeEditorControl.Document.Text; }
            set { codeEditorControl.Document.Text = value; }
        }

        public string FontName
        {
            get { return codeEditorControl.FontName; }
            set { codeEditorControl.FontName = value; }
        }

        public float FontSize
        {
            get { return codeEditorControl.FontSize; }
            set { codeEditorControl.FontSize = value; }
        }

        public string SelectedText
        {
            get { return codeEditorControl.Selection.Text; }
        }

        public void Undo()
        {
            codeEditorControl.Undo();
        }

        public void Cut()
        {
            codeEditorControl.Cut();
        }

        public void Copy()
        {
            codeEditorControl.Copy();
        }

        public void Paste()
        {
            codeEditorControl.Paste();
        }

        public void SelectAll()
        {
            codeEditorControl.SelectAll();
        }
    }


}