using System;
using System.Drawing;
using System.Windows.Forms;

namespace IronEditor.UI.WinForms.Controls
{
    public partial class StandardIDETextBox : UserControl, IIDETextBox
    {
        public new event IDETextBoxEvent.TextChangedHandler TextChanged;

        private TextBox textBox;

        public StandardIDETextBox()
        {
            InitializeComponent();
            this.Dock = DockStyle.Fill;

            textBox = new TextBox();
            textBox.Multiline = true;
            textBox.Dock = DockStyle.Fill;
            textBox.TextChanged += textbox_TextChanged;
            Controls.Add(textBox);
        }

        void textbox_TextChanged(object sender, EventArgs e)
        {
            if (TextChanged != null)
                TextChanged(this, e);
        }

        public string Code
        {
            get { return textBox.Text; }
            set { textBox.Text = value; }
        }

        public string FontName
        {
            get { return textBox.Font.FontFamily.Name; }
            set { textBox.Font = new Font(value, FontSize); }
        }

        public float FontSize
        {
            get { return textBox.Font.Size; }
            set { textBox.Font = new Font(FontName, value); }
        }

        public string SelectedText
        {
            get { return textBox.SelectedText; }
        }

        public void Undo()
        {
            textBox.Undo();
        }

        public void Cut()
        {
            textBox.Cut();
        }

        public void Copy()
        {
            textBox.Copy();
        }

        public void Paste()
        {
            textBox.Paste();
        }

        public void SelectAll()
        {
            textBox.SelectAll();
        }

    }
}
