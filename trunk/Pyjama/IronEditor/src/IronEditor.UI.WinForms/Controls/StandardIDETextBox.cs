using System;
using System.Drawing;
using System.Windows.Forms;

namespace IronEditor.UI.WinForms.Controls
{
    public partial class StandardIDETextBox : UserControl, IIDETextBox
    {
        public new event IDETextBoxEvent.TextChangedHandler TextChanged;
        private IMainForm MainForm;
        private TextBox textBox;

        public StandardIDETextBox(IMainForm main_form)
        {
            MainForm = main_form;
            InitializeComponent();
            this.Dock = DockStyle.Fill;
            textBox = new TextBox();
            textBox.Multiline = true;
            textBox.AcceptsReturn = true;
            textBox.AcceptsTab = true;
            textBox.Dock = DockStyle.Fill;
            textBox.KeyPress += new KeyPressEventHandler(textBox_KeyPress);
            textBox.KeyUp += new KeyEventHandler(textBox_KeyUp);
            textBox.TextChanged += textbox_TextChanged;
            textBox.Font = new Font("Courier New", 10); 
            Controls.Add(textBox);
        }

        void textBox_KeyUp(object sender, KeyEventArgs e)
        {
            // Do things when a key goes down  
            // FIXME: this can be wrong when selecting
            int col = (textBox.SelectionStart - textBox.GetFirstCharIndexOfCurrentLine() + 1);
            int line = (textBox.GetLineFromCharIndex(textBox.SelectionStart) + 1);
            MainForm.UpdateGUI(col, line);
        }

        void textBox_KeyPress(object sender, KeyPressEventArgs e)
        {
            // Add ability to handle newlines after line ending with colon
            if (e.KeyChar == '\t') // tab
            {
                int start = textBox.SelectionStart;
                textBox.Text = textBox.Text.Insert(start, "    ");
                textBox.SelectionStart = start + 4;
                e.Handled = true;
            }
        }

        public int CurrentColumn
        {
            get {return (textBox.SelectionStart - textBox.GetFirstCharIndexOfCurrentLine() + 1);}
        }

        public int CurrentLine
        {
            get { return (textBox.GetLineFromCharIndex(textBox.SelectionStart) + 1); }
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
