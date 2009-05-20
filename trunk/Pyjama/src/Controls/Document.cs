using System;
using System.Drawing;
using System.Windows.Forms;

namespace Pyjama
{

  public class MyRichTextBox: RichTextBox {
    // Document Typing/Display Widget
	protected override void OnKeyDown(KeyEventArgs e)
	{
	  //System.Console.WriteLine("OnKeyDown?");
	  if (e.KeyData == Keys.Tab)
	    {
	      //System.Console.WriteLine("OnKeyDown!");
	      this.SelectedText = "    ";                
	      e.Handled = true;
	    }
	  else
	    {
	      base.OnKeyDown(e);
	    }
	}
	
	/*
	protected override bool IsInputKey(Keys keyData)
	{
	  // To have [TAB, Return, Escape] keys raise the KeyDown
	  // event, you must override the IsInputKey method in each
	  // control on your form. The code for the override of the
	  // IsInputKey would need to determine if one of the special
	  // keys is pressed and return a value of true.
	  System.Console.WriteLine("IsInputKey?");
	  if (keyData == Keys.Tab)
	    {
	      System.Console.WriteLine("IsInputKey!");
	      return true;
	    }
	  else
	    {
	      return base.IsInputKey(keyData);
	    }
	}
	*/
  }

    public partial class Document : UserControl, IDocument
    {
        public new event Document.TextChangedHandler TextChanged;
        private IMainForm MainForm;
        private RichTextBox textBox;

        public delegate void TextChangedHandler(object sender, EventArgs e);

        public Document(IMainForm main_form)
        {
            MainForm = main_form;
            InitializeComponent();
            this.Dock = DockStyle.Fill;
            textBox = new MyRichTextBox();
            textBox.Multiline = true;
            //textBox.AcceptsReturn = true;
            textBox.AcceptsTab = true;
            textBox.Dock = DockStyle.Fill;
            textBox.KeyPress += new KeyPressEventHandler(textBox_KeyPress);
            textBox.KeyUp += new KeyEventHandler(textBox_KeyUp);
            textBox.TextChanged += textbox_TextChanged;
            textBox.Font = new Font("Courier New", 10);
            textBox.WordWrap = false;
            Controls.Add(textBox);
        }

        void textBox_KeyUp(object sender, KeyEventArgs e)
        {
            // Do things when a key goes down  
            // FIXME: this can be wrong when selecting
	    System.Console.WriteLine("textBox_KeyUp: {0} {1} handled: {2}", 
				     e.KeyCode, e.Control, e.Handled);
            int col = (textBox.SelectionStart - textBox.GetFirstCharIndexOfCurrentLine() + 1);
            int line = (textBox.GetLineFromCharIndex(textBox.SelectionStart) + 1);
            MainForm.UpdateGUI(col, line);
        }

        void textBox_KeyPress(object sender, KeyPressEventArgs e)
        {
	    System.Console.WriteLine("Document.textBox_KeyPress: {0}, handled: {1}", 
				     (int)e.KeyChar,
				     e.Handled);
            // Add ability to handle newlines after line ending with colon
            if (e.KeyChar == '\t') // tab
            {
	          System.Console.WriteLine("tab!");
                //int start = textBox.SelectionStart;
                //textBox.Text = textBox.Text.Insert(start, "    ");
                //textBox.SelectionStart = start + 4;
                e.Handled = true;
            }
        }

        void textbox_TextChanged(object sender, EventArgs e)
        {
            if (TextChanged != null)
                TextChanged(this, e);
        }

        public int CurrentColumn
        {
            get {return (textBox.SelectionStart - textBox.GetFirstCharIndexOfCurrentLine() + 1);}
        }

        public int CurrentLine
        {
            get { return (textBox.GetLineFromCharIndex(textBox.SelectionStart) + 1); }
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

        private void Document_Load(object sender, EventArgs e)
        {

        }

    }
}
