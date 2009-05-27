using System;
using System.Drawing;
using System.Windows.Forms;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace Pyjama
{

    public class MyRichTextBox : RichTextBox
    {
	/*
	public MyRichTextBox() {
	    // Double buffer
	    this.SetStyle(ControlStyles.UserPaint, true);
            this.SetStyle(ControlStyles.DoubleBuffer, true);
            this.SetStyle(ControlStyles.ResizeRedraw, true);
            this.SetStyle(ControlStyles.AllPaintingInWmPaint, true);
	}
	*/

	/*
	protected override void WndProc(ref Message m) {
	    if ((m.Msg != 0x2111) || ((((uint)m.WParam >> 16)
				       & 0xFFFF) != 768))
		base.WndProc(ref m);
	} 
	*/

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
            // FIXME: if the previous line is unfinished
            /*
            else if (e.KeyData == Keys.Enter)
            {
                this.SelectedText = "\n    ";
                e.Handled = true;
	        }
             */
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
        private MyRichTextBox textBox;
        private Dictionary<string,Color> colors = new Dictionary<string,Color>();
        private Dictionary<string,Font> fonts = new Dictionary<string,Font>();
        public String[] keywords;
        public String[] syntax;

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
            // Resue fonts on formatting:
            colors.Add("default", Color.Black);
            fonts.Add("default", new Font("Courier New", 10, FontStyle.Regular));
            colors.Add("syntax", Color.Red);
            fonts.Add("syntax", new Font("Courier New", 10, FontStyle.Regular));
            colors.Add("comment", Color.LightGreen);
            fonts.Add("comment", new Font("Courier New", 10, FontStyle.Regular));
            colors.Add("keyword", Color.Blue);
            fonts.Add("keyword", new Font("Courier New", 10, FontStyle.Bold));
            keywords = new string[] { "and", "del", "for", "is", "raise",
                                      "assert", "elif", "from", "lambda", "return",
                                      "break", "else", "global", "not", "try",
                                      "class", "except", "if", "or", "while",
                                      "continue", "exec", "import", "pass", "yield",
                                      "def", "finally", "in",
                                      "as", "with"};
	    syntax = new string[] { "self", "print", "None", "True", "False"};

            // -----------------------------
            Controls.Add(textBox);
        }

        void textBox_KeyUp(object sender, KeyEventArgs e)
        {
            // Do things when a key goes down  
            // FIXME: this can be wrong when selecting
            //System.Console.WriteLine("textBox_KeyUp: {0} {1} handled: {2}", 
            //	     e.KeyCode, e.Control, e.Handled);
            int col = (textBox.SelectionStart - textBox.GetFirstCharIndexOfCurrentLine() + 1);
            int line = (textBox.GetLineFromCharIndex(textBox.SelectionStart) + 1);
            MainForm.UpdateGUI(col, line);
        }

        void textBox_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '\t' || e.KeyChar == '\r') // tab or enter/return to eat
            {
                e.Handled = true;
            }
        }

        void Parse(string text)
        {
            // Foreach line in input,
            // identify key words and format them when adding to the rich text box.
            Regex r = new Regex("\\n");
            String[] lines = r.Split(text);
            foreach (string l in lines)
            {
                ParseLine(l);
            }
        }

        void ParseLine(string line)
        {
            RichTextBox m_rtb = textBox;

            Regex r = new Regex("([ \\t{}();])");
            String[] tokens = r.Split(line);
            foreach (string token in tokens)
            {
                // Set the token's default color and font.
                m_rtb.SelectionColor = colors["default"];
                m_rtb.SelectionFont = fonts["default"];
                // Check for a comment.
                if (token.StartsWith("#"))
                {
                    // Find the start of the comment and then extract the whole comment.
                    int index = line.IndexOf("#");
                    string comment = line.Substring(index, line.Length - index);
                    m_rtb.SelectionColor = colors["comment"];
                    m_rtb.SelectionFont = fonts["comment"];
                    m_rtb.SelectedText = comment;
                    break;
                }
                // Check whether the token is a keyword. 
                for (int i = 0; i < keywords.Length; i++)
                {
                    if (keywords[i] == token)
                    {
                        // Apply alternative color and font to highlight keyword.
                        m_rtb.SelectionColor = colors["keyword"];
                        m_rtb.SelectionFont = fonts["keyword"];
                        break;
                    }
                }
                // Check whether the token is a special syntax
                for (int i = 0; i < syntax.Length; i++)
                {
                    if (syntax[i] == token)
                    {
                        // Apply alternative color and font to highlight keyword.
                        m_rtb.SelectionColor = colors["syntax"];
                        m_rtb.SelectionFont = fonts["syntax"];
                        break;
                    }
                }
                m_rtb.SelectedText = token;
            }
            m_rtb.SelectedText = "\n";
        } 

        void textbox_TextChanged(object sender, EventArgs e)
        {
            // Modify signal, for "*" and mark dirty:
            if (TextChanged != null)
                TextChanged(this, e);
            MyRichTextBox m_rtb = textBox;
            // Calculate the starting position of the current line.
            int start = 0, end = 0;
            for (start = m_rtb.SelectionStart - 1; start > 0; start--)
            {
                if (m_rtb.Text[start] == '\n') {
		    start++; 
		    break; 
		}
            }
            // Calculate the end position of the current line.
            for (end = m_rtb.SelectionStart; end < m_rtb.Text.Length; end++)
            {
                if (m_rtb.Text[end] == '\n') 
		    break;
            }
	    start = start < 0 ? 0 : start;
	    //System.Console.WriteLine("start={0}, stop={1}", start, end);
            // Extract the current line that is being edited.
	    String line = m_rtb.Text.Substring(start, end - start);
            // Backup the users current selection point.
            int selectionStart = m_rtb.SelectionStart;
            int selectionLength = m_rtb.SelectionLength;
            // Split the line into tokens.
            Regex r = new Regex("([ \\t{}();:,.])");
            string[] tokens = r.Split(line);
            int index = start;
            foreach (string token in tokens)
            {
		if (token.Trim() == "") {
		    index += token.Length;
		    continue;
		}
                // Set the token's default color and font.
                m_rtb.SelectionStart = index;
                m_rtb.SelectionLength = token.Length;
                m_rtb.SelectionColor = colors["default"];
                m_rtb.SelectionFont = fonts["default"];
                // Check for a comment.
                if (token.StartsWith("#"))
                {
                    // Find the start of the comment and then extract the whole comment.
                    int length = line.Length - (index - start);
                    string commentText = m_rtb.Text.Substring(index, length);
                    m_rtb.SelectionStart = index;
                    m_rtb.SelectionLength = length;
                    m_rtb.SelectionColor = colors["comment"];
                    m_rtb.SelectionFont = fonts["comment"];
                    break;
                }
                else if (token.StartsWith("\""))
                {
                    // Find the start of the comment and then extract the whole comment.
                    int length = line.Length - (index - start);
                    string commentText = m_rtb.Text.Substring(index, length);
                    m_rtb.SelectionStart = index;
                    m_rtb.SelectionLength = length;
                    m_rtb.SelectionColor = colors["syntax"];
                    m_rtb.SelectionFont = fonts["syntax"];
                    break;
                }
                // Check whether the token is a keyword. 
                for (int i = 0; i < keywords.Length; i++)
                {
                    if (token == keywords[i])
                    {
                        // Apply alternative color and font to highlight keyword.        
                        m_rtb.SelectionColor = colors["keyword"];
                        m_rtb.SelectionFont = fonts["keyword"];
                        break;
                    }
                }
                // Check whether the token is a special syntax
                for (int i = 0; i < syntax.Length; i++)
		    {
                    if (syntax[i] == token)
                    {
                        // Apply alternative color and font to highlight keyword.
                        m_rtb.SelectionColor = colors["syntax"];
                        m_rtb.SelectionFont = fonts["syntax"];
                        break;
                    }
                }
                index += token.Length;
            }
            // Restore the users current selection point.    
            m_rtb.SelectionStart = selectionStart;
            m_rtb.SelectionLength = selectionLength;
        }

        public int CurrentColumn
        {
            get { return (textBox.SelectionStart - textBox.GetFirstCharIndexOfCurrentLine() + 1); }
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
