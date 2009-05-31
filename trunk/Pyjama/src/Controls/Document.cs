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
			// MAKE SURE THIS MATCHES KeyPress, below
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
        private Dictionary<string, Color> colors = new Dictionary<string, Color>();
        private Dictionary<string, Font> fonts = new Dictionary<string, Font>();
        public Dictionary<string, int> keywords;

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
            textBox.MouseClick += new MouseEventHandler(textBox_MouseClick);
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
            colors.Add("quote", Color.LightBlue);
            fonts.Add("quote", new Font("Courier New", 10, FontStyle.Regular));
            keywords = new Dictionary<string, int> {{ "and", 1}, {"del", 1}, {"for", 1}, 
                                                    {"is", 1}, {"raise", 1},
                                                    {"assert",1}, {"elif",1}, {"from",1}, 
                                                    {"lambda",1}, {"return",1},
                                                    {"break",1}, {"else",1}, {"global",1}, 
                                                    {"not",1}, {"try",1},
                                                    {"class",1}, {"except",1}, {"if",1}, 
                                                    {"or",1}, {"while",1},
                                                    {"continue",1}, {"exec",1}, {"import",1}, 
                                                    {"pass",1}, {"yield",1},
                                                    {"def",1}, {"finally",1}, {"in",1},
                                                    {"as",1}, {"with",1},
                                                    {"self",2}, {"print",2}, {"None",2}, 
                                                    {"True",2}, {"False",2} };

            // -----------------------------
            Controls.Add(textBox);
        }

        void textBox_MouseClick(object sender, MouseEventArgs e)
        {
            int col = (textBox.SelectionStart - textBox.GetFirstCharIndexOfCurrentLine() + 1);
            int line = (textBox.GetLineFromCharIndex(textBox.SelectionStart) + 1);
            MainForm.UpdateGUI(col, line);
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
		  // MAKE SURE THIS MATCHES OnKeyDown of MyRichTextBox
		  if (e.KeyChar == '\t') 
			// e.KeyChar == '\r') // tab or enter/return to eat
		  {
			e.Handled = true;
		  }
        }

	/*
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
	*/

        private static bool EndSymbol(char c)
        {
            return (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' ||
                    c == '(' || c == ')' || c == '.' || c == '-' || c == '/' || c == '\0' ||
                    c == ':' || c == '\\' || c == '*' || c == '@' || c == '%');
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
                if (m_rtb.Text[start] == '\n')
                {
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
            String line = m_rtb.Text.Substring(start, end - start) + '\0';
            // Backup the users current selection point.
            int selectionStart = m_rtb.SelectionStart;
            int selectionLength = m_rtb.SelectionLength;
            int index = start;
            int line_pos = 0;
            int mode = 0; // 0 start; 1 in token; 2 in double quote; 3 in quote
            int tokenStart = 0;
            string token;
            foreach (char c in line)
            {
                if (mode == 0)
                {
                    if (c == '"') {
                        mode = 2;
                        tokenStart = index;
                    } else if (c == '\'') {
                        mode = 3;
                        tokenStart = index;
                    } else if (c == '#')
                    {
                        m_rtb.SelectionStart = index;
                        m_rtb.SelectionLength = line.Length - line_pos;
                        m_rtb.SelectionColor = colors["comment"];
                        m_rtb.SelectionFont = fonts["comment"];
                        break; // done!
                    }
                    else if (!EndSymbol(c)) // start of token, number, or word
                    {
                        mode = 1;
                        tokenStart = index;
                    }
                } // else more token or quote
                else if (mode == 1) // in token
                {
                    if (EndSymbol(c)) // end of token
                    {
                        token = m_rtb.Text.Substring(tokenStart, index - tokenStart);
                        if (keywords.ContainsKey(token))
                        {
                            mode = 0;
                            int colorCode = keywords[token];
                            if (colorCode == 1)
                            {
                                m_rtb.SelectionStart = tokenStart;
                                m_rtb.SelectionLength = token.Length;
                                m_rtb.SelectionColor = colors["keyword"];
                                m_rtb.SelectionFont = fonts["keyword"];
                            }
                            else if (colorCode == 2)
                            {
                                m_rtb.SelectionStart = tokenStart;
                                m_rtb.SelectionLength = token.Length;
                                m_rtb.SelectionColor = colors["syntax"];
                                m_rtb.SelectionFont = fonts["syntax"];
                            }
                        }
                        else
                        {
                            mode = 0;
                            m_rtb.SelectionStart = tokenStart;
                            m_rtb.SelectionLength = token.Length;
                            m_rtb.SelectionColor = colors["default"];
                            m_rtb.SelectionFont = fonts["default"];
                        }
                    } // else still in token
                }
                else if (mode == 2) // in double quote
                {
                    if (c == '"' || c == '\0') // end of double quote
                    {
                        mode = 0;
                        if (c == '"')
                            token = m_rtb.Text.Substring(tokenStart, index - tokenStart + 1);
                        else
                            token = m_rtb.Text.Substring(tokenStart, index - tokenStart);
                        m_rtb.SelectionStart = tokenStart;
                        m_rtb.SelectionLength = token.Length;
                        m_rtb.SelectionColor = colors["quote"];
                        m_rtb.SelectionFont = fonts["quote"];
                    } // else still in double quote
                }
                else if (mode == 3) // in quote
                {
                    if (c == '\'' || c == '\0') // end of quote
                    {
                        mode = 0;
                        if (c == '\'')
                            token = m_rtb.Text.Substring(tokenStart, index - tokenStart + 1);
                        else
                            token = m_rtb.Text.Substring(tokenStart, index - tokenStart);
                        m_rtb.SelectionStart = tokenStart;
                        m_rtb.SelectionLength = token.Length;
                        m_rtb.SelectionColor = colors["quote"];
                        m_rtb.SelectionFont = fonts["quote"];
                    } // else still in quote
                }
                index++;
                line_pos++;
            }
            // Restore the users current selection point.    
            m_rtb.SelectionStart = selectionStart;
            m_rtb.SelectionLength = selectionLength;
            m_rtb.SelectionColor = colors["default"];
            m_rtb.SelectionFont = fonts["default"];
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
