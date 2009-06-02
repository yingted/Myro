using System;
using System.Drawing;
using System.Windows.Forms;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace Pyjama
{

    public class MyRichTextBox : RichTextBox
    {
        public bool lockUpdate = false;
        
        protected override void WndProc(ref Message m) {
            if (lockUpdate)
            {
                if (m.Msg == 15) // paint
                    return;
            }
          try
          {
            base.WndProc(ref m);
		  } catch {
			System.Console.WriteLine("Mono bug caught!");
			string oldRtf = Rtf;
			int oldSelectionStart = SelectionStart;
			int oldSelectionLength = SelectionLength;
			Clear();
			Rtf = oldRtf;
			SelectionStart = oldSelectionStart;
			SelectionLength = oldSelectionLength;
		  }
        } 
		
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
        public MyRichTextBox textBox;
        private Dictionary<string, Color> colors = new Dictionary<string, Color>();
        private Dictionary<string, Font> fonts = new Dictionary<string, Font>();
        public Dictionary<string, int> keywords;

        public delegate void TextChangedHandler(object sender, EventArgs e);
        int mode = 0; // used in parsing richtext
        int[] last_mode = new int[10000];

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
            colors.Add("comment", Color.DarkGreen);
            fonts.Add("comment", new Font("Courier New", 10, FontStyle.Regular));
            colors.Add("keyword", Color.Blue);
            fonts.Add("keyword", new Font("Courier New", 10, FontStyle.Bold));
            colors.Add("quote", Color.DarkBlue);
            fonts.Add("quote", new Font("Courier New", 10, FontStyle.Regular));
            colors.Add("doublequote", Color.DarkGreen);
            fonts.Add("doublequote", new Font("Courier New", 10, FontStyle.Regular));
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
            UpdateGUI();
        }

        public void UpdateGUI()
        {
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
            int lineno = textBox.GetLineFromCharIndex(textBox.GetFirstCharIndexOfCurrentLine());
            textBox.lockUpdate = true;
            if (lineno == 0)
                mode = 0; // start out in no mode
            else 
                mode = last_mode[lineno - 1]; // else get last left off mode
            for (int i = lineno; i < lineno + 30; i++)
            {
                FormatLine(i);
                if (mode == 0) break;
            }
            textBox.lockUpdate = false;
        }

        public void FormatAll()
        {
            last_mode = new int[10000]; // reset to zeros
            textBox.lockUpdate = true;
            mode = 0; // start out in no mode
            // FIXME: need to format only the visible
            for (int i = 0; i < 30; i++)
            {
                FormatLine(i);
            }
            textBox.lockUpdate = false;
        }

        public void FormatLine(int lineno) {
            // FIXME: most works, but we need to treat triple quote/double 
            // as unique (because you can have single quotes in double quotes
            // FIXME: would be nice if scroll didn't change as we move over lines
            if (lineno >= textBox.Lines.Length || lineno < 0)
                return;
            String line = textBox.Lines[lineno] + '\0';
            int start = textBox.GetFirstCharIndexFromLine(lineno);
            // Backup the users current selection point.
            int selectionStart = textBox.SelectionStart;
            int selectionLength = textBox.SelectionLength;
            int index = start;
            int line_pos = 0;
            int tokenStart = 0;
            string token;
            if (mode == 2) // inside quote
                tokenStart = index;
            else if (mode == 3) // inside double quote
                tokenStart = index;
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
                        mode = 0;
                        textBox.SelectionStart = index;
                        textBox.SelectionLength = line.Length - line_pos - 1;
                        textBox.SelectionColor = colors["comment"];
                        textBox.SelectionFont = fonts["comment"];
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
                        token = textBox.Text.Substring(tokenStart, index - tokenStart);
                        if (keywords.ContainsKey(token))
                        {
                            mode = 0;
                            int colorCode = keywords[token];
                            if (colorCode == 1)
                            {
                                textBox.SelectionStart = tokenStart;
                                textBox.SelectionLength = token.Length;
                                textBox.SelectionColor = colors["keyword"];
                                textBox.SelectionFont = fonts["keyword"];
                            }
                            else if (colorCode == 2)
                            {
                                textBox.SelectionStart = tokenStart;
                                textBox.SelectionLength = token.Length;
                                textBox.SelectionColor = colors["syntax"];
                                textBox.SelectionFont = fonts["syntax"];
                            }
                        }
                        else
                        {
                            mode = 0;
                            textBox.SelectionStart = tokenStart;
                            textBox.SelectionLength = token.Length;
                            textBox.SelectionColor = colors["default"];
                            textBox.SelectionFont = fonts["default"];
                        }
                    } // else still in token
                }
                else if (mode == 2) // in double quote
                {
                    if (c == '"' || c == '\0') // end of double quote
                    {
                        if (c == '"')
                        {
                            mode = 0;
                            token = textBox.Text.Substring(tokenStart, index - tokenStart + 1);
                        }
                        else
                        {
                            token = textBox.Text.Substring(tokenStart, index - tokenStart);
                        }
                        textBox.SelectionStart = tokenStart;
                        textBox.SelectionLength = token.Length;
                        textBox.SelectionColor = colors["doublequote"];
                        textBox.SelectionFont = fonts["doublequote"];
                    } // else still in double quote
                }
                else if (mode == 3) // in quote
                {
                    if (c == '\'' || c == '\0') // end of quote
                    {
                        if (c == '\'')
                        {
                            mode = 0;
                            token = textBox.Text.Substring(tokenStart, index - tokenStart + 1);
                        }
                        else
                        {
                            token = textBox.Text.Substring(tokenStart, index - tokenStart);
                        }
                        textBox.SelectionStart = tokenStart;
                        textBox.SelectionLength = token.Length;
                        textBox.SelectionColor = colors["quote"];
                        textBox.SelectionFont = fonts["quote"];
                    } // else still in quote
                }
                index++;
                line_pos++;
            }
            last_mode[lineno] = mode; // what mode did we leave off in?
            // Restore the users current selection point.    
            textBox.SelectionStart = selectionStart;
            textBox.SelectionLength = selectionLength;
            textBox.SelectionColor = colors["default"];
            textBox.SelectionFont = fonts["default"];
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
