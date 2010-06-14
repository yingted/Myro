using System;
using System.Drawing;
using System.Windows.Forms;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Configuration;
using System.Xml;
using System.IO;
using System.Reflection;
using System.Collections;

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
        public ArrayList keywords = new ArrayList();
        public ArrayList booleans = new ArrayList();
        public ArrayList syntax = new ArrayList();

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
            
            string fileName = @"Config/Python.xml";
            string fullPath = Path.GetFullPath(fileName);
            XmlDocument doc = new XmlDocument();
            doc.Load(fullPath);
            XmlElement root = doc.DocumentElement;
            //XmlNodeList list = root.ChildNodes;

            //FixMe: Make more general
            //Idea: If XML format does not change, the order of elements will always
            //be color, font, size, and style

            /* Default Format */
            colors.Add(root.Name.ToString(), Color.FromName(root.GetAttribute("color")));
            fonts.Add(root.Name.ToString(), new Font(root.GetAttribute("font"),
                Convert.ToInt32(root.GetAttribute("size")),
                (FontStyle)Enum.Parse(typeof(FontStyle), root.GetAttribute("style"))));
 
            /* Comment Format */
            XmlNodeList commentList = doc.SelectNodes("default/comment");
            XmlNode commentNode = commentList[0];
            XmlAttributeCollection commentCol = commentNode.Attributes;

            colors.Add("comment", Color.FromName(commentCol[0].Value.ToString()));
            fonts.Add("comment", new Font(commentCol[1].Value.ToString(),
                Convert.ToInt32(commentCol[2].Value.ToString()),
                (FontStyle)Enum.Parse(typeof(FontStyle), commentCol[3].Value.ToString())));

            /* Quote Format */
            XmlNodeList quoteList = doc.SelectNodes("default/quote");
            XmlNode quoteNode = quoteList[0];
            XmlAttributeCollection quoteCol = quoteNode.Attributes;

            colors.Add("quote", Color.FromName(quoteCol[0].Value.ToString()));
            fonts.Add("quote", new Font(quoteCol[1].Value.ToString(),
                Convert.ToInt32(quoteCol[2].Value.ToString()),
                (FontStyle)Enum.Parse(typeof(FontStyle), quoteCol[3].Value.ToString())));

            /* Keyword */
            XmlNodeList keywordList = doc.SelectNodes("default/keyword");
            XmlNode keywordNode = keywordList[0];
            XmlAttributeCollection keywordCol = keywordNode.Attributes;

            colors.Add("keyword", Color.FromName(keywordCol[0].Value.ToString()));
            fonts.Add("keyword", new Font(keywordCol[1].Value.ToString(), 
                Convert.ToInt32(keywordCol[2].Value.ToString()),
                (FontStyle)Enum.Parse(typeof(FontStyle), keywordCol[3].Value.ToString())));

            XmlNodeList keyChilds = keywordNode.ChildNodes;
            foreach (XmlNode kChild in keyChilds)
                keywords.Add(kChild.InnerXml.ToString());

            /* Syntax Format */
            XmlNodeList syntaxList = doc.SelectNodes("default/syntax");
            XmlNode syntaxNode = syntaxList[0];
            XmlAttributeCollection syntaxCol = syntaxNode.Attributes;

            colors.Add("syntax", Color.FromName(syntaxCol[0].Value.ToString()));
            fonts.Add("syntax", new Font(syntaxCol[1].Value.ToString(),
                Convert.ToInt32(syntaxCol[2].Value.ToString()),
                (FontStyle)Enum.Parse(typeof(FontStyle), syntaxCol[3].Value.ToString())));

            XmlNodeList syntaxChilds = syntaxNode.ChildNodes;
            foreach (XmlNode sChild in syntaxChilds)
                syntax.Add(sChild.InnerXml.ToString());

            /* Booleans */
            XmlNodeList boolList = doc.SelectNodes("default/boolean");
            XmlNode boolNode = boolList[0];
            XmlAttributeCollection boolCol = boolNode.Attributes;

            colors.Add("boolean", Color.FromName(boolCol[0].Value.ToString()));
            fonts.Add("boolean", new Font(boolCol[1].Value.ToString(),
                Convert.ToInt32(boolCol[2].Value.ToString()),
                (FontStyle)Enum.Parse(typeof(FontStyle), boolCol[3].Value.ToString())));

            XmlNodeList boolChilds = boolNode.ChildNodes;
            foreach (XmlNode bChild in boolChilds)
                booleans.Add(bChild.InnerXml.ToString());                  
           
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
            for (int i = 0; i < 60; i++)
            {
                FormatLine(i);
            }
            textBox.lockUpdate = false;
        }

        // Going to see if I can write a parser using the RegEx class
        public void ParseAll()
        {
            textBox.lockUpdate = true;
            Regex r = new Regex("\\n");
            String[] lines = r.Split(textBox.Text.ToString());
            foreach (string l in lines)
            {
                ParseLine(l);
            }
            textBox.lockUpdate = false;
        }

        public void ParseLine(string line)
        {
            string pattern;
            pattern = @"\s+[:]\t$";
            Regex r = new Regex(pattern);
            String[] tokens = r.Split(line);
            foreach (string token in tokens)
            {
                System.Console.WriteLine("string = ", token);
                textBox.SelectionColor = colors["default"];
                textBox.SelectionFont = fonts["default"];

                if (keywords.Contains(token))
                {
                    textBox.SelectionColor = colors["keyword"];
                    textBox.SelectionFont = fonts["keyword"];
                    break;
                }
                else if (booleans.Contains(token))
                {
                    textBox.SelectionColor = colors["boolean"];
                    textBox.SelectionFont = fonts["boolean"];
                    break;
                }
                else if (syntax.Contains(token))
                {
                    textBox.SelectionColor = colors["comment"];
                    textBox.SelectionFont = fonts["comment"];
                    break;
                }
                textBox.SelectedText = token;
            }
            textBox.SelectedText = "\n";
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
                        textBox.SelectionColor = colors["quote"];
                        textBox.SelectionFont = fonts["quote"];
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
                        
                        if (keywords.Contains(token))
                        {
                            mode = 0;
                            textBox.SelectionStart = tokenStart;
                            textBox.SelectionLength = token.Length;
                            textBox.SelectionColor = colors["keyword"];
                            textBox.SelectionFont = fonts["keyword"];
                        }
                        else if (booleans.Contains(token))
                        {
                            mode = 0;
                            textBox.SelectionStart = tokenStart;
                            textBox.SelectionLength = token.Length;
                            textBox.SelectionColor = colors["boolean"];
                            textBox.SelectionFont = fonts["boolean"];
                        }
                        else if (syntax.Contains(token))
                        {
                            mode = 0;
                            textBox.SelectionStart = tokenStart;
                            textBox.SelectionLength = token.Length;
                            textBox.SelectionColor = colors["syntax"];
                            textBox.SelectionFont = fonts["syntax"];
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
                        textBox.SelectionColor = colors["quote"];
                        textBox.SelectionFont = fonts["quote"];
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
