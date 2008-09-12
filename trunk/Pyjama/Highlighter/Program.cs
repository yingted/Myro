using System;
using System.Drawing;
using System.Windows.Forms;
using System.IO;

namespace RichTextBoxTest
{
        public class MainForm : System.Windows.Forms.Form
        {
                private System.Windows.Forms.Button button1;
                private System.Windows.Forms.RichTextBox richTextBox1;
				TinyPG.Highlighter.Scanner highlighterScanner;
				TinyPG.Highlighter.TextHighlighter textHighlighter;

                public MainForm()
                {
                        InitializeComponent();
                }
                
                [STAThread]
                public static void Main(string[] args)
                {
                        Application.Run(new MainForm());
                }
                
                #region Windows Forms Designer generated code
                /// <summary>
                /// This method is required for Windows Forms designer support.
                /// Do not change the method contents inside the
                /// source code editor. The Forms designer might
                /// not be able to load this method if it was changed manually.
                /// </summary>
                private void InitializeComponent()
                {
                        this.richTextBox1 = new System.Windows.Forms.RichTextBox();
						
						highlighterScanner = new TinyPG.Highlighter.Scanner();
						textHighlighter = new TinyPG.Highlighter.TextHighlighter(this.richTextBox1, 
							highlighterScanner, 
							new TinyPG.Highlighter.Parser(highlighterScanner));

                        this.button1 = new System.Windows.Forms.Button();
                        this.SuspendLayout();
                        // 
                        // richTextBox1
                        // 
                         this.richTextBox1.Dock = System.Windows.Forms.DockStyle.Bottom;
                         this.richTextBox1.Location = new System.Drawing.Point(0, 67);
                         this.richTextBox1.Name = "richTextBox1";
                         this.richTextBox1.Size = new System.Drawing.Size(672, 348);
                         this.richTextBox1.TabIndex = 0;
                         this.richTextBox1.Text = "richTextBox1";
						
						this.textHighlighter.Dock = System.Windows.Forms.DockStyle.Bottom;
						this.textHighlighter.Location = new System.Drawing.Point(0, 67);
						this.textHighlighter.Name = "textHighlighter";
						this.textHighlighter.Size = new System.Drawing.Size(672, 348);
						this.textHighlighter.TabIndex = 0;
						this.textHighlighter.Text = "textHighlighter";

                        // 
                        // button1
                        // 
                        this.button1.Location = new System.Drawing.Point(296, 16);
                        this.button1.Name = "button1";
                        this.button1.TabIndex = 1;
                        this.button1.Text = "button1";
                        this.button1.Click += new System.EventHandler(this.Button1Click);
                        // 
                        // MainForm
                        // 
                        this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
                        this.ClientSize = new System.Drawing.Size(672, 415);
                        this.Controls.Add(this.button1);
                        this.Controls.Add(this.textHighlighter);
                        //this.Controls.Add(this.richTextBox1);
                        this.Name = "MainForm";
                        this.Text = "MainForm";
                        this.ResumeLayout(false);
                }
                #endregion
                
                void Button1Click(object sender, System.EventArgs e)
                {
                        FileStream fileStream = new FileStream("simple.rtf", FileMode.Open);
                        //richTextBox1.LoadFile(fileStream, RichTextBoxStreamType.RichText);
                        textHighlighter.LoadFile(fileStream, RichTextBoxStreamType.RichText);
						textHighlighter.threadAutoHighlight.Start();
                }
                
        }
}
