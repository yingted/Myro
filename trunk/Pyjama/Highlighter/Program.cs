using System;
using System.Drawing;
using System.Windows.Forms;
using System.Text;
using System.IO;

using Highlighter;

namespace RichTextBoxTest
{
        public class MainForm : System.Windows.Forms.Form
        {
                private System.Windows.Forms.Button button1;
                private System.Windows.Forms.RichTextBox richTextBox1;

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
				  this.richTextBox1 = new RichTextBox();
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
				  this.Controls.Add(this.richTextBox1);
				  this.Name = "MainForm";
				  this.Text = "MainForm";
				  this.ResumeLayout(false);
                }
                #endregion
					
				void Button1Click(object sender, System.EventArgs e)
                {
				  this.richTextBox1.Rtf = HighlightString(richTextBox1.Text);
                }
				
				public static string HighlightString(string text) {
				  // clear tree
				  tvParsetree.Nodes.Clear();
				  input = ""; // grammar
				  ParseTree tree = parser.Parse(input, new GrammarTree());
				  if (tree.Errors.Count == 0) {
					grammar = (Grammar)tree.Eval();
					grammar.Preprocess();
                    compiler.Compile(grammar);
					SetHighlighterLanguage(grammar.Directives["TinyPG"]["Language"]);


					Scanner scanner = new Highlighter.Scanner();
					Parser parser = new Highlighter.Parser(scanner);
					ParseTree Tree = parser.Parse(text);
					if (Tree == null) return "";
					StringBuilder sb = new StringBuilder();
					ParseNode start = Tree.Nodes[0];
					HightlightNode(start, sb);
					AddRtfHeader(sb);
					AddRtfEnd(sb);
					return sb.ToString();
				  }
				}
				
				private void SetHighlighterLanguage(string language)
				{
				  switch (CodeGeneratorFactory.GetSupportedLanguage(language))
				  {
				  case SupportedLanguage.VBNet:
					scanner.Patterns[Highlighter.TokenType.DOTNET_STRING] = scanner.Patterns[Highlighter.TokenType.VB_STRING];
					scanner.Patterns[Highlighter.TokenType.DOTNET_SYMBOL] = scanner.Patterns[Highlighter.TokenType.VB_SYMBOL];
					scanner.Patterns[Highlighter.TokenType.DOTNET_COMMENTBLOCK] = scanner.Patterns[Highlighter.TokenType.VB_COMMENTBLOCK];
					scanner.Patterns[Highlighter.TokenType.DOTNET_COMMENTLINE] = scanner.Patterns[Highlighter.TokenType.VB_COMMENTLINE];
					scanner.Patterns[Highlighter.TokenType.DOTNET_KEYWORD] = scanner.Patterns[Highlighter.TokenType.VB_KEYWORD];
					scanner.Patterns[Highlighter.TokenType.DOTNET_NONKEYWORD] = scanner.Patterns[Highlighter.TokenType.VB_NONKEYWORD];
					break;
				  default:
					scanner.Patterns[Highlighter.TokenType.DOTNET_STRING] = scanner.Patterns[Highlighter.TokenType.CS_STRING];
					scanner.Patterns[Highlighter.TokenType.DOTNET_SYMBOL] = scanner.Patterns[Highlighter.TokenType.CS_SYMBOL];
					scanner.Patterns[Highlighter.TokenType.DOTNET_COMMENTBLOCK] = scanner.Patterns[Highlighter.TokenType.CS_COMMENTBLOCK];
					scanner.Patterns[Highlighter.TokenType.DOTNET_COMMENTLINE] = scanner.Patterns[Highlighter.TokenType.CS_COMMENTLINE];
					scanner.Patterns[Highlighter.TokenType.DOTNET_KEYWORD] = scanner.Patterns[Highlighter.TokenType.CS_KEYWORD];
					scanner.Patterns[Highlighter.TokenType.DOTNET_NONKEYWORD] = scanner.Patterns[Highlighter.TokenType.CS_NONKEYWORD];
					break;
				  }
				}
				
				private static void HightlightNode(ParseNode node, StringBuilder sb)
				{
				  if (node.Nodes.Count == 0)
				  {
					switch (node.Token.Type)
					{
                    case TokenType.GRAMMARCOMMENTLINE:
					  sb.Append(@"{{\cf1 ");
					  break;
                    case TokenType.GRAMMARCOMMENTBLOCK:
					  sb.Append(@"{{\cf2 ");
					  break;
                    case TokenType.DIRECTIVESTRING:
					  sb.Append(@"{{\cf3 ");
					  break;
                    case TokenType.DIRECTIVEKEYWORD:
					  sb.Append(@"{{\cf4 ");
					  break;
                    case TokenType.DIRECTIVEOPEN:
					  sb.Append(@"{{\cf5 ");
					  break;
                    case TokenType.DIRECTIVECLOSE:
					  sb.Append(@"{{\cf6 ");
					  break;
                    case TokenType.ATTRIBUTEKEYWORD:
					  sb.Append(@"{{\cf7 ");
					  break;
                    case TokenType.CS_KEYWORD:
					  sb.Append(@"{{\cf8 ");
					  break;
                    case TokenType.VB_KEYWORD:
					  sb.Append(@"{{\cf9 ");
					  break;
                    case TokenType.DOTNET_KEYWORD:
					  sb.Append(@"{{\cf10 ");
					  break;
                    case TokenType.DOTNET_TYPES:
					  sb.Append(@"{{\cf11 ");
					  break;
                    case TokenType.CS_COMMENTLINE:
					  sb.Append(@"{{\cf12 ");
					  break;
                    case TokenType.CS_COMMENTBLOCK:
					  sb.Append(@"{{\cf13 ");
					  break;
                    case TokenType.CS_STRING:
					  sb.Append(@"{{\cf14 ");
					  break;
                    case TokenType.VB_COMMENTLINE:
					  sb.Append(@"{{\cf15 ");
					  break;
                    case TokenType.VB_COMMENTBLOCK:
					  sb.Append(@"{{\cf16 ");
					  break;
                    case TokenType.VB_STRING:
					  sb.Append(@"{{\cf17 ");
					  break;
                    case TokenType.DOTNET_COMMENTLINE:
					  sb.Append(@"{{\cf18 ");
					  break;
                    case TokenType.DOTNET_COMMENTBLOCK:
					  sb.Append(@"{{\cf19 ");
					  break;
                    case TokenType.DOTNET_STRING:
					  sb.Append(@"{{\cf20 ");
					  break;
                    case TokenType.CODEBLOCKOPEN:
					  sb.Append(@"{{\cf21 ");
					  break;
                    case TokenType.CODEBLOCKCLOSE:
					  sb.Append(@"{{\cf22 ");
					  break;
                    case TokenType.GRAMMARKEYWORD:
					  sb.Append(@"{{\cf23 ");
					  break;
                    case TokenType.GRAMMARARROW:
					  sb.Append(@"{{\cf24 ");
					  break;
                    case TokenType.GRAMMARSTRING:
					  sb.Append(@"{{\cf25 ");
					  break;
					  
                    default:
					  sb.Append(@"{{\cf0 ");
					  break;
					}
					sb.Append(node.Token.Text.Replace(@"\", @"\\").Replace("{", @"\{").Replace("}", @"\}").Replace("\n", "\\par\n")); // "));
					sb.Append(@"}");
				  }
				  
				  foreach (ParseNode n in node.Nodes)
				  {
					HightlightNode(n, sb);
				  }
				}
				
				// define the color palette to be used here
				private static void AddRtfHeader(StringBuilder sb)
				{
				  sb.Insert(0, @"{\rtf1\ansi\deff0{\fonttbl{\f0\fnil\fcharset0 Consolas;}}{\colortbl;\red0\green128\blue0;\red0\green128\blue0;\red255\green0\blue0;\red128\green0\blue255;\red128\green0\blue128;\red128\green0\blue128;\red43\green145\blue202;\red0\green0\blue255;\red255\green0\blue0;\red0\green0\blue255;\red43\green145\blue202;\red0\green128\blue0;\red0\green128\blue0;\red163\green21\blue21;\red0\green128\blue0;\red0\green128\blue0;\red163\green21\blue21;\red0\green128\blue0;\red0\green128\blue0;\red163\green21\blue21;\red128\green0\blue128;\red128\green0\blue128;\red0\green0\blue255;\red128\green0\blue128;\red163\green21\blue21;}\viewkind4\uc1\pard\lang1033\f0\fs20");
				}
				
				private static void AddRtfEnd(StringBuilder sb)
				{
				  sb.Append("}");
				}
				
				
        }
}
