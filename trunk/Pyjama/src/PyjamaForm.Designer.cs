using System.Drawing;
using System.Windows.Forms;

﻿namespace Pyjama
 {

     class MyTextBox : TextBox
     {
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
     }

     partial class PyjamaForm
     {
         /// <summary>
         /// Required designer variable.
         /// </summary>
         private System.ComponentModel.IContainer components = null;

         /// <summary>
         /// Clean up any resources being used.
         /// </summary>
         /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
         protected override void Dispose(bool disposing)
         {
             if (disposing && (components != null))
             {
                 components.Dispose();
             }
             base.Dispose(disposing);
         }

         #region Windows Form Designer generated code

         /// <summary>
         /// Required method for Designer support - do not modify
         /// the contents of this method with the code editor.
         /// </summary>
         private void InitializeComponent()
         {
             System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(PyjamaForm));
             this.mainContentSplit = new System.Windows.Forms.SplitContainer();
             this.statusStrip1 = new System.Windows.Forms.StatusStrip();
             this.toolStripStatusLabel8 = new System.Windows.Forms.ToolStripStatusLabel();
             this.toolStripStatusLabel9 = new System.Windows.Forms.ToolStripStatusLabel();
             this.toolStripStatusLabel10 = new System.Windows.Forms.ToolStripDropDownButton();
             this.pythonToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
             this.rubyToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
             this.schemeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripStatusLabel11 = new System.Windows.Forms.ToolStripStatusLabel();
             this.lineNumber = new System.Windows.Forms.ToolStripSplitButton();
             this.lineNumberEntry = new System.Windows.Forms.ToolStripTextBox();
             this.toolStripStatusLabel13 = new System.Windows.Forms.ToolStripStatusLabel();
             this.columnNumber = new System.Windows.Forms.ToolStripStatusLabel();
             this._docManager = new Pyjama.DocumentManager();
             this.splitContainer1 = new System.Windows.Forms.SplitContainer();
             this.outputWindow = new Pyjama.Console();
             this.commandContainer = new System.Windows.Forms.SplitContainer();
             this.commandTextBox = new Pyjama.MyTextBox();
             this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
             this.runButton = new System.Windows.Forms.Button();
             this.commandLabel = new System.Windows.Forms.Label();
             this.statusStrip2 = new System.Windows.Forms.StatusStrip();
             this.toolStripStatusLabel5 = new System.Windows.Forms.ToolStripStatusLabel();
             this.toolStripStatusLabel6 = new System.Windows.Forms.ToolStripStatusLabel();
             this.shellLanguageButton = new System.Windows.Forms.ToolStripDropDownButton();
             this.shellLanguageSelect1 = new System.Windows.Forms.ToolStripMenuItem();
             this.shellLanguageSelect2 = new System.Windows.Forms.ToolStripMenuItem();
             this.shellLanguageSelect3 = new System.Windows.Forms.ToolStripMenuItem();
             this.main = new System.Windows.Forms.Panel();
             this.toolStrip = new System.Windows.Forms.Panel();
             this.toolStrip1 = new System.Windows.Forms.ToolStrip();
             this.newToolStripButton = new System.Windows.Forms.ToolStripButton();
             this.openToolStripButton = new System.Windows.Forms.ToolStripButton();
             this.saveToolStripButton = new System.Windows.Forms.ToolStripButton();
             this.printToolStripButton = new System.Windows.Forms.ToolStripButton();
             this.toolStripSeparator6 = new System.Windows.Forms.ToolStripSeparator();
             this.cutToolStripButton = new System.Windows.Forms.ToolStripButton();
             this.copyToolStripButton = new System.Windows.Forms.ToolStripButton();
             this.pasteToolStripButton = new System.Windows.Forms.ToolStripButton();
             this.toolStripSeparator7 = new System.Windows.Forms.ToolStripSeparator();
             this.toolStripButton1 = new System.Windows.Forms.ToolStripButton();
             this.menuStrip1 = new System.Windows.Forms.MenuStrip();
             this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.newToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripSeparator = new System.Windows.Forms.ToolStripSeparator();
             this.saveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.saveAsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
             this.printToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.printSetupMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.previewMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.printScriptMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
             this.closeTabToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.editToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.undoToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.redoToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
             this.cutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.copyToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.pasteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
             this.selectAllToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripSeparator8 = new System.Windows.Forms.ToolStripSeparator();
             this.selectEditorToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.languageToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.executeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.shellToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.shellRestartToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.evaluateToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.newlineToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.optionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.contentsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
             this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
             this.toolStripStatusLabel1 = new System.Windows.Forms.ToolStripStatusLabel();
             this.toolStripStatusLabel2 = new System.Windows.Forms.ToolStripStatusLabel();
             this.languageName = new System.Windows.Forms.ToolStripStatusLabel();
             this.toolStripStatusLabel3 = new System.Windows.Forms.ToolStripStatusLabel();
             this.toolStripStatusLabel4 = new System.Windows.Forms.ToolStripStatusLabel();
             this.mainContentSplit.Panel1.SuspendLayout();
             this.mainContentSplit.Panel2.SuspendLayout();
             this.mainContentSplit.SuspendLayout();
             this.statusStrip1.SuspendLayout();
             this.splitContainer1.Panel1.SuspendLayout();
             this.splitContainer1.Panel2.SuspendLayout();
             this.splitContainer1.SuspendLayout();
             this.commandContainer.Panel1.SuspendLayout();
             this.commandContainer.Panel2.SuspendLayout();
             this.commandContainer.SuspendLayout();
             this.tableLayoutPanel1.SuspendLayout();
             this.statusStrip2.SuspendLayout();
             this.main.SuspendLayout();
             this.toolStrip.SuspendLayout();
             this.toolStrip1.SuspendLayout();
             this.menuStrip1.SuspendLayout();
             this.SuspendLayout();
             // 
             // mainContentSplit
             // 
             this.mainContentSplit.BackColor = System.Drawing.SystemColors.Info;
             this.mainContentSplit.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
             this.mainContentSplit.Cursor = System.Windows.Forms.Cursors.Default;
             resources.ApplyResources(this.mainContentSplit, "mainContentSplit");
             this.mainContentSplit.FixedPanel = System.Windows.Forms.FixedPanel.Panel1;
             this.mainContentSplit.Name = "mainContentSplit";
             // 
             // mainContentSplit.Panel1
             // 
             this.mainContentSplit.Panel1.Controls.Add(this.statusStrip1);
             this.mainContentSplit.Panel1.Controls.Add(this._docManager);
             resources.ApplyResources(this.mainContentSplit.Panel1, "mainContentSplit.Panel1");
             // 
             // mainContentSplit.Panel2
             // 
             this.mainContentSplit.Panel2.BackColor = System.Drawing.SystemColors.ActiveCaptionText;
             this.mainContentSplit.Panel2.Controls.Add(this.splitContainer1);
             this.mainContentSplit.Panel2.Controls.Add(this.statusStrip2);
             resources.ApplyResources(this.mainContentSplit.Panel2, "mainContentSplit.Panel2");
             // 
             // statusStrip1
             // 
             this.statusStrip1.BackColor = System.Drawing.SystemColors.Control;
             this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatusLabel8,
            this.toolStripStatusLabel9,
            this.toolStripStatusLabel10,
            this.toolStripStatusLabel11,
            this.lineNumber,
            this.toolStripStatusLabel13,
            this.columnNumber});
             resources.ApplyResources(this.statusStrip1, "statusStrip1");
             this.statusStrip1.Name = "statusStrip1";
             this.statusStrip1.SizingGrip = false;
             // 
             // toolStripStatusLabel8
             // 
             resources.ApplyResources(this.toolStripStatusLabel8, "toolStripStatusLabel8");
             this.toolStripStatusLabel8.Name = "toolStripStatusLabel8";
             this.toolStripStatusLabel8.Padding = new System.Windows.Forms.Padding(0, 0, 30, 0);
             // 
             // toolStripStatusLabel9
             // 
             resources.ApplyResources(this.toolStripStatusLabel9, "toolStripStatusLabel9");
             this.toolStripStatusLabel9.Name = "toolStripStatusLabel9";
             // 
             // toolStripStatusLabel10
             // 
             this.toolStripStatusLabel10.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.pythonToolStripMenuItem1,
            this.rubyToolStripMenuItem1,
            this.schemeToolStripMenuItem});
             this.toolStripStatusLabel10.Name = "toolStripStatusLabel10";
             this.toolStripStatusLabel10.Padding = new System.Windows.Forms.Padding(35, 0, 0, 0);
             resources.ApplyResources(this.toolStripStatusLabel10, "toolStripStatusLabel10");
             // 
             // pythonToolStripMenuItem1
             // 
             this.pythonToolStripMenuItem1.Checked = true;
             this.pythonToolStripMenuItem1.CheckState = System.Windows.Forms.CheckState.Checked;
             this.pythonToolStripMenuItem1.Name = "pythonToolStripMenuItem1";
             resources.ApplyResources(this.pythonToolStripMenuItem1, "pythonToolStripMenuItem1");
             // 
             // rubyToolStripMenuItem1
             // 
             this.rubyToolStripMenuItem1.Name = "rubyToolStripMenuItem1";
             resources.ApplyResources(this.rubyToolStripMenuItem1, "rubyToolStripMenuItem1");
             // 
             // schemeToolStripMenuItem
             // 
             this.schemeToolStripMenuItem.Name = "schemeToolStripMenuItem";
             resources.ApplyResources(this.schemeToolStripMenuItem, "schemeToolStripMenuItem");
             // 
             // toolStripStatusLabel11
             // 
             resources.ApplyResources(this.toolStripStatusLabel11, "toolStripStatusLabel11");
             this.toolStripStatusLabel11.Name = "toolStripStatusLabel11";
             // 
             // lineNumber
             // 
             this.lineNumber.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.lineNumberEntry});
             this.lineNumber.Name = "lineNumber";
             this.lineNumber.Padding = new System.Windows.Forms.Padding(20, 0, 0, 0);
             resources.ApplyResources(this.lineNumber, "lineNumber");
             this.lineNumber.ButtonClick += new System.EventHandler(this.lineNumber_ButtonClick);
             // 
             // lineNumberEntry
             // 
             this.lineNumberEntry.Name = "lineNumberEntry";
             resources.ApplyResources(this.lineNumberEntry, "lineNumberEntry");
             this.lineNumberEntry.KeyDown += new System.Windows.Forms.KeyEventHandler(this.lineNumberEntry_KeyDown);
             this.lineNumberEntry.Click += new System.EventHandler(this.lineNumberEntry_Click);
             // 
             // toolStripStatusLabel13
             // 
             resources.ApplyResources(this.toolStripStatusLabel13, "toolStripStatusLabel13");
             this.toolStripStatusLabel13.Name = "toolStripStatusLabel13";
             // 
             // columnNumber
             // 
             this.columnNumber.BackColor = System.Drawing.SystemColors.Control;
             this.columnNumber.Name = "columnNumber";
             this.columnNumber.Padding = new System.Windows.Forms.Padding(20, 0, 0, 0);
             resources.ApplyResources(this.columnNumber, "columnNumber");
             // 
             // _docManager
             // 
             resources.ApplyResources(this._docManager, "_docManager");
             this._docManager.BackColor = System.Drawing.SystemColors.Control;
             this._docManager.FontToUse = null;
             this._docManager.Name = "_docManager";
             // 
             // splitContainer1
             // 
             resources.ApplyResources(this.splitContainer1, "splitContainer1");
             this.splitContainer1.BackColor = System.Drawing.SystemColors.ControlLight;
             this.splitContainer1.FixedPanel = System.Windows.Forms.FixedPanel.Panel2;
             this.splitContainer1.Name = "splitContainer1";
             // 
             // splitContainer1.Panel1
             // 
             this.splitContainer1.Panel1.BackColor = System.Drawing.SystemColors.ActiveCaptionText;
             this.splitContainer1.Panel1.Controls.Add(this.outputWindow);
             // 
             // splitContainer1.Panel2
             // 
             this.splitContainer1.Panel2.Controls.Add(this.commandContainer);
             // 
             // outputWindow
             // 
             resources.ApplyResources(this.outputWindow, "outputWindow");
             this.outputWindow.BackColor = System.Drawing.SystemColors.WindowText;
             this.outputWindow.Cursor = System.Windows.Forms.Cursors.Default;
             this.outputWindow.ForeColor = System.Drawing.SystemColors.HighlightText;
             this.outputWindow.Name = "outputWindow";
             // 
             // commandContainer
             // 
             resources.ApplyResources(this.commandContainer, "commandContainer");
             this.commandContainer.Name = "commandContainer";
             // 
             // commandContainer.Panel1
             // 
             this.commandContainer.Panel1.Controls.Add(this.commandTextBox);
             // 
             // commandContainer.Panel2
             // 
             this.commandContainer.Panel2.Controls.Add(this.tableLayoutPanel1);
             // 
             // commandTextBox
             // 
             this.commandTextBox.AcceptsTab = true;
             this.commandTextBox.BackColor = System.Drawing.SystemColors.WindowText;
             this.commandTextBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
             resources.ApplyResources(this.commandTextBox, "commandTextBox");
             this.commandTextBox.ForeColor = System.Drawing.SystemColors.HighlightText;
             this.commandTextBox.Name = "commandTextBox";
             this.commandTextBox.TextChanged += new System.EventHandler(this.commandTextBox_TextChanged);
             this.commandTextBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.commandTextBox_KeyDown);
             this.commandTextBox.KeyUp += new System.Windows.Forms.KeyEventHandler(this.commandTextBox_KeyUp);
             this.commandTextBox.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.commandTextBox_KeyPress);
             // 
             // tableLayoutPanel1
             // 
             resources.ApplyResources(this.tableLayoutPanel1, "tableLayoutPanel1");
             this.tableLayoutPanel1.Controls.Add(this.runButton, 0, 1);
             this.tableLayoutPanel1.Controls.Add(this.commandLabel, 0, 0);
             this.tableLayoutPanel1.Name = "tableLayoutPanel1";
             // 
             // runButton
             // 
             resources.ApplyResources(this.runButton, "runButton");
             this.runButton.Name = "runButton";
             this.runButton.UseVisualStyleBackColor = true;
             this.runButton.Click += new System.EventHandler(this.runButton_Click);
             // 
             // commandLabel
             // 
             resources.ApplyResources(this.commandLabel, "commandLabel");
             this.commandLabel.Name = "commandLabel";
             this.commandLabel.Click += new System.EventHandler(this.commandLabel_Click);
             // 
             // statusStrip2
             // 
             this.statusStrip2.BackColor = System.Drawing.SystemColors.Control;
             this.statusStrip2.GripStyle = System.Windows.Forms.ToolStripGripStyle.Visible;
             this.statusStrip2.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatusLabel5,
            this.toolStripStatusLabel6,
            this.shellLanguageButton});
             resources.ApplyResources(this.statusStrip2, "statusStrip2");
             this.statusStrip2.Name = "statusStrip2";
             // 
             // toolStripStatusLabel5
             // 
             resources.ApplyResources(this.toolStripStatusLabel5, "toolStripStatusLabel5");
             this.toolStripStatusLabel5.Name = "toolStripStatusLabel5";
             this.toolStripStatusLabel5.Padding = new System.Windows.Forms.Padding(0, 0, 30, 0);
             // 
             // toolStripStatusLabel6
             // 
             resources.ApplyResources(this.toolStripStatusLabel6, "toolStripStatusLabel6");
             this.toolStripStatusLabel6.Name = "toolStripStatusLabel6";
             // 
             // shellLanguageButton
             // 
             this.shellLanguageButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
             this.shellLanguageButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.shellLanguageSelect1,
            this.shellLanguageSelect2,
            this.shellLanguageSelect3});
             resources.ApplyResources(this.shellLanguageButton, "shellLanguageButton");
             this.shellLanguageButton.Name = "shellLanguageButton";
             this.shellLanguageButton.Padding = new System.Windows.Forms.Padding(35, 0, 0, 0);
             // 
             // shellLanguageSelect1
             // 
             this.shellLanguageSelect1.Checked = true;
             this.shellLanguageSelect1.CheckState = System.Windows.Forms.CheckState.Checked;
             this.shellLanguageSelect1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
             this.shellLanguageSelect1.Name = "shellLanguageSelect1";
             resources.ApplyResources(this.shellLanguageSelect1, "shellLanguageSelect1");
             this.shellLanguageSelect1.Click += new System.EventHandler(this.shellLanguageSelect1_Click);
             // 
             // shellLanguageSelect2
             // 
             this.shellLanguageSelect2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
             this.shellLanguageSelect2.Name = "shellLanguageSelect2";
             resources.ApplyResources(this.shellLanguageSelect2, "shellLanguageSelect2");
             this.shellLanguageSelect2.Click += new System.EventHandler(this.shellLanguageSelect2_Click);
             // 
             // shellLanguageSelect3
             // 
             this.shellLanguageSelect3.Name = "shellLanguageSelect3";
             resources.ApplyResources(this.shellLanguageSelect3, "shellLanguageSelect3");
             this.shellLanguageSelect3.Click += new System.EventHandler(this.shellLanguageSelect3_Click);
             // 
             // main
             // 
             this.main.BackColor = System.Drawing.SystemColors.Control;
             this.main.Controls.Add(this.mainContentSplit);
             this.main.Controls.Add(this.toolStrip);
             resources.ApplyResources(this.main, "main");
             this.main.Name = "main";
             // 
             // toolStrip
             // 
             resources.ApplyResources(this.toolStrip, "toolStrip");
             this.toolStrip.Controls.Add(this.toolStrip1);
             this.toolStrip.Name = "toolStrip";
             // 
             // toolStrip1
             // 
             resources.ApplyResources(this.toolStrip1, "toolStrip1");
             this.toolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
             this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newToolStripButton,
            this.openToolStripButton,
            this.saveToolStripButton,
            this.printToolStripButton,
            this.toolStripSeparator6,
            this.cutToolStripButton,
            this.copyToolStripButton,
            this.pasteToolStripButton,
            this.toolStripSeparator7,
            this.toolStripButton1});
             this.toolStrip1.Name = "toolStrip1";
             this.toolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional;
             // 
             // newToolStripButton
             // 
             this.newToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             this.newToolStripButton.Image = global::Pyjama.Properties.Resources.NewDocumentHS;
             resources.ApplyResources(this.newToolStripButton, "newToolStripButton");
             this.newToolStripButton.Name = "newToolStripButton";
             this.newToolStripButton.Click += new System.EventHandler(this.newToolStripMenuItem_Click);
             // 
             // openToolStripButton
             // 
             this.openToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             resources.ApplyResources(this.openToolStripButton, "openToolStripButton");
             this.openToolStripButton.Name = "openToolStripButton";
             this.openToolStripButton.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
             // 
             // saveToolStripButton
             // 
             this.saveToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             resources.ApplyResources(this.saveToolStripButton, "saveToolStripButton");
             this.saveToolStripButton.Name = "saveToolStripButton";
             this.saveToolStripButton.Click += new System.EventHandler(this.saveToolStripMenuItem_Click);
             // 
             // printToolStripButton
             // 
             this.printToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             resources.ApplyResources(this.printToolStripButton, "printToolStripButton");
             this.printToolStripButton.Name = "printToolStripButton";
             this.printToolStripButton.Click += new System.EventHandler(this.printToolStripButton_Click);
             // 
             // toolStripSeparator6
             // 
             this.toolStripSeparator6.Name = "toolStripSeparator6";
             resources.ApplyResources(this.toolStripSeparator6, "toolStripSeparator6");
             // 
             // cutToolStripButton
             // 
             this.cutToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             resources.ApplyResources(this.cutToolStripButton, "cutToolStripButton");
             this.cutToolStripButton.Name = "cutToolStripButton";
             this.cutToolStripButton.Click += new System.EventHandler(this.cutToolStripMenuItem_Click);
             // 
             // copyToolStripButton
             // 
             this.copyToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             resources.ApplyResources(this.copyToolStripButton, "copyToolStripButton");
             this.copyToolStripButton.Name = "copyToolStripButton";
             this.copyToolStripButton.Click += new System.EventHandler(this.copyToolStripMenuItem_Click);
             // 
             // pasteToolStripButton
             // 
             this.pasteToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             resources.ApplyResources(this.pasteToolStripButton, "pasteToolStripButton");
             this.pasteToolStripButton.Name = "pasteToolStripButton";
             this.pasteToolStripButton.Click += new System.EventHandler(this.pasteToolStripMenuItem_Click);
             // 
             // toolStripSeparator7
             // 
             this.toolStripSeparator7.Name = "toolStripSeparator7";
             resources.ApplyResources(this.toolStripSeparator7, "toolStripSeparator7");
             // 
             // toolStripButton1
             // 
             this.toolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
             this.toolStripButton1.Image = global::Pyjama.Properties.Resources.FormRunHS;
             resources.ApplyResources(this.toolStripButton1, "toolStripButton1");
             this.toolStripButton1.Name = "toolStripButton1";
             this.toolStripButton1.Click += new System.EventHandler(this.execute_Click);
             // 
             // menuStrip1
             // 
             this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.editToolStripMenuItem,
            this.languageToolStripMenuItem,
            this.toolsToolStripMenuItem,
            this.helpToolStripMenuItem});
             resources.ApplyResources(this.menuStrip1, "menuStrip1");
             this.menuStrip1.Name = "menuStrip1";
             // 
             // fileToolStripMenuItem
             // 
             this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newToolStripMenuItem,
            this.openToolStripMenuItem,
            this.toolStripSeparator,
            this.saveToolStripMenuItem,
            this.saveAsToolStripMenuItem,
            this.toolStripSeparator1,
            this.printToolStripMenuItem,
            this.toolStripSeparator2,
            this.closeTabToolStripMenuItem,
            this.exitToolStripMenuItem});
             this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
             resources.ApplyResources(this.fileToolStripMenuItem, "fileToolStripMenuItem");
             // 
             // newToolStripMenuItem
             // 
             this.newToolStripMenuItem.Image = global::Pyjama.Properties.Resources.NewDocumentHS;
             this.newToolStripMenuItem.Name = "newToolStripMenuItem";
             resources.ApplyResources(this.newToolStripMenuItem, "newToolStripMenuItem");
             this.newToolStripMenuItem.Click += new System.EventHandler(this.newToolStripMenuItem_Click_1);
             // 
             // openToolStripMenuItem
             // 
             resources.ApplyResources(this.openToolStripMenuItem, "openToolStripMenuItem");
             this.openToolStripMenuItem.Name = "openToolStripMenuItem";
             this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
             // 
             // toolStripSeparator
             // 
             this.toolStripSeparator.Name = "toolStripSeparator";
             resources.ApplyResources(this.toolStripSeparator, "toolStripSeparator");
             // 
             // saveToolStripMenuItem
             // 
             resources.ApplyResources(this.saveToolStripMenuItem, "saveToolStripMenuItem");
             this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
             this.saveToolStripMenuItem.Click += new System.EventHandler(this.saveToolStripMenuItem_Click);
             // 
             // saveAsToolStripMenuItem
             // 
             this.saveAsToolStripMenuItem.Name = "saveAsToolStripMenuItem";
             resources.ApplyResources(this.saveAsToolStripMenuItem, "saveAsToolStripMenuItem");
             this.saveAsToolStripMenuItem.Click += new System.EventHandler(this.saveAsToolStripMenuItem_Click);
             // 
             // toolStripSeparator1
             // 
             this.toolStripSeparator1.Name = "toolStripSeparator1";
             resources.ApplyResources(this.toolStripSeparator1, "toolStripSeparator1");
             // 
             // printToolStripMenuItem
             // 
             this.printToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.printSetupMenuItem,
            this.previewMenuItem,
            this.printScriptMenuItem});
             resources.ApplyResources(this.printToolStripMenuItem, "printToolStripMenuItem");
             this.printToolStripMenuItem.Name = "printToolStripMenuItem";
             // 
             // printSetupMenuItem
             // 
             this.printSetupMenuItem.Name = "printSetupMenuItem";
             resources.ApplyResources(this.printSetupMenuItem, "printSetupMenuItem");
             this.printSetupMenuItem.Click += new System.EventHandler(this.printSetupMenuItem_Click);
             // 
             // previewMenuItem
             // 
             this.previewMenuItem.Name = "previewMenuItem";
             resources.ApplyResources(this.previewMenuItem, "previewMenuItem");
             this.previewMenuItem.Click += new System.EventHandler(this.previewMenuItem_Click);
             // 
             // printScriptMenuItem
             // 
             this.printScriptMenuItem.Name = "printScriptMenuItem";
             resources.ApplyResources(this.printScriptMenuItem, "printScriptMenuItem");
             this.printScriptMenuItem.Click += new System.EventHandler(this.printScriptMenuItem_Click);
             // 
             // toolStripSeparator2
             // 
             this.toolStripSeparator2.Name = "toolStripSeparator2";
             resources.ApplyResources(this.toolStripSeparator2, "toolStripSeparator2");
             // 
             // closeTabToolStripMenuItem
             // 
             this.closeTabToolStripMenuItem.Name = "closeTabToolStripMenuItem";
             resources.ApplyResources(this.closeTabToolStripMenuItem, "closeTabToolStripMenuItem");
             this.closeTabToolStripMenuItem.Click += new System.EventHandler(this.closeTabToolStripMenuItem_Click);
             // 
             // exitToolStripMenuItem
             // 
             this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
             resources.ApplyResources(this.exitToolStripMenuItem, "exitToolStripMenuItem");
             this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
             // 
             // editToolStripMenuItem
             // 
             this.editToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.undoToolStripMenuItem,
            this.redoToolStripMenuItem,
            this.toolStripSeparator3,
            this.cutToolStripMenuItem,
            this.copyToolStripMenuItem,
            this.pasteToolStripMenuItem,
            this.toolStripSeparator4,
            this.selectAllToolStripMenuItem,
            this.toolStripSeparator8,
            this.selectEditorToolStripMenuItem});
             this.editToolStripMenuItem.Name = "editToolStripMenuItem";
             resources.ApplyResources(this.editToolStripMenuItem, "editToolStripMenuItem");
             // 
             // undoToolStripMenuItem
             // 
             this.undoToolStripMenuItem.Name = "undoToolStripMenuItem";
             resources.ApplyResources(this.undoToolStripMenuItem, "undoToolStripMenuItem");
             this.undoToolStripMenuItem.Click += new System.EventHandler(this.undoToolStripMenuItem_Click);
             // 
             // redoToolStripMenuItem
             // 
             this.redoToolStripMenuItem.Name = "redoToolStripMenuItem";
             resources.ApplyResources(this.redoToolStripMenuItem, "redoToolStripMenuItem");
             // 
             // toolStripSeparator3
             // 
             this.toolStripSeparator3.Name = "toolStripSeparator3";
             resources.ApplyResources(this.toolStripSeparator3, "toolStripSeparator3");
             // 
             // cutToolStripMenuItem
             // 
             resources.ApplyResources(this.cutToolStripMenuItem, "cutToolStripMenuItem");
             this.cutToolStripMenuItem.Name = "cutToolStripMenuItem";
             this.cutToolStripMenuItem.Click += new System.EventHandler(this.cutToolStripMenuItem_Click);
             // 
             // copyToolStripMenuItem
             // 
             resources.ApplyResources(this.copyToolStripMenuItem, "copyToolStripMenuItem");
             this.copyToolStripMenuItem.Name = "copyToolStripMenuItem";
             this.copyToolStripMenuItem.Click += new System.EventHandler(this.copyToolStripMenuItem_Click);
             // 
             // pasteToolStripMenuItem
             // 
             resources.ApplyResources(this.pasteToolStripMenuItem, "pasteToolStripMenuItem");
             this.pasteToolStripMenuItem.Name = "pasteToolStripMenuItem";
             this.pasteToolStripMenuItem.Click += new System.EventHandler(this.pasteToolStripMenuItem_Click);
             // 
             // toolStripSeparator4
             // 
             this.toolStripSeparator4.Name = "toolStripSeparator4";
             resources.ApplyResources(this.toolStripSeparator4, "toolStripSeparator4");
             // 
             // selectAllToolStripMenuItem
             // 
             this.selectAllToolStripMenuItem.Name = "selectAllToolStripMenuItem";
             resources.ApplyResources(this.selectAllToolStripMenuItem, "selectAllToolStripMenuItem");
             this.selectAllToolStripMenuItem.Click += new System.EventHandler(this.selectAllToolStripMenuItem_Click);
             // 
             // toolStripSeparator8
             // 
             this.toolStripSeparator8.Name = "toolStripSeparator8";
             resources.ApplyResources(this.toolStripSeparator8, "toolStripSeparator8");
             // 
             // selectEditorToolStripMenuItem
             // 
             this.selectEditorToolStripMenuItem.Name = "selectEditorToolStripMenuItem";
             resources.ApplyResources(this.selectEditorToolStripMenuItem, "selectEditorToolStripMenuItem");
             this.selectEditorToolStripMenuItem.Click += new System.EventHandler(this.selectEditorToolStripMenuItem_Click);
             // 
             // languageToolStripMenuItem
             // 
             this.languageToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.executeToolStripMenuItem,
            this.shellToolStripMenuItem,
            this.shellRestartToolStripMenuItem,
            this.evaluateToolStripMenuItem,
            this.newlineToolStripMenuItem});
             this.languageToolStripMenuItem.Name = "languageToolStripMenuItem";
             resources.ApplyResources(this.languageToolStripMenuItem, "languageToolStripMenuItem");
             // 
             // executeToolStripMenuItem
             // 
             this.executeToolStripMenuItem.Name = "executeToolStripMenuItem";
             resources.ApplyResources(this.executeToolStripMenuItem, "executeToolStripMenuItem");
             this.executeToolStripMenuItem.Click += new System.EventHandler(this.execute_Click);
             // 
             // shellToolStripMenuItem
             // 
             this.shellToolStripMenuItem.Name = "shellToolStripMenuItem";
             resources.ApplyResources(this.shellToolStripMenuItem, "shellToolStripMenuItem");
             this.shellToolStripMenuItem.Click += new System.EventHandler(this.shellToolStripMenuItem_Click);
             // 
             // shellRestartToolStripMenuItem
             // 
             this.shellRestartToolStripMenuItem.Name = "shellRestartToolStripMenuItem";
             resources.ApplyResources(this.shellRestartToolStripMenuItem, "shellRestartToolStripMenuItem");
             this.shellRestartToolStripMenuItem.Click += new System.EventHandler(this.shellRestartToolStripMenuItem_Click);
             // 
             // evaluateToolStripMenuItem
             // 
             this.evaluateToolStripMenuItem.Name = "evaluateToolStripMenuItem";
             resources.ApplyResources(this.evaluateToolStripMenuItem, "evaluateToolStripMenuItem");
             this.evaluateToolStripMenuItem.Click += new System.EventHandler(this.runButton_Click);
             // 
             // newlineToolStripMenuItem
             // 
             this.newlineToolStripMenuItem.Name = "newlineToolStripMenuItem";
             resources.ApplyResources(this.newlineToolStripMenuItem, "newlineToolStripMenuItem");
             this.newlineToolStripMenuItem.Click += new System.EventHandler(this.newlineToolStripMenuItem_Click);
             // 
             // toolsToolStripMenuItem
             // 
             this.toolsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.optionsToolStripMenuItem});
             this.toolsToolStripMenuItem.Name = "toolsToolStripMenuItem";
             resources.ApplyResources(this.toolsToolStripMenuItem, "toolsToolStripMenuItem");
             // 
             // optionsToolStripMenuItem
             // 
             this.optionsToolStripMenuItem.Name = "optionsToolStripMenuItem";
             resources.ApplyResources(this.optionsToolStripMenuItem, "optionsToolStripMenuItem");
             this.optionsToolStripMenuItem.Click += new System.EventHandler(this.optionsToolStripMenuItem_Click);
             // 
             // helpToolStripMenuItem
             // 
             this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.contentsToolStripMenuItem,
            this.toolStripSeparator5,
            this.aboutToolStripMenuItem});
             this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
             resources.ApplyResources(this.helpToolStripMenuItem, "helpToolStripMenuItem");
             // 
             // contentsToolStripMenuItem
             // 
             this.contentsToolStripMenuItem.Name = "contentsToolStripMenuItem";
             resources.ApplyResources(this.contentsToolStripMenuItem, "contentsToolStripMenuItem");
             this.contentsToolStripMenuItem.Click += new System.EventHandler(this.contentsToolStripMenuItem_Click);
             // 
             // toolStripSeparator5
             // 
             this.toolStripSeparator5.Name = "toolStripSeparator5";
             resources.ApplyResources(this.toolStripSeparator5, "toolStripSeparator5");
             // 
             // aboutToolStripMenuItem
             // 
             this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
             resources.ApplyResources(this.aboutToolStripMenuItem, "aboutToolStripMenuItem");
             this.aboutToolStripMenuItem.Click += new System.EventHandler(this.aboutToolStripMenuItem_Click);
             // 
             // toolStripStatusLabel1
             // 
             resources.ApplyResources(this.toolStripStatusLabel1, "toolStripStatusLabel1");
             this.toolStripStatusLabel1.Name = "toolStripStatusLabel1";
             this.toolStripStatusLabel1.Padding = new System.Windows.Forms.Padding(0, 0, 30, 0);
             // 
             // toolStripStatusLabel2
             // 
             resources.ApplyResources(this.toolStripStatusLabel2, "toolStripStatusLabel2");
             this.toolStripStatusLabel2.Name = "toolStripStatusLabel2";
             // 
             // languageName
             // 
             this.languageName.Name = "languageName";
             this.languageName.Padding = new System.Windows.Forms.Padding(70, 0, 0, 0);
             resources.ApplyResources(this.languageName, "languageName");
             // 
             // toolStripStatusLabel3
             // 
             resources.ApplyResources(this.toolStripStatusLabel3, "toolStripStatusLabel3");
             this.toolStripStatusLabel3.Name = "toolStripStatusLabel3";
             // 
             // toolStripStatusLabel4
             // 
             resources.ApplyResources(this.toolStripStatusLabel4, "toolStripStatusLabel4");
             this.toolStripStatusLabel4.Name = "toolStripStatusLabel4";
             // 
             // PyjamaForm
             // 
             resources.ApplyResources(this, "$this");
             this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
             this.Controls.Add(this.main);
             this.Controls.Add(this.menuStrip1);
             this.Name = "PyjamaForm";
             this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
             this.mainContentSplit.Panel1.ResumeLayout(false);
             this.mainContentSplit.Panel1.PerformLayout();
             this.mainContentSplit.Panel2.ResumeLayout(false);
             this.mainContentSplit.Panel2.PerformLayout();
             this.mainContentSplit.ResumeLayout(false);
             this.statusStrip1.ResumeLayout(false);
             this.statusStrip1.PerformLayout();
             this.splitContainer1.Panel1.ResumeLayout(false);
             this.splitContainer1.Panel1.PerformLayout();
             this.splitContainer1.Panel2.ResumeLayout(false);
             this.splitContainer1.ResumeLayout(false);
             this.commandContainer.Panel1.ResumeLayout(false);
             this.commandContainer.Panel1.PerformLayout();
             this.commandContainer.Panel2.ResumeLayout(false);
             this.commandContainer.ResumeLayout(false);
             this.tableLayoutPanel1.ResumeLayout(false);
             this.tableLayoutPanel1.PerformLayout();
             this.statusStrip2.ResumeLayout(false);
             this.statusStrip2.PerformLayout();
             this.main.ResumeLayout(false);
             this.main.PerformLayout();
             this.toolStrip.ResumeLayout(false);
             this.toolStrip.PerformLayout();
             this.toolStrip1.ResumeLayout(false);
             this.toolStrip1.PerformLayout();
             this.menuStrip1.ResumeLayout(false);
             this.menuStrip1.PerformLayout();
             this.ResumeLayout(false);
             this.PerformLayout();

         }

         #endregion

         private System.Windows.Forms.Panel main;
         private System.Windows.Forms.MenuStrip menuStrip1;
         private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator;
         private System.Windows.Forms.ToolStripMenuItem saveToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem saveAsToolStripMenuItem;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
         private System.Windows.Forms.ToolStripMenuItem printToolStripMenuItem;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
         private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem editToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem undoToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem redoToolStripMenuItem;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
         private System.Windows.Forms.ToolStripMenuItem cutToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem copyToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem pasteToolStripMenuItem;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
         private System.Windows.Forms.ToolStripMenuItem selectAllToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem toolsToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem optionsToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem helpToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem contentsToolStripMenuItem;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator5;
         private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
         private System.Windows.Forms.Panel toolStrip;
         private System.Windows.Forms.ToolStrip toolStrip1;
         private System.Windows.Forms.SplitContainer mainContentSplit;
         private System.Windows.Forms.ToolStripButton newToolStripButton;
         private System.Windows.Forms.ToolStripButton openToolStripButton;
         private System.Windows.Forms.ToolStripButton saveToolStripButton;
         private System.Windows.Forms.ToolStripButton printToolStripButton;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator6;
         private System.Windows.Forms.ToolStripButton cutToolStripButton;
         private System.Windows.Forms.ToolStripButton copyToolStripButton;
         private System.Windows.Forms.ToolStripButton pasteToolStripButton;
         private System.Windows.Forms.ToolStripSeparator toolStripSeparator7;
         private System.Windows.Forms.ToolStripMenuItem languageToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem executeToolStripMenuItem;
         private System.Windows.Forms.ToolStripMenuItem printSetupMenuItem;
         private System.Windows.Forms.ToolStripMenuItem newToolStripMenuItem;
         private System.Windows.Forms.ToolStripButton toolStripButton1;
         private System.Windows.Forms.ToolStripMenuItem closeTabToolStripMenuItem;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel1;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel2;
         private System.Windows.Forms.ToolStripStatusLabel languageName;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel3;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel4;
         private DocumentManager _docManager;
         private System.Windows.Forms.StatusStrip statusStrip1;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel8;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel9;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel11;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel13;
         private System.Windows.Forms.ToolStripDropDownButton toolStripStatusLabel10;
         private System.Windows.Forms.ToolStripMenuItem pythonToolStripMenuItem1;
         private System.Windows.Forms.ToolStripMenuItem rubyToolStripMenuItem1;
         private System.Windows.Forms.ToolStripMenuItem schemeToolStripMenuItem;
         private System.Windows.Forms.StatusStrip statusStrip2;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel5;
         private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel6;
         private System.Windows.Forms.ToolStripDropDownButton shellLanguageButton;
         private System.Windows.Forms.ToolStripMenuItem shellLanguageSelect1;
         private System.Windows.Forms.ToolStripMenuItem shellLanguageSelect2;
         private System.Windows.Forms.ToolStripMenuItem shellLanguageSelect3;
         private System.Windows.Forms.SplitContainer splitContainer1;
         private System.Windows.Forms.SplitContainer commandContainer;
         private MyTextBox commandTextBox;
         private Console outputWindow;
         private TableLayoutPanel tableLayoutPanel1;
         public Button runButton;
         private Label commandLabel;
         private ToolStripMenuItem shellToolStripMenuItem;
         private ToolStripMenuItem shellRestartToolStripMenuItem;
         private ToolStripMenuItem evaluateToolStripMenuItem;
         private ToolStripSeparator toolStripSeparator8;
         private ToolStripMenuItem selectEditorToolStripMenuItem;
         private ToolStripMenuItem newlineToolStripMenuItem;
         private ToolStripMenuItem previewMenuItem;
         private ToolStripMenuItem printScriptMenuItem;
         private ToolStripSplitButton lineNumber;
         private ToolStripTextBox lineNumberEntry;
         private ToolStripStatusLabel columnNumber;



     }
 }

