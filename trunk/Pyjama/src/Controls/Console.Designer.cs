namespace Pyjama
{
    partial class Console
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Console));
            this.restartToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.languageToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.pythonToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.rubyToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.schemeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
            this.printToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.textbox = new UIIronTextBox.IronTextBoxControl();
            this.SuspendLayout();
            // 
            // restartToolStripMenuItem
            // 
            this.restartToolStripMenuItem.Name = "restartToolStripMenuItem";
            this.restartToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.restartToolStripMenuItem.Text = "Restart";
            // 
            // languageToolStripMenuItem
            // 
            this.languageToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.pythonToolStripMenuItem,
            this.rubyToolStripMenuItem,
            this.schemeToolStripMenuItem});
            this.languageToolStripMenuItem.Name = "languageToolStripMenuItem";
            this.languageToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.languageToolStripMenuItem.Text = "Language";
            // 
            // pythonToolStripMenuItem
            // 
            this.pythonToolStripMenuItem.Name = "pythonToolStripMenuItem";
            this.pythonToolStripMenuItem.Size = new System.Drawing.Size(116, 22);
            this.pythonToolStripMenuItem.Text = "Python";
            // 
            // rubyToolStripMenuItem
            // 
            this.rubyToolStripMenuItem.Name = "rubyToolStripMenuItem";
            this.rubyToolStripMenuItem.Size = new System.Drawing.Size(116, 22);
            this.rubyToolStripMenuItem.Text = "Ruby";
            // 
            // schemeToolStripMenuItem
            // 
            this.schemeToolStripMenuItem.Name = "schemeToolStripMenuItem";
            this.schemeToolStripMenuItem.Size = new System.Drawing.Size(116, 22);
            this.schemeToolStripMenuItem.Text = "Scheme";
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(123, 6);
            // 
            // printToolStripMenuItem
            // 
            this.printToolStripMenuItem.Name = "printToolStripMenuItem";
            this.printToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.printToolStripMenuItem.Text = "Print...";
            // 
            // textbox
            // 
            this.textbox.AutoScroll = true;
            this.textbox.BackColor = System.Drawing.SystemColors.WindowText;
            this.textbox.ConsoleTextBackColor = System.Drawing.Color.Black;
            this.textbox.ConsoleTextFont = new System.Drawing.Font("DejaVu Sans Mono", 10);
            this.textbox.ConsoleTextForeColor = System.Drawing.Color.White;
            this.textbox.Cursor = System.Windows.Forms.Cursors.Default;
            this.textbox.defBuilder = ((System.Text.StringBuilder)(resources.GetObject("textbox.defBuilder")));
            this.textbox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.textbox.Font = new System.Drawing.Font("DejaVu Sans Mono", 10);
            this.textbox.ForeColor = System.Drawing.SystemColors.Window;
            this.textbox.Location = new System.Drawing.Point(0, 0);
            this.textbox.Margin = new System.Windows.Forms.Padding(0);
            this.textbox.Name = "textbox";
            this.textbox.Prompt = "---- Python Mode ----";
            this.textbox.Size = new System.Drawing.Size(300, 300);
            this.textbox.TabIndex = 1;
            this.textbox.TextChanged += new System.EventHandler(this.output_TextChanged);
            this.textbox.Load += new System.EventHandler(this.textbox_Load);
            // 
            // Console
            // 
            this.AutoScroll = true;
            this.Controls.Add(this.textbox);
            this.DoubleBuffered = true;
            this.Name = "Console";
            this.Size = new System.Drawing.Size(300, 300);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ToolStripMenuItem languageToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem printToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem pythonToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem rubyToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem schemeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem restartToolStripMenuItem;
        public UIIronTextBox.IronTextBoxControl textbox;
    }
}
