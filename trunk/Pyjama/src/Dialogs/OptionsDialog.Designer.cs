namespace Pyjama.Dialogs
{
    partial class OptionsDialog
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
            this.cancel = new System.Windows.Forms.Button();
            this.save = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            this.optionsTree = new System.Windows.Forms.TreeView();
            this.optionPanel = new System.Windows.Forms.Panel();
            this.panel3 = new System.Windows.Forms.Panel();
            this.panel1.SuspendLayout();
            this.panel3.SuspendLayout();
            this.SuspendLayout();
            // 
            // cancel
            // 
            this.cancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancel.Location = new System.Drawing.Point(455, 9);
            this.cancel.Name = "cancel";
            this.cancel.Size = new System.Drawing.Size(75, 23);
            this.cancel.TabIndex = 0;
            this.cancel.Text = "Cancel";
            this.cancel.UseVisualStyleBackColor = true;
            this.cancel.Click += new System.EventHandler(this.cancel_Click);
            // 
            // save
            // 
            this.save.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.save.Location = new System.Drawing.Point(374, 9);
            this.save.Name = "save";
            this.save.Size = new System.Drawing.Size(75, 23);
            this.save.TabIndex = 1;
            this.save.Text = "Save";
            this.save.UseVisualStyleBackColor = true;
            this.save.Click += new System.EventHandler(this.save_Click);
            // 
            // panel1
            // 
            this.panel1.AutoSize = true;
            this.panel1.Controls.Add(this.optionsTree);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(145, 176);
            this.panel1.TabIndex = 2;
            // 
            // optionsTree
            // 
            this.optionsTree.Dock = System.Windows.Forms.DockStyle.Left;
            this.optionsTree.Location = new System.Drawing.Point(0, 0);
            this.optionsTree.Name = "optionsTree";
            this.optionsTree.ShowRootLines = false;
            this.optionsTree.Size = new System.Drawing.Size(145, 176);
            this.optionsTree.TabIndex = 3;
            this.optionsTree.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.optionsTree_AfterSelect);
            // 
            // optionPanel
            // 
            this.optionPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.optionPanel.Location = new System.Drawing.Point(145, 0);
            this.optionPanel.Name = "optionPanel";
            this.optionPanel.Size = new System.Drawing.Size(397, 175);
            this.optionPanel.TabIndex = 3;
            // 
            // panel3
            // 
            this.panel3.Controls.Add(this.save);
            this.panel3.Controls.Add(this.cancel);
            this.panel3.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel3.Location = new System.Drawing.Point(0, 176);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(542, 44);
            this.panel3.TabIndex = 4;
            this.panel3.Paint += new System.Windows.Forms.PaintEventHandler(this.panel3_Paint);
            // 
            // OptionsDialog
            // 
            this.AcceptButton = this.save;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.CancelButton = this.cancel;
            this.ClientSize = new System.Drawing.Size(542, 220);
            this.Controls.Add(this.optionPanel);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.panel3);
            this.MaximizeBox = true;
            this.MinimumSize = new System.Drawing.Size(548, 245);
            this.Name = "OptionsDialog";
            this.Text = "Pyjama Options";
            this.panel1.ResumeLayout(false);
            this.panel3.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button cancel;
        private System.Windows.Forms.Button save;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.TreeView optionsTree;
        private System.Windows.Forms.Panel optionPanel;
        private System.Windows.Forms.Panel panel3;
    }
}