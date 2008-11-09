namespace Pyjama.Dialogs
{
    partial class AboutDialog
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
            this.okButton = new System.Windows.Forms.Button();
            this.title = new System.Windows.Forms.Label();
            this.version = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.urlHomepage = new System.Windows.Forms.LinkLabel();
            this.SuspendLayout();
            // 
            // okButton
            // 
            this.okButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.okButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.okButton.Location = new System.Drawing.Point(266, 239);
            this.okButton.Name = "okButton";
            this.okButton.Size = new System.Drawing.Size(75, 23);
            this.okButton.TabIndex = 0;
            this.okButton.Text = "OK";
            this.okButton.UseVisualStyleBackColor = true;
            // 
            // title
            // 
            this.title.AutoSize = true;
            this.title.Font = new System.Drawing.Font("Tahoma", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.title.Location = new System.Drawing.Point(12, 9);
            this.title.Name = "title";
            this.title.Size = new System.Drawing.Size(39, 19);
            this.title.TabIndex = 1;
            this.title.Text = "{0}";
            // 
            // version
            // 
            this.version.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.version.AutoSize = true;
            this.version.Location = new System.Drawing.Point(61, 244);
            this.version.Name = "version";
            this.version.Size = new System.Drawing.Size(0, 13);
            this.version.TabIndex = 4;
            // 
            // label3
            // 
            this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(11, 244);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(49, 13);
            this.label3.TabIndex = 5;
            this.label3.Text = "Version: ";
            // 
            // urlHomepage
            // 
            this.urlHomepage.AutoSize = true;
            this.urlHomepage.LinkArea = new System.Windows.Forms.LinkArea(17, 28);
            this.urlHomepage.Location = new System.Drawing.Point(16, 57);
            this.urlHomepage.Name = "urlHomepage";
            this.urlHomepage.Size = new System.Drawing.Size(191, 18);
            this.urlHomepage.TabIndex = 6;
            this.urlHomepage.TabStop = true;
            this.urlHomepage.Text = "Project Homepage PyjamaProject.org";
            this.urlHomepage.UseCompatibleTextRendering = true;
            this.urlHomepage.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.urlHomepage_LinkClicked);
            // 
            // AboutDialog
            // 
            this.AcceptButton = this.okButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.okButton;
            this.ClientSize = new System.Drawing.Size(353, 274);
            this.Controls.Add(this.urlHomepage);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.version);
            this.Controls.Add(this.title);
            this.Controls.Add(this.okButton);
            this.Name = "AboutDialog";
            this.Text = "{0} - About";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button okButton;
        private System.Windows.Forms.Label title;
        private System.Windows.Forms.Label version;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.LinkLabel urlHomepage;
    }
}