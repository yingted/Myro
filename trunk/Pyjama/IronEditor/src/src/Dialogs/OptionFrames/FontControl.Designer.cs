namespace Pyjama.Dialogs.OptionFrames
{
    partial class FontControl
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
            this.fontText = new System.Windows.Forms.Label();
            this.font = new System.Windows.Forms.ComboBox();
            this.size = new System.Windows.Forms.ComboBox();
            this.sizeLabel = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // fontText
            // 
            this.fontText.AutoSize = true;
            this.fontText.Location = new System.Drawing.Point(0, 0);
            this.fontText.Name = "fontText";
            this.fontText.Size = new System.Drawing.Size(31, 13);
            this.fontText.TabIndex = 0;
            this.fontText.Text = "Font:";
            // 
            // font
            // 
            this.font.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.font.FormattingEnabled = true;
            this.font.Location = new System.Drawing.Point(3, 16);
            this.font.Name = "font";
            this.font.Size = new System.Drawing.Size(231, 21);
            this.font.TabIndex = 1;
            // 
            // size
            // 
            this.size.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.size.FormattingEnabled = true;
            this.size.Location = new System.Drawing.Point(258, 16);
            this.size.Name = "size";
            this.size.Size = new System.Drawing.Size(111, 21);
            this.size.TabIndex = 3;
            // 
            // sizeLabel
            // 
            this.sizeLabel.AutoSize = true;
            this.sizeLabel.Location = new System.Drawing.Point(255, 0);
            this.sizeLabel.Name = "sizeLabel";
            this.sizeLabel.Size = new System.Drawing.Size(30, 13);
            this.sizeLabel.TabIndex = 2;
            this.sizeLabel.Text = "Size:";
            // 
            // FontControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.size);
            this.Controls.Add(this.sizeLabel);
            this.Controls.Add(this.font);
            this.Controls.Add(this.fontText);
            this.Name = "FontControl";
            this.Size = new System.Drawing.Size(378, 41);
            this.Load += new System.EventHandler(this.FontControl_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label fontText;
        private System.Windows.Forms.ComboBox font;
        private System.Windows.Forms.ComboBox size;
        private System.Windows.Forms.Label sizeLabel;
    }
}
