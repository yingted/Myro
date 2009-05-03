namespace SimpleApp
{
    partial class Form1
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.ironTextBoxControl1 = new UIIronTextBox.IronTextBoxControl();
            this.btClose = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // ironTextBoxControl1
            // 
            this.ironTextBoxControl1.ConsoleTextBackColor = System.Drawing.Color.White;
            this.ironTextBoxControl1.ConsoleTextFont = new System.Drawing.Font("Lucida Console", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ironTextBoxControl1.ConsoleTextForeColor = System.Drawing.SystemColors.WindowText;
            this.ironTextBoxControl1.defBuilder = ((System.Text.StringBuilder)(resources.GetObject("ironTextBoxControl1.defBuilder")));
            this.ironTextBoxControl1.Location = new System.Drawing.Point(12, 12);
            this.ironTextBoxControl1.Name = "ironTextBoxControl1";
            this.ironTextBoxControl1.Prompt = ">>>";
            this.ironTextBoxControl1.Size = new System.Drawing.Size(603, 416);
            this.ironTextBoxControl1.TabIndex = 0;
            // 
            // btClose
            // 
            this.btClose.Location = new System.Drawing.Point(276, 444);
            this.btClose.Name = "btClose";
            this.btClose.Size = new System.Drawing.Size(75, 23);
            this.btClose.TabIndex = 1;
            this.btClose.Text = "Close";
            this.btClose.UseVisualStyleBackColor = true;
            this.btClose.Click += new System.EventHandler(this.btClose_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(627, 479);
            this.Controls.Add(this.btClose);
            this.Controls.Add(this.ironTextBoxControl1);
            this.Name = "Form1";
            this.Text = "Form1";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.ResumeLayout(false);

        }

        #endregion

        private UIIronTextBox.IronTextBoxControl ironTextBoxControl1;
        private System.Windows.Forms.Button btClose;
    }
}

