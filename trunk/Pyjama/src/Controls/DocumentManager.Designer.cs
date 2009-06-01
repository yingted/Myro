namespace Pyjama
{
    partial class DocumentManager
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
            this.tabControl = new System.Windows.Forms.TabControl();
            this.SuspendLayout();
            // 
            // tabControl
            // 
            this.tabControl.AllowDrop = true;
            this.tabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl.HotTrack = true;
            this.tabControl.Location = new System.Drawing.Point(0, 0);
            this.tabControl.Name = "tabControl";
            this.tabControl.SelectedIndex = 0;
            this.tabControl.Size = new System.Drawing.Size(434, 368);
            this.tabControl.TabIndex = 0;
            // 
            // DocumentManager
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.Controls.Add(this.tabControl);
            this.Name = "DocumentManager";
            this.Size = new System.Drawing.Size(434, 368);
            this.ResumeLayout(false);

        }

        #endregion

        //private DocumentTabControl tabControl;
        private System.Windows.Forms.TabControl tabControl;

        public void CloseTab()
        {
            if (tabControl.TabCount > 0)
                tabControl.TabPages.Remove(tabControl.SelectedTab);
        }
        public void FocusActiveTab()
        {
            if (tabControl.TabCount > 0)
                ((DocumentPage)tabControl.SelectedTab).textBox.Focus();
        }

    }
}
