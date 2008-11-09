namespace IronEditor.UI.WinForms.Dialogs
{
    partial class NewProjectDialog
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
            this.Create = new System.Windows.Forms.Button();
            this.cancel = new System.Windows.Forms.Button();
            this.languagesCombo = new System.Windows.Forms.ComboBox();
            this.langLabel = new System.Windows.Forms.Label();
            this.title = new System.Windows.Forms.Label();
            this.projectType = new System.Windows.Forms.ComboBox();
            this.projectSettings = new System.Windows.Forms.Panel();
            this.projectBrowse = new System.Windows.Forms.Button();
            this.projectPath = new System.Windows.Forms.TextBox();
            this.pathLabel = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.projectSettings.SuspendLayout();
            this.SuspendLayout();
            // 
            // Create
            // 
            this.Create.Location = new System.Drawing.Point(195, 167);
            this.Create.Name = "Create";
            this.Create.Size = new System.Drawing.Size(75, 23);
            this.Create.TabIndex = 6;
            this.Create.Text = "Create";
            this.Create.UseVisualStyleBackColor = true;
            this.Create.Click += new System.EventHandler(this.Create_Click);
            // 
            // cancel
            // 
            this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancel.Location = new System.Drawing.Point(276, 167);
            this.cancel.Name = "cancel";
            this.cancel.Size = new System.Drawing.Size(75, 23);
            this.cancel.TabIndex = 7;
            this.cancel.Text = "Cancel";
            this.cancel.UseVisualStyleBackColor = true;
            this.cancel.Click += new System.EventHandler(this.cancel_Click);
            // 
            // languagesCombo
            // 
            this.languagesCombo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.languagesCombo.FormattingEnabled = true;
            this.languagesCombo.Location = new System.Drawing.Point(107, 59);
            this.languagesCombo.Name = "languagesCombo";
            this.languagesCombo.Size = new System.Drawing.Size(242, 21);
            this.languagesCombo.TabIndex = 2;
            // 
            // langLabel
            // 
            this.langLabel.AutoSize = true;
            this.langLabel.Location = new System.Drawing.Point(12, 62);
            this.langLabel.Name = "langLabel";
            this.langLabel.Size = new System.Drawing.Size(58, 13);
            this.langLabel.TabIndex = 1;
            this.langLabel.Text = "Language:";
            // 
            // title
            // 
            this.title.AutoSize = true;
            this.title.Font = new System.Drawing.Font("Tahoma", 10.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.title.Location = new System.Drawing.Point(12, 9);
            this.title.Name = "title";
            this.title.Size = new System.Drawing.Size(122, 17);
            this.title.TabIndex = 0;
            this.title.Text = "New Project Setup";
            // 
            // projectType
            // 
            this.projectType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.projectType.FormattingEnabled = true;
            this.projectType.Items.AddRange(new object[] {
            "Loose File",
            "Project"});
            this.projectType.Location = new System.Drawing.Point(106, 86);
            this.projectType.Name = "projectType";
            this.projectType.Size = new System.Drawing.Size(121, 21);
            this.projectType.TabIndex = 4;
            this.projectType.SelectedIndexChanged += new System.EventHandler(this.projectType_SelectedIndexChanged);
            // 
            // projectSettings
            // 
            this.projectSettings.AutoSize = true;
            this.projectSettings.Controls.Add(this.projectBrowse);
            this.projectSettings.Controls.Add(this.projectPath);
            this.projectSettings.Controls.Add(this.pathLabel);
            this.projectSettings.Location = new System.Drawing.Point(13, 113);
            this.projectSettings.Name = "projectSettings";
            this.projectSettings.Size = new System.Drawing.Size(341, 27);
            this.projectSettings.TabIndex = 5;
            this.projectSettings.Visible = false;
            // 
            // projectBrowse
            // 
            this.projectBrowse.Location = new System.Drawing.Point(263, 1);
            this.projectBrowse.Name = "projectBrowse";
            this.projectBrowse.Size = new System.Drawing.Size(75, 23);
            this.projectBrowse.TabIndex = 2;
            this.projectBrowse.Text = "Browse";
            this.projectBrowse.UseVisualStyleBackColor = true;
            this.projectBrowse.Click += new System.EventHandler(this.projectBrowse_Click);
            // 
            // projectPath
            // 
            this.projectPath.Location = new System.Drawing.Point(93, 3);
            this.projectPath.Name = "projectPath";
            this.projectPath.Size = new System.Drawing.Size(164, 21);
            this.projectPath.TabIndex = 1;
            // 
            // pathLabel
            // 
            this.pathLabel.AutoSize = true;
            this.pathLabel.Location = new System.Drawing.Point(-1, 6);
            this.pathLabel.Name = "pathLabel";
            this.pathLabel.Size = new System.Drawing.Size(70, 13);
            this.pathLabel.TabIndex = 0;
            this.pathLabel.Text = "File Location:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 89);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(72, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Project Type:";
            // 
            // NewProjectDialog
            // 
            this.AcceptButton = this.Create;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancel;
            this.ClientSize = new System.Drawing.Size(363, 202);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.projectSettings);
            this.Controls.Add(this.projectType);
            this.Controls.Add(this.title);
            this.Controls.Add(this.langLabel);
            this.Controls.Add(this.languagesCombo);
            this.Controls.Add(this.cancel);
            this.Controls.Add(this.Create);
            this.MinimumSize = new System.Drawing.Size(300, 140);
            this.Name = "NewProjectDialog";
            this.Text = "New Project";
            this.projectSettings.ResumeLayout(false);
            this.projectSettings.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button Create;
        private System.Windows.Forms.Button cancel;
        private System.Windows.Forms.ComboBox languagesCombo;
        private System.Windows.Forms.Label langLabel;
        private System.Windows.Forms.Label title;
        private System.Windows.Forms.ComboBox projectType;
        private System.Windows.Forms.Panel projectSettings;
        private System.Windows.Forms.Button projectBrowse;
        private System.Windows.Forms.TextBox projectPath;
        private System.Windows.Forms.Label pathLabel;
        private System.Windows.Forms.Label label1;
    }
}