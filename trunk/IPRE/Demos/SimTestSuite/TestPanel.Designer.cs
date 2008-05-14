namespace SimTestSuite
{
    partial class TestPanel
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.driveBox = new System.Windows.Forms.PictureBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.contactSensorImg = new System.Windows.Forms.PictureBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.lightSensorImg = new System.Windows.Forms.PictureBox();
            this.IRSensorImg = new System.Windows.Forms.GroupBox();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.lineSensorImg = new System.Windows.Forms.PictureBox();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.driveBox)).BeginInit();
            this.groupBox2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.contactSensorImg)).BeginInit();
            this.groupBox3.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.lightSensorImg)).BeginInit();
            this.IRSensorImg.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.groupBox4.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.lineSensorImg)).BeginInit();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.label4);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.driveBox);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(201, 276);
            this.groupBox1.TabIndex = 1;
            this.groupBox1.TabStop = false;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(6, 228);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(44, 26);
            this.label4.TabIndex = 2;
            this.label4.Text = "X: Y:\r\nLP: RP:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(68, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Drive Control";
            // 
            // driveBox
            // 
            this.driveBox.Location = new System.Drawing.Point(6, 32);
            this.driveBox.Name = "driveBox";
            this.driveBox.Size = new System.Drawing.Size(189, 193);
            this.driveBox.TabIndex = 1;
            this.driveBox.TabStop = false;
            this.driveBox.MouseMove += new System.Windows.Forms.MouseEventHandler(this.driveBox_MouseMove);
            this.driveBox.MouseDown += new System.Windows.Forms.MouseEventHandler(this.driveBox_MouseDown);
            this.driveBox.MouseUp += new System.Windows.Forms.MouseEventHandler(this.driveBox_MouseUp);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.contactSensorImg);
            this.groupBox2.Location = new System.Drawing.Point(219, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(206, 62);
            this.groupBox2.TabIndex = 2;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Contact Sensors";
            this.groupBox2.Enter += new System.EventHandler(this.groupBox2_Enter);
            // 
            // contactSensorImg
            // 
            this.contactSensorImg.Location = new System.Drawing.Point(6, 19);
            this.contactSensorImg.Name = "contactSensorImg";
            this.contactSensorImg.Size = new System.Drawing.Size(194, 37);
            this.contactSensorImg.TabIndex = 0;
            this.contactSensorImg.TabStop = false;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.lightSensorImg);
            this.groupBox3.Location = new System.Drawing.Point(219, 80);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(206, 62);
            this.groupBox3.TabIndex = 3;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Light Sensors";
            // 
            // lightSensorImg
            // 
            this.lightSensorImg.Location = new System.Drawing.Point(6, 19);
            this.lightSensorImg.Name = "lightSensorImg";
            this.lightSensorImg.Size = new System.Drawing.Size(194, 37);
            this.lightSensorImg.TabIndex = 0;
            this.lightSensorImg.TabStop = false;
            // 
            // IRSensorImg
            // 
            this.IRSensorImg.Controls.Add(this.pictureBox1);
            this.IRSensorImg.Location = new System.Drawing.Point(219, 148);
            this.IRSensorImg.Name = "IRSensorImg";
            this.IRSensorImg.Size = new System.Drawing.Size(206, 62);
            this.IRSensorImg.TabIndex = 4;
            this.IRSensorImg.TabStop = false;
            this.IRSensorImg.Text = "IR Sensors";
            // 
            // pictureBox1
            // 
            this.pictureBox1.Location = new System.Drawing.Point(6, 19);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(194, 37);
            this.pictureBox1.TabIndex = 0;
            this.pictureBox1.TabStop = false;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.lineSensorImg);
            this.groupBox4.Location = new System.Drawing.Point(219, 216);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(206, 62);
            this.groupBox4.TabIndex = 5;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Line Sensors";
            // 
            // lineSensorImg
            // 
            this.lineSensorImg.Location = new System.Drawing.Point(6, 19);
            this.lineSensorImg.Name = "lineSensorImg";
            this.lineSensorImg.Size = new System.Drawing.Size(194, 37);
            this.lineSensorImg.TabIndex = 0;
            this.lineSensorImg.TabStop = false;
            // 
            // TestPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(759, 564);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.IRSensorImg);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Name = "TestPanel";
            this.Text = "TestPanel";
            this.Load += new System.EventHandler(this.TestPanel_Load);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.driveBox)).EndInit();
            this.groupBox2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.contactSensorImg)).EndInit();
            this.groupBox3.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.lightSensorImg)).EndInit();
            this.IRSensorImg.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.groupBox4.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.lineSensorImg)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.PictureBox driveBox;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.PictureBox contactSensorImg;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.PictureBox lightSensorImg;
        private System.Windows.Forms.GroupBox IRSensorImg;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.PictureBox lineSensorImg;
    }
}