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
            this.groupBox5 = new System.Windows.Forms.GroupBox();
            this.IRSensorImg = new System.Windows.Forms.PictureBox();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.lineSensorImg = new System.Windows.Forms.PictureBox();
            this.groupBox6 = new System.Windows.Forms.GroupBox();
            this.stallSensorImg = new System.Windows.Forms.PictureBox();
            this.groupBox7 = new System.Windows.Forms.GroupBox();
            this.durationLabel = new System.Windows.Forms.Label();
            this.durBar = new System.Windows.Forms.TrackBar();
            this.frequency2Label = new System.Windows.Forms.Label();
            this.freqBar2 = new System.Windows.Forms.TrackBar();
            this.frequencyLabel = new System.Windows.Forms.Label();
            this.freqBar1 = new System.Windows.Forms.TrackBar();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.driveBox)).BeginInit();
            this.groupBox2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.contactSensorImg)).BeginInit();
            this.groupBox3.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.lightSensorImg)).BeginInit();
            this.groupBox5.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.IRSensorImg)).BeginInit();
            this.groupBox4.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.lineSensorImg)).BeginInit();
            this.groupBox6.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.stallSensorImg)).BeginInit();
            this.groupBox7.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.durBar)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.freqBar2)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.freqBar1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
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
            this.groupBox2.Text = "Contact Sensors / IR Bumpers";
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
            this.groupBox3.Location = new System.Drawing.Point(219, 148);
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
            // groupBox5
            // 
            this.groupBox5.Controls.Add(this.IRSensorImg);
            this.groupBox5.Location = new System.Drawing.Point(219, 216);
            this.groupBox5.Name = "groupBox5";
            this.groupBox5.Size = new System.Drawing.Size(206, 62);
            this.groupBox5.TabIndex = 4;
            this.groupBox5.TabStop = false;
            this.groupBox5.Text = "IR Sensors";
            // 
            // IRSensorImg
            // 
            this.IRSensorImg.Location = new System.Drawing.Point(6, 19);
            this.IRSensorImg.Name = "IRSensorImg";
            this.IRSensorImg.Size = new System.Drawing.Size(194, 37);
            this.IRSensorImg.TabIndex = 0;
            this.IRSensorImg.TabStop = false;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.lineSensorImg);
            this.groupBox4.Location = new System.Drawing.Point(219, 284);
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
            // groupBox6
            // 
            this.groupBox6.Controls.Add(this.stallSensorImg);
            this.groupBox6.Location = new System.Drawing.Point(219, 80);
            this.groupBox6.Name = "groupBox6";
            this.groupBox6.Size = new System.Drawing.Size(206, 62);
            this.groupBox6.TabIndex = 3;
            this.groupBox6.TabStop = false;
            this.groupBox6.Text = "Stall Sensor";
            this.groupBox6.Enter += new System.EventHandler(this.groupBox6_Enter);
            // 
            // stallSensorImg
            // 
            this.stallSensorImg.Location = new System.Drawing.Point(6, 19);
            this.stallSensorImg.Name = "stallSensorImg";
            this.stallSensorImg.Size = new System.Drawing.Size(194, 37);
            this.stallSensorImg.TabIndex = 0;
            this.stallSensorImg.TabStop = false;
            // 
            // groupBox7
            // 
            this.groupBox7.Controls.Add(this.durationLabel);
            this.groupBox7.Controls.Add(this.durBar);
            this.groupBox7.Controls.Add(this.frequency2Label);
            this.groupBox7.Controls.Add(this.freqBar2);
            this.groupBox7.Controls.Add(this.frequencyLabel);
            this.groupBox7.Controls.Add(this.freqBar1);
            this.groupBox7.Controls.Add(this.pictureBox1);
            this.groupBox7.Location = new System.Drawing.Point(431, 12);
            this.groupBox7.Name = "groupBox7";
            this.groupBox7.Size = new System.Drawing.Size(206, 152);
            this.groupBox7.TabIndex = 3;
            this.groupBox7.TabStop = false;
            this.groupBox7.Text = "Tone";
            // 
            // durationLabel
            // 
            this.durationLabel.AutoSize = true;
            this.durationLabel.Location = new System.Drawing.Point(138, 131);
            this.durationLabel.Name = "durationLabel";
            this.durationLabel.Size = new System.Drawing.Size(47, 13);
            this.durationLabel.TabIndex = 6;
            this.durationLabel.Text = "Duration";
            // 
            // durBar
            // 
            this.durBar.Location = new System.Drawing.Point(6, 103);
            this.durBar.Maximum = 1000;
            this.durBar.Name = "durBar";
            this.durBar.Size = new System.Drawing.Size(194, 45);
            this.durBar.TabIndex = 5;
            this.durBar.TickStyle = System.Windows.Forms.TickStyle.None;
            this.durBar.ValueChanged += new System.EventHandler(this.duration_Change);
            this.durBar.MouseUp += new System.Windows.Forms.MouseEventHandler(this.tone_MouseUp);
            // 
            // frequency2Label
            // 
            this.frequency2Label.AutoSize = true;
            this.frequency2Label.Location = new System.Drawing.Point(138, 87);
            this.frequency2Label.Name = "frequency2Label";
            this.frequency2Label.Size = new System.Drawing.Size(57, 13);
            this.frequency2Label.TabIndex = 4;
            this.frequency2Label.Text = "Frequency";
            // 
            // freqBar2
            // 
            this.freqBar2.Location = new System.Drawing.Point(6, 59);
            this.freqBar2.Maximum = 800;
            this.freqBar2.Minimum = 200;
            this.freqBar2.Name = "freqBar2";
            this.freqBar2.Size = new System.Drawing.Size(194, 45);
            this.freqBar2.TabIndex = 3;
            this.freqBar2.TickStyle = System.Windows.Forms.TickStyle.None;
            this.freqBar2.Value = 200;
            this.freqBar2.ValueChanged += new System.EventHandler(this.frequency2_Change);
            this.freqBar2.MouseUp += new System.Windows.Forms.MouseEventHandler(this.tone_MouseUp);
            // 
            // frequencyLabel
            // 
            this.frequencyLabel.AutoSize = true;
            this.frequencyLabel.Location = new System.Drawing.Point(138, 43);
            this.frequencyLabel.Name = "frequencyLabel";
            this.frequencyLabel.Size = new System.Drawing.Size(57, 13);
            this.frequencyLabel.TabIndex = 2;
            this.frequencyLabel.Text = "Frequency";
            // 
            // freqBar1
            // 
            this.freqBar1.Location = new System.Drawing.Point(6, 19);
            this.freqBar1.Maximum = 800;
            this.freqBar1.Minimum = 200;
            this.freqBar1.Name = "freqBar1";
            this.freqBar1.Size = new System.Drawing.Size(194, 45);
            this.freqBar1.TabIndex = 1;
            this.freqBar1.TickStyle = System.Windows.Forms.TickStyle.None;
            this.freqBar1.Value = 200;
            this.freqBar1.ValueChanged += new System.EventHandler(this.frequency_Change);
            this.freqBar1.Scroll += new System.EventHandler(this.trackBar1_Scroll);
            this.freqBar1.MouseUp += new System.Windows.Forms.MouseEventHandler(this.tone_MouseUp);
            // 
            // pictureBox1
            // 
            this.pictureBox1.Location = new System.Drawing.Point(6, 19);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(194, 37);
            this.pictureBox1.TabIndex = 0;
            this.pictureBox1.TabStop = false;
            // 
            // TestPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(647, 354);
            this.Controls.Add(this.groupBox7);
            this.Controls.Add(this.groupBox6);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.groupBox5);
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
            this.groupBox5.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.IRSensorImg)).EndInit();
            this.groupBox4.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.lineSensorImg)).EndInit();
            this.groupBox6.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.stallSensorImg)).EndInit();
            this.groupBox7.ResumeLayout(false);
            this.groupBox7.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.durBar)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.freqBar2)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.freqBar1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
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
        private System.Windows.Forms.GroupBox groupBox5;
        private System.Windows.Forms.PictureBox IRSensorImg;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.PictureBox lineSensorImg;
        private System.Windows.Forms.GroupBox groupBox6;
        private System.Windows.Forms.PictureBox stallSensorImg;
        private System.Windows.Forms.GroupBox groupBox7;
        private System.Windows.Forms.Label frequencyLabel;
        private System.Windows.Forms.TrackBar freqBar1;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.Label durationLabel;
        private System.Windows.Forms.TrackBar durBar;
        private System.Windows.Forms.Label frequency2Label;
        private System.Windows.Forms.TrackBar freqBar2;
    }
}