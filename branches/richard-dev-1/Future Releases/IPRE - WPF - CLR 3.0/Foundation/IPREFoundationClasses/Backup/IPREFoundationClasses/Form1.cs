using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections;

namespace IPREFoundationClasses.RobotGUI
{
    public partial class Form1 : Form
    {
        RobotBrain rbt;

        int rectX = 20, rectY = 20, rectWidth = 150, rectHeight = 150, lengthTick = 5, fontSpacing = 5;
        Graphics g;
        bool mouseDown = false;
        
        public Form1()
        {
            InitializeComponent();
            myInitialize();
        }

        public Form1(RobotBrain rbt)
        {
            this.rbt = rbt;
            InitializeComponent();
            myInitialize();
        }

        private void myInitialize()
        {
            Bitmap bmp = new Bitmap(driveBox.Width, driveBox.Height);
            g = Graphics.FromImage(bmp);
            g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
            g.DrawEllipse(new Pen(Color.Black),new Rectangle(rectX, rectY,rectWidth,rectHeight));
            g.DrawLine(new Pen(Color.Black), new Point(rectX - lengthTick, rectY + rectHeight/2), new Point(rectX + rectWidth + lengthTick, rectY + rectHeight/2));
            g.DrawLine(new Pen(Color.Black), new Point(rectX + rectWidth/2, rectY - lengthTick), new Point(rectX + rectWidth/2, rectY + rectHeight + lengthTick));
            g.DrawString("X", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 + fontSpacing));
            g.DrawString("Y", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth/2 + fontSpacing, rectY - 3*lengthTick));
            g.DrawString("F", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 2 * fontSpacing, rectY - 3 * lengthTick));
            g.DrawString("B", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 3 * fontSpacing, rectY + rectHeight + lengthTick));
            g.DrawString("R", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 - 2* fontSpacing));
            g.DrawString("L", this.Font, new SolidBrush(Color.Black), new PointF(rectX - lengthTick - 3 * fontSpacing, rectY + rectHeight / 2 - 2 * fontSpacing));
            driveBox.Image = bmp;
        }

        private void driveBox_MouseDown(object sender, MouseEventArgs e)
        {
            mouseDown = true;
        }

        private void driveBox_MouseUp(object sender, MouseEventArgs e)
        {
            mouseDown = false;
            g.Clear(Color.Empty);
            g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
            g.DrawEllipse(new Pen(Color.Black), new Rectangle(rectX, rectY, rectWidth, rectHeight));
            g.DrawLine(new Pen(Color.Black), new Point(rectX - lengthTick, rectY + rectHeight / 2), new Point(rectX + rectWidth + lengthTick, rectY + rectHeight / 2));
            g.DrawLine(new Pen(Color.Black), new Point(rectX + rectWidth / 2, rectY - lengthTick), new Point(rectX + rectWidth / 2, rectY + rectHeight + lengthTick));
            g.DrawString("X", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 + fontSpacing));
            g.DrawString("Y", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 + fontSpacing, rectY - 3 * lengthTick));
            g.DrawString("F", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 2 * fontSpacing, rectY - 3 * lengthTick));
            g.DrawString("B", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 3 * fontSpacing, rectY + rectHeight + lengthTick));
            g.DrawString("R", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 - 2 * fontSpacing));
            g.DrawString("L", this.Font, new SolidBrush(Color.Black), new PointF(rectX - lengthTick - 3 * fontSpacing, rectY + rectHeight / 2 - 2 * fontSpacing));

            double xcircle = e.X - rectX - rectWidth / 2.0;
            double ycircle = -1.0 * (e.Y - rectY - rectHeight / 2.0);
            double radius = rectWidth / 2.0;
            if (xcircle * xcircle + ycircle * ycircle > radius * radius)
            {
                double slope = ycircle / xcircle;
                xcircle = Math.Sign(xcircle) * Math.Sqrt((radius * radius) / (1.0 + slope * slope));
                ycircle = slope * xcircle;
            }
            int xcoord = (int)(xcircle + rectX + rectWidth / 2.0);
            int ycoord = (int)((-1.0 * ycircle) + rectY + rectHeight / 2.0);

            g.FillPolygon(new SolidBrush(Color.FromArgb(100, Color.Red)), new Point[] { new Point(rectX + rectWidth / 2, rectY + rectHeight / 2), new Point(xcoord, ycoord), new Point(xcoord, rectY + rectHeight / 2) });
            driveBox.Refresh();

            rbt.Stop();
            Console.WriteLine("All Stop");
            Console.WriteLine();
        }

        private void driveBox_MouseMove(object sender, MouseEventArgs e)
        {
            if (mouseDown)
            {
                g.Clear(Color.Empty);

                g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;

                g.DrawEllipse(new Pen(Color.Black), new Rectangle(rectX, rectY, rectWidth, rectHeight));

                g.DrawLine(new Pen(Color.Black), new Point(rectX - lengthTick, rectY + rectHeight / 2), new Point(rectX + rectWidth + lengthTick, rectY + rectHeight / 2));
                g.DrawLine(new Pen(Color.Black), new Point(rectX + rectWidth / 2, rectY - lengthTick), new Point(rectX + rectWidth / 2, rectY + rectHeight + lengthTick));
                g.DrawString("X", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 + fontSpacing));
                g.DrawString("Y", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 + fontSpacing, rectY - 3 * lengthTick));
                g.DrawString("F", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 2 * fontSpacing, rectY - 3 * lengthTick));
                g.DrawString("B", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 3 * fontSpacing, rectY + rectHeight + lengthTick));
                g.DrawString("R", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 - 2 * fontSpacing));
                g.DrawString("L", this.Font, new SolidBrush(Color.Black), new PointF(rectX - lengthTick - 3 * fontSpacing, rectY + rectHeight / 2 - 2 * fontSpacing));

                double xcircle = e.X - rectX - rectWidth / 2.0;
                double ycircle = -1.0 * (e.Y - rectY - rectHeight / 2.0);
                double radius = rectWidth / 2.0;
                if (xcircle * xcircle + ycircle*ycircle > radius*radius)
                {
                    double slope = (double)ycircle / (double)xcircle;
                    xcircle = Math.Sign(xcircle) * Math.Sqrt((radius * radius) / (1.0 + slope * slope));
                    ycircle = slope * xcircle;
                }
                int xcoord = (int)(xcircle + rectX + rectWidth / 2.0);
                int ycoord = (int)((-1.0 * ycircle) + rectY + rectHeight / 2.0);

                g.FillPolygon(new SolidBrush(Color.FromArgb(50, Color.Red)), new Point[] { new Point(rectX + rectWidth / 2, rectY + rectHeight / 2), new Point(xcoord, ycoord), new Point(xcoord, rectY + rectHeight / 2) });
                driveBox.Refresh();

                // project on -1:0:1 scale
                double ypcircle = ycircle / radius;
                double xpcircle = xcircle / radius;

                double degree = (Math.Atan(ypcircle / xpcircle) * 180 / Math.PI + 180) % 180;
                Console.WriteLine("Half-degree:" + degree / 2.0);
                double halfRadian = degree / 2.0 * Math.PI / 180.0;
                double right_wheel = Math.Sin(halfRadian);
                double left_wheel = Math.Cos(halfRadian);

                double power = Math.Sqrt(ypcircle * ypcircle + xpcircle * xpcircle);
                if (left_wheel > right_wheel)
                {
                    right_wheel = (right_wheel / left_wheel) * power;
                    left_wheel = power;
                }
                else
                {
                    left_wheel = (left_wheel / right_wheel) * power;
                    right_wheel = power;
                }

                if (ypcircle < 0)
                {
                    double temp = left_wheel;
                    left_wheel = -1.0 * right_wheel;
                    right_wheel = -1.0 * temp;
                }

                rbt.SetMotors((float)left_wheel, (float)right_wheel);
                Console.WriteLine("Power:" + left_wheel + "," + right_wheel);
                Console.WriteLine();
            }
        }

        void refreshSensors()
        {
            sensorList.Items.Clear();

            foreach (string sensor in Defines.getSensorString().Values)
            {
                try
                {
                    float[] res = rbt.get(sensor);
                    int counter = 0;
                    foreach (float f in res)
                    {
                        sensorList.Items.Add("" + sensor + " #" + counter + " : " + f);
                        counter++;
                    }
                }
                catch(RobotBrain.NoInstanceException)
                {
                }
            }

            sensorList.Refresh();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            refreshSensors();
        }

        public void refreshLog(List<LogEntry> logList)
        {
            logOutput.Items.Clear();
            foreach (LogEntry e in logList)
                logOutput.Items.Add(e);
        }

        private void logOutput_DrawItem(object sender, DrawItemEventArgs e)
        {
            LogEntry entry = (LogEntry)logOutput.Items[e.Index];
            Brush b = Brushes.Red;

            switch (entry.Level)
            {
                case LogLevel.Info:
                    b = Brushes.Black;
                    break;
                case LogLevel.Debug:
                    b = Brushes.Blue;
                    break;
                case LogLevel.Warning:
                    b = Brushes.Green;
                    break;
                case LogLevel.Error:
                    b = Brushes.Red;
                    break;
            }
            //e.Graphics.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
            e.Graphics.DrawString(entry.LogText, new Font(FontFamily.GenericSansSerif, 8), b, e.Bounds.X, e.Bounds.Y);
        }
    }
}