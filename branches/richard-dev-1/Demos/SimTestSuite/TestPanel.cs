﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Threading;

using Myro;

namespace SimTestSuite
{
    public partial class TestPanel : Form
    {
        // Drive control taken from Form1 in IPREFoundationClasses
        Robot rbt;

        int rectX = 20, rectY = 20, rectWidth = 150, rectHeight = 150, lengthTick = 5, fontSpacing = 5;
        Graphics g;
        bool mouseDown = false;

        public TestPanel(Robot rbt)
        {
            this.rbt = rbt;
            InitializeComponent();
            myInitialize();
            new Thread(new ThreadStart(updateLoop)).Start();
        }

        //public TestPanel()
        //{
        //    InitializeComponent();
        //}


        private void myInitialize()
        {
            Bitmap bmp = new Bitmap(driveBox.Width, driveBox.Height);
            g = Graphics.FromImage(bmp);
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
            driveBox.Image = bmp;
        }

        private void updateLoop()
        {
            while (true)
            {
                //Console.WriteLine("update");
                this.update();
                Thread.Sleep(100);
            }
        }

        private void driveBox_MouseDown(object sender, MouseEventArgs e)
        {
            //g.Clear(Color.Empty);
            //g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
            //g.DrawEllipse(new Pen(Color.Black), new Rectangle(rectX, rectY, rectWidth, rectHeight));
            //g.DrawLine(new Pen(Color.Black), new Point(rectX - lengthTick, rectY + rectHeight / 2), new Point(rectX + rectWidth + lengthTick, rectY + rectHeight / 2));
            //g.DrawLine(new Pen(Color.Black), new Point(rectX + rectWidth / 2, rectY - lengthTick), new Point(rectX + rectWidth / 2, rectY + rectHeight + lengthTick));
            //g.DrawString("X", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 + fontSpacing));
            //g.DrawString("Y", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 + fontSpacing, rectY - 3 * lengthTick));
            //g.DrawString("F", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 2 * fontSpacing, rectY - 3 * lengthTick));
            //g.DrawString("B", new Font(FontFamily.GenericSansSerif, 8), Brushes.Black, new PointF(rectX + rectWidth / 2 - 3 * fontSpacing, rectY + rectHeight + lengthTick));
            //g.DrawString("R", this.Font, new SolidBrush(Color.Black), new PointF(rectX + rectWidth + lengthTick, rectY + rectHeight / 2 - 2 * fontSpacing));
            //g.DrawString("L", this.Font, new SolidBrush(Color.Black), new PointF(rectX - lengthTick - 3 * fontSpacing, rectY + rectHeight / 2 - 2 * fontSpacing));

            //double xcircle = e.X - rectX - rectWidth / 2.0;
            //double ycircle = -1.0 * (e.Y - rectY - rectHeight / 2.0);
            //double radius = rectWidth / 2.0;
            //if (xcircle * xcircle + ycircle * ycircle > radius * radius)
            //{
            //    double slope = ycircle / xcircle;
            //    xcircle = Math.Sign(xcircle) * Math.Sqrt((radius * radius) / (1.0 + slope * slope));
            //    ycircle = slope * xcircle;
            //}
            //int xcoord = (int)(xcircle + rectX + rectWidth / 2.0);
            //int ycoord = (int)((-1.0 * ycircle) + rectY + rectHeight / 2.0);

            //g.FillPolygon(new SolidBrush(Color.FromArgb(100, Color.Red)), new Point[] { new Point(rectX + rectWidth / 2, rectY + rectHeight / 2), new Point(xcoord, ycoord), new Point(xcoord, rectY + rectHeight / 2) });
            //driveBox.Refresh();

            //// project on -1:0:1 scale
            //double ypcircle = ycircle / radius;
            //double xpcircle = xcircle / radius;

            //double degree = (Math.Atan(ypcircle / xpcircle) * 180 / Math.PI + 180) % 180;
            ////Console.WriteLine("Half-degree:" + degree / 2.0);
            //double halfRadian = degree / 2.0 * Math.PI / 180.0;
            //double right_wheel = Math.Sin(halfRadian);
            //double left_wheel = Math.Cos(halfRadian);

            //double power = Math.Sqrt(ypcircle * ypcircle + xpcircle * xpcircle);
            //if (left_wheel > right_wheel)
            //{
            //    right_wheel = (right_wheel / left_wheel) * power;
            //    left_wheel = power;
            //}
            //else
            //{
            //    left_wheel = (left_wheel / right_wheel) * power;
            //    right_wheel = power;
            //}

            //if (ypcircle < 0)
            //{
            //    double temp = left_wheel;
            //    left_wheel = -1.0 * right_wheel;
            //    right_wheel = -1.0 * temp;
            //}

            //rbt.SetMotors((float)left_wheel, (float)right_wheel);

            doDrive(e);

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

            //double xcircle = e.X - rectX - rectWidth / 2.0;
            //double ycircle = -1.0 * (e.Y - rectY - rectHeight / 2.0);
            //double radius = rectWidth / 2.0;
            //if (xcircle * xcircle + ycircle * ycircle > radius * radius)
            //{
            //    double slope = ycircle / xcircle;
            //    xcircle = Math.Sign(xcircle) * Math.Sqrt((radius * radius) / (1.0 + slope * slope));
            //    ycircle = slope * xcircle;
            //}
            //int xcoord = (int)(xcircle + rectX + rectWidth / 2.0);
            //int ycoord = (int)((-1.0 * ycircle) + rectY + rectHeight / 2.0);

            //g.FillPolygon(new SolidBrush(Color.FromArgb(100, Color.Red)), new Point[] { new Point(rectX + rectWidth / 2, rectY + rectHeight / 2), new Point(xcoord, ycoord), new Point(xcoord, rectY + rectHeight / 2) });

            driveBox.Refresh();

            rbt.Movement.Stop();

            //Console.WriteLine("All Stop");
            //Console.WriteLine();
        }

        private void driveBox_MouseMove(object sender, MouseEventArgs e)
        {
            if (mouseDown)
            {
                doDrive(e);
            }
        }

        public void doDrive(MouseEventArgs e)
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
            if (xcircle * xcircle + ycircle * ycircle > radius * radius)
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
            //Console.WriteLine("Half-degree:" + degree / 2.0);
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

            rbt.Movement.SetMotors((float)left_wheel, (float)right_wheel);

            //Console.WriteLine("Power:" + left_wheel + "," + right_wheel);
            //Console.WriteLine();
        }

        private int[] layout(int widthTotal, int widthOne, int count)
        {
            if (count > 0)
            {
                int widthEach = widthTotal / count;
                int offset = (widthEach - widthOne) / 2;
                int[] ret = new int[count];
                for (int i = 0; i < count; i++)
                    ret[i] = widthEach * i + offset;
                return ret;
            }
            else
            {
                return new int[0];
            }
        }

        private void drawCircleMeters(PictureBox control, Color color, double[] vals, string[] labels, double min, double max)
        {
            if (control.IsHandleCreated)
            {
                Bitmap bmp = new Bitmap(control.Width, control.Height);
                Graphics g = Graphics.FromImage(bmp);
                Brush brush = new SolidBrush(color);
                Brush black = new SolidBrush(Color.Black);

                int maxCircleRadius = 15;
                int[] xs = layout(control.Width, maxCircleRadius * 2, vals.Length);
                //Console.WriteLine("L: " + vals[0] + "  R: " + vals[1]);
                IEnumerable<int> radii = from v in vals
                                         let normalized = (v - min) / (max - min)
                                         let normalizedBounded = (normalized > 0 ? normalized : 0)
                                         select (int)(normalizedBounded * (double)maxCircleRadius);
                StringFormat format = StringFormat.GenericDefault;
                format.Alignment = StringAlignment.Center;
                Font font = new Font("Sans Serif", 7);
                for (int i = 0; i < xs.Length; i++)
                {
                    //Console.WriteLine("Radius: " + radii.ElementAt(i));
                    int offset = maxCircleRadius - radii.ElementAt(i);
                    g.FillEllipse(brush, xs[i] + offset, offset, 2 * radii.ElementAt(i), 2 * radii.ElementAt(i));
                    g.DrawString(vals[i].ToString(), font, black, xs[i] + maxCircleRadius, (float)maxCircleRadius * 1.8f, format);
                    //Console.WriteLine(labels.Length + " labels");
                    if (i < labels.Length)
                    {
                        g.DrawString(labels[i].ToString(), font, black, xs[i] + maxCircleRadius, 0, format);
                        //Console.WriteLine("Label: " + labels[i]);
                    }
                }
                control.Image = bmp;
                control.Invoke(new Action(delegate() { control.Refresh(); }));
            }
        }

        private void update()
        {
            double[] contacts = rbt.Sensors.get("bumpers");
            drawCircleMeters(contactSensorImg, Color.MediumVioletRed, contacts, rbt.Sensors.getNames("bumpers"), 0.0, 1.0);
            double[] stall = rbt.Sensors.get("stall");
            drawCircleMeters(stallSensorImg, Color.Red, stall, rbt.Sensors.getNames("stall"), 0.0, 1.0);
            //double[] light = rbt.Sensors.get("light");
            //drawCircleMeters(lightSensorImg, Color.DeepSkyBlue, light, rbt.Sensors.getNames("light"), 2000.0, 0.0);
            //double[] sonar = rbt.Sensors.get("sonar");
            //drawCircleMeters(SonarImg, Color.Tan, sonar, rbt.Sensors.getNames("sonar"), 40.0, 0.0);
            //double[] line = rbt.Sensors.get("line");
            //drawCircleMeters(lineSensorImg, Color.DarkGray, line, rbt.Sensors.getNames("line"), 0.0, 1.0);
        }

        private void TestPanel_Load(object sender, EventArgs e)
        {

        }

        private void groupBox2_Enter(object sender, EventArgs e)
        {

        }

        private void groupBox6_Enter(object sender, EventArgs e)
        {

        }

        private void frequency_Change(object sender, EventArgs e)
        {
            frequencyLabel.Text = ((TrackBar)sender).Value.ToString() + " Hz";
        }

        private void trackBar1_Scroll(object sender, EventArgs e)
        {

        }

        private void frequency2_Change(object sender, EventArgs e)
        {
            frequency2Label.Text = ((TrackBar)sender).Value.ToString() + " Hz";
        }

        private void duration_Change(object sender, EventArgs e)
        {
            durationLabel.Text = ((TrackBar)sender).Value.ToString() + " ms";
        }

        private void tone_MouseUp(object sender, MouseEventArgs e)
        {
            rbt.Sound.beep(durBar.Value, freqBar1.Value, freqBar2.Value);
        }

        private void loudCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            rbt.Sound.SetLoud(loudCheckBox.Checked);
        }
    }
}
