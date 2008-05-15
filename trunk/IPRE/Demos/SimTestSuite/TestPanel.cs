using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Threading;

using IPREFoundationClasses;

namespace SimTestSuite
{
    public partial class TestPanel : Form
    {
        // Drive control taken from Form1 in IPREFoundationClasses
        RobotBrain rbt;

        int rectX = 20, rectY = 20, rectWidth = 150, rectHeight = 150, lengthTick = 5, fontSpacing = 5;
        Graphics g;
        bool mouseDown = false;

        public TestPanel(RobotBrain rbt)
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
                this.update();
                Thread.Sleep(400);
            }
        }

        private void driveBox_MouseDown(object sender, MouseEventArgs e)
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
                double slope = ycircle / xcircle;
                xcircle = Math.Sign(xcircle) * Math.Sqrt((radius * radius) / (1.0 + slope * slope));
                ycircle = slope * xcircle;
            }
            int xcoord = (int)(xcircle + rectX + rectWidth / 2.0);
            int ycoord = (int)((-1.0 * ycircle) + rectY + rectHeight / 2.0);

            g.FillPolygon(new SolidBrush(Color.FromArgb(100, Color.Red)), new Point[] { new Point(rectX + rectWidth / 2, rectY + rectHeight / 2), new Point(xcoord, ycoord), new Point(xcoord, rectY + rectHeight / 2) });
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

            rbt.SetMotors((float)left_wheel, (float)right_wheel);

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

            rbt.Stop();
            //Console.WriteLine("All Stop");
            //Console.WriteLine();
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

                rbt.SetMotors((float)left_wheel, (float)right_wheel);
                //Console.WriteLine("Power:" + left_wheel + "," + right_wheel);
                //Console.WriteLine();
            }
        }

        private int[] layout(int widthTotal, int widthOne, int count)
        {
            int widthEach = widthTotal / count;
            int offset = (widthEach - widthOne) / 2;
            int[] ret = new int[count];
            for (int i = 0; i < count; i++)
                ret[i] = widthEach * i + offset;
            return ret;
        }

        private void drawCircleMeters(PictureBox control, float[] vals, float min, float max)
        {
            Bitmap bmp = new Bitmap(contactSensorImg.Width, contactSensorImg.Height);
            Graphics g = Graphics.FromImage(bmp);
            Brush red = new SolidBrush(Color.Red);

            int maxCircleRadius = 15;
            int[] xs = layout(contactSensorImg.Width, maxCircleRadius * 2, vals.Length);
            //Console.WriteLine("L: " + vals[0] + "  R: " + vals[1]);
            IEnumerable<int> radii = from v in vals
                                     let normalized = (v - min) / (max - min)
                                     select (int)(normalized * (float)maxCircleRadius);
            for (int i = 0; i < xs.Length; i++)
            {
                //Console.WriteLine("Radius: " + radii.ElementAt(i));
                int offset = maxCircleRadius - radii.ElementAt(i);
                g.FillEllipse(red, xs[i] + offset, offset, 2 * radii.ElementAt(i), 2 * radii.ElementAt(i));
            }
            control.Image = bmp;
            control.Invoke(new Action(delegate() { control.Refresh(); }));
        }

        private void update()
        {
            float[] contacts = rbt.getContact();
            drawCircleMeters(contactSensorImg, contacts, 0.0f, 1.0f);
        }

        private void TestPanel_Load(object sender, EventArgs e)
        {

        }

        private void groupBox2_Enter(object sender, EventArgs e)
        {

        }
    }
}
