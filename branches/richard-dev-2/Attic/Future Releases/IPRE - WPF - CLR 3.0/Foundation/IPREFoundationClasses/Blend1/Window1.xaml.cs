using System;
using System.IO;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Navigation;

namespace Blend1
{
	public partial class Window1
	{
        int rectX = 23, rectY = 37, rectWidth = 128, rectHeight = 128;
        double canvasOffsetX, canvasOffsetY;
        bool mouseDown = false;
        
		public Window1()
		{
			this.InitializeComponent();
			
			// Insert code required on object creation below this point.
            myInitialization();
            sensorListBox.Items.Add("LightSensor: 45cd");
            sensorListBox.Items.Add("SoundSensor: 13dB");
            sensorListBox.Items.Add("ProximitySensor: 101ft");
            sensorListBox.Items.Add("HeatSensor: 23degC");

            outputListBox.Items.Add("Connecting to Scribbler ...");
            outputListBox.Items.Add("Scribbled Connected !!!");
            outputListBox.Items.Add("Starting AvoidCollsion routine.");
            outputListBox.Items.Add("Shutting Down.");
		}

        private void myInitialization()
        {
            canvasOffsetX = pointer.Width / 2.0;
            canvasOffsetY = pointer.Height / 2.0;
        }

        private void Quit_Click(object sender, RoutedEventArgs e)
        {
            Application.Current.Shutdown();
        }

        private void Minimize_Click(object sender, RoutedEventArgs e)
        {
            Application.Current.MainWindow.WindowState = WindowState.Minimized;
        }

        private void DragBar_MouseLeftButtonDown(object sender, System.Windows.Input.MouseButtonEventArgs e)
        {
            this.DragMove();
        }

        private void radar_MouseMove(object sender, System.Windows.Input.MouseEventArgs e)
        {
            double mouseX = e.GetPosition(radar).X, mouseY = e.GetPosition(radar).Y;

            if (mouseDown)
            {
                double xcircle = mouseX - rectX - rectWidth / 2.0;
                double ycircle = -1.0 * (mouseY - rectY - rectHeight / 2.0);
                double radius = rectWidth / 2.0;
                if (xcircle * xcircle + ycircle * ycircle > radius * radius)
                {
                    double slope = (double)ycircle / (double)xcircle;
                    xcircle = Math.Sign(xcircle) * Math.Sqrt((radius * radius) / (1.0 + slope * slope));
                    ycircle = slope * xcircle;
                }
                int xcoord = (int)(xcircle + rectX + rectWidth / 2.0);
                int ycoord = (int)((-1.0 * ycircle) + rectY + rectHeight / 2.0);
                Canvas.SetLeft(pointer, xcoord - canvasOffsetX);
                Canvas.SetTop(pointer, ycoord - canvasOffsetY);


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

                //rbt.SetMotors((float)left_wheel, (float)right_wheel);
                //Console.WriteLine("Power:" + left_wheel + "," + right_wheel);
                //Console.WriteLine();
            }
        }

        private void radar_MouseLeftButtonDown(object sender, System.Windows.Input.MouseButtonEventArgs e)
        {
            double mouseX = e.GetPosition(radar).X, mouseY = e.GetPosition(radar).Y;

            pointer.Visibility = Visibility.Visible;
            //Canvas.SetLeft(pointer, mouseX - 4);
            //Canvas.SetTop(pointer, mouseY - 4);
            double xcircle = mouseX - rectX - rectWidth / 2.0;
            double ycircle = -1.0 * (mouseY - rectY - rectHeight / 2.0);
            double radius = rectWidth / 2.0;
            if (xcircle * xcircle + ycircle * ycircle > radius * radius)
            {
                double slope = (double)ycircle / (double)xcircle;
                xcircle = Math.Sign(xcircle) * Math.Sqrt((radius * radius) / (1.0 + slope * slope));
                ycircle = slope * xcircle;
            }
            int xcoord = (int)(xcircle + rectX + rectWidth / 2.0);
            int ycoord = (int)((-1.0 * ycircle) + rectY + rectHeight / 2.0);
            Canvas.SetLeft(pointer, xcoord - canvasOffsetX);
            Canvas.SetTop(pointer, ycoord - canvasOffsetY);


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

            //rbt.SetMotors((float)left_wheel, (float)right_wheel);
            //Console.WriteLine("Power:" + left_wheel + "," + right_wheel);
            //Console.WriteLine();
            mouseDown = true;
        }

        private void radar_MouseLeftButtonUp(object sender, System.Windows.Input.MouseButtonEventArgs e)
        {
            pointer.Visibility = Visibility.Hidden;
            //rbt.Stop();
            mouseDown = false;
        }
	}
}