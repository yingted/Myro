// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using game = Myro.Services.GameController.Proxy;
using Microsoft.Ccr.Core;
using Myro;

namespace Myro.GUI.WPFControls
{
    /// <summary>
    /// Interaction logic for DriveControl.xaml
    /// </summary>
    public partial class DriveControl : UserControl
    {
        Port<Tuple<double, double>> drivePort = new Port<Tuple<double, double>>();
        DispatcherQueue taskQueue;

        /// <summary>
        /// Constructor.  Dispose must be called on this class.
        /// </summary>
        public DriveControl()
        {
            InitializeComponent();
        }

        private void driveHandler(Tuple<double, double> motors)
        {
            try
            {
                Robot.motors(motors.Item0, motors.Item1);
            }
            catch (Exception) { }
        }

        /// <summary>
        /// From SimpleDashboard sample
        /// </summary>
        /// <param name="axes"></param>
        private void UpdateJoystickAxes(game.Axes axes)
        {
            DrawJoystick(axes.X, -axes.Y);

            double left;
            double right;

            if (axes.Y < 0)
            {
                left = (-axes.Y + axes.X / (-2.0 / 1000.0 * Math.Abs(axes.X) + 3.0)) / 1000.0;
                right = (-axes.Y - axes.X / (-2.0 / 1000.0 * Math.Abs(axes.X) + 3.0)) / 1000.0;
            }
            else
            {
                left = (-axes.Y + axes.X / (-2.0 / 1000.0 * Math.Abs(axes.X) + 3.0)) / 1000.0;
                right = (-axes.Y - axes.X / (-2.0 / 1000.0 * Math.Abs(axes.X) + 3.0)) / 1000.0;
            }
            if (left > 1.0) left = 1.0;
            if (left < -1.0) left = -1.0;
            if (right > 1.0) right = 1.0;
            if (right < -1.0) right = -1.0;

            DrawMotors(left, right);
            drivePort.Post(new Tuple<double, double>(left, right));
        }

        private void DrawJoystick(int x, int y)
        {
            Path1.Data = new EllipseGeometry(
                new Rect(0, 0, driveCanvas.ActualWidth, driveCanvas.ActualHeight));

            double partial = y * driveCanvas.ActualHeight / 2200;
            if (partial > 0)
            {
                Path2.Data = new PathGeometry(new List<PathFigure>() {
                    new PathFigure(new Point(0, driveCanvas.ActualHeight / 2.0),
                        new List<PathSegment>() { 
                            new ArcSegment(
                                new Point(driveCanvas.ActualWidth,driveCanvas.ActualHeight/2.0),
                                new Size(driveCanvas.ActualWidth/2.0, partial),
                                0.0, false, SweepDirection.Clockwise, true)},
                        false)});
            }
            else
            {
                Path2.Data = new PathGeometry(new List<PathFigure>() {
                    new PathFigure(new Point(0, driveCanvas.ActualHeight / 2.0),
                        new List<PathSegment>() { 
                            new ArcSegment(
                                new Point(driveCanvas.ActualWidth,driveCanvas.ActualHeight/2.0),
                                new Size(driveCanvas.ActualWidth/2.0, -partial),
                                0.0, false, SweepDirection.Counterclockwise, true)},
                        false)});
            }

            partial = x * driveCanvas.ActualHeight / 2200;
            if (partial > 0)
            {
                Path3.Data = new PathGeometry(new List<PathFigure>() {
                    new PathFigure(new Point(driveCanvas.ActualWidth / 2.0, 0),
                        new List<PathSegment>() { 
                            new ArcSegment(
                                new Point(driveCanvas.ActualWidth/2.0,driveCanvas.ActualHeight),
                                new Size(partial, driveCanvas.ActualHeight/2.0),
                                0.0, false, SweepDirection.Clockwise, true)},
                        false)});
            }
            else
            {
                Path3.Data = new PathGeometry(new List<PathFigure>() {
                    new PathFigure(new Point(driveCanvas.ActualWidth / 2.0, 0),
                        new List<PathSegment>() { 
                            new ArcSegment(
                                new Point(driveCanvas.ActualWidth/2.0,driveCanvas.ActualHeight),
                                new Size(-partial, driveCanvas.ActualHeight/2.0),
                                0.0, false, SweepDirection.Counterclockwise, true)},
                        false)});
            }
        }

        private void DrawMotors(double left, double right)
        {
            if (leftPath != null && rightPath != null && leftPath.Data != null && rightPath.Data != null)
            {
                RectangleGeometry rectL = (RectangleGeometry)leftPath.Data;
                RectangleGeometry rectR = (RectangleGeometry)rightPath.Data;
                double l1, l2, r1, r2;
                double height = leftCanvas.ActualHeight;
                double halfHeight = height / 2.0;
                if (left > 0.0)
                {
                    l1 = (1.0 - left) * halfHeight;
                    l2 = halfHeight;
                }
                else
                {
                    l1 = halfHeight;
                    l2 = -left * halfHeight + halfHeight;
                }
                if (right > 0.0)
                {
                    r1 = (1.0 - right) * halfHeight;
                    r2 = halfHeight;
                }
                else
                {
                    r1 = halfHeight;
                    r2 = -right * halfHeight + halfHeight;
                }
                rectL.Rect = new Rect(new Point(0, l1), new Point(leftCanvas.ActualWidth, l2));
                rectR.Rect = new Rect(new Point(0, r1), new Point(leftCanvas.ActualWidth, r2));
            }
        }

        private void onMouseMove(object sender, MouseEventArgs e)
        {
            if (driveCanvas.IsMouseCaptured)
            {
                double x, y;
                var pos = e.GetPosition(driveCanvas);
                //x = Math.Min(driveCanvas.ActualWidth, Math.Max(pos.X, 0.0));
                //y = Math.Min(driveCanvas.ActualHeight, Math.Max(pos.Y, 0.0));

                x = pos.X * 2000.0 / driveCanvas.ActualWidth - 1000.0;
                y = pos.Y * 2000.0 / driveCanvas.ActualHeight - 1000.0;

                if (Math.Abs(x) > 1000.0)
                {
                    double f = 1000.0 / Math.Abs(x);
                    x *= f;
                    y *= f;
                }
                if (Math.Abs(y) > 1000.0)
                {
                    double f = 1000.0 / Math.Abs(y);
                    x *= f;
                    y *= f;
                }

                game.Axes axes = new game.Axes();
                axes.X = (int)x;
                axes.Y = (int)y;

                UpdateJoystickAxes(axes);
            }

        }

        private void onInitialized(object sender, EventArgs e)
        {
            taskQueue = new DispatcherQueue("DriveControl", new Dispatcher(1, "DriveControl"),
                TaskExecutionPolicy.ConstrainQueueDepthDiscardTasks, 1);
            Arbiter.Activate(taskQueue, Arbiter.Receive(true, drivePort, driveHandler));
            UpdateJoystickAxes(new game.Axes());
        }

        private void onSizeChanged(object sender, SizeChangedEventArgs e)
        {
            UpdateJoystickAxes(new game.Axes());
        }

        private void onMouseUp(object sender, MouseButtonEventArgs e)
        {
            driveCanvas.ReleaseMouseCapture();
            UpdateJoystickAxes(new game.Axes());
        }

        private void onMouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            driveCanvas.CaptureMouse();
        }

        public void Dispose()
        {
            taskQueue.Dispose();
        }

        private void OnUnloaded(object sender, RoutedEventArgs e)
        {
            // This was sometimes causing taskQueue to be disposed twice
            //this.Dispose();
        }
    }
}
