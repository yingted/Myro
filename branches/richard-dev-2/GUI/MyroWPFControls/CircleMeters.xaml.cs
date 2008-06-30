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

namespace Myro.GUI.WPFControls
{
    /// <summary>
    /// Interaction logic for CircleMeters.xaml
    /// </summary>
    public partial class CircleMeters : UserControl
    {
        public class ValueChangeArgs {
            public int Index { get; internal set; }
            public double Value { get; internal set; }
        }
        public delegate void ValueChangeHandler(object sender, ValueChangeArgs e);
        public event ValueChangeHandler ValueChange;

        List<Canvas> canvases = new List<Canvas>();
        Color curColor = Brushes.DarkGray.Color;
        double[] curValues = null;

        public CircleMeters()
        {
            InitializeComponent();
            //AddVisualChild(mainVisual);
            //AddLogicalChild(mainVisual);
        }

        public void SetColor(Color color)
        {
            curColor = color;
            layoutVisuals(canvases.Count);
        }

        private void layoutVisuals(int count)
        {
            //double[] centers = new double[count];
            double widthOne = this.ActualHeight;
            mainCanvas.Children.Clear();
            canvases.Clear();
            if (count > 0)
            {
                double widthEach = this.ActualWidth / (double)count;
                double offset = (widthEach - widthOne) / 2;
                for (int i = 0; i < count; i++)
                {
                    double left = widthEach * i + offset;
                    //mainVisual.Children.Add(new DrawingVisual());
                    Canvas c = new Canvas();
                    c.SetValue(Canvas.LeftProperty, left);
                    c.SetValue(Canvas.TopProperty, 0.0);
                    c.Width = widthOne;
                    c.Height = widthOne;
                    Path p = new Path();
                    if (curColor != null)
                    {
                        p.Fill = Brushes.Transparent;
                        p.Stroke = new SolidColorBrush(Color.FromArgb(128, curColor.R, curColor.G, curColor.B));
                        p.StrokeThickness = -1.0;
                    }
                    c.Children.Add(p);
                    p = new Path();
                    if (curColor != null)
                        p.Fill = new SolidColorBrush(curColor);
                    c.Children.Add(p);
                    c.MouseLeftButtonDown += MouseLeftButtonDown;
                    canvases.Add(c);
                    mainCanvas.Children.Add(c);
                }
            }
        }

        public void setData(double[] values, string[] labels, double min, double max)
        {
            this.curValues = values;
            double maxCircleRadius = this.ActualHeight / 2;
            if (values.Length != canvases.Count)
                layoutVisuals(values.Length);
            //Console.WriteLine("L: " + vals[0] + "  R: " + vals[1]);
            List<double> radii = new List<double>(
                from v in values
                let normalized = (v - min) / (max - min)
                let normalizedBoundedM = (normalized > 0 ? normalized : 0)
                let normalizedBounded = (normalizedBoundedM < 1 ? normalizedBoundedM : 1)
                select normalizedBounded * maxCircleRadius);
            //StringFormat format = StringFormat.GenericDefault;
            //format.Alignment = StringAlignment.Center;
            //Font font = new Font("Sans Serif", 7);
            for (int i = 0; i < values.Length; i++)
            {
                //Console.WriteLine("Radius: " + radii.ElementAt(i));
                double offset = maxCircleRadius - radii[i];
                ((Path)canvases[i].Children[0]).Data = new EllipseGeometry(
                    new Point(canvases[i].ActualWidth / 2.0, canvases[i].ActualHeight / 2.0), maxCircleRadius, maxCircleRadius);
                ((Path)canvases[i].Children[1]).Data = new EllipseGeometry(
                    new Point(canvases[i].ActualWidth / 2.0, canvases[i].ActualHeight / 2.0), radii[i], radii[i]);
                //g.FillEllipse(brush, xs[i] + offset, offset, 2 * radii.ElementAt(i), 2 * radii.ElementAt(i));
                //g.DrawString(vals[i].ToString(), font, black, xs[i] + maxCircleRadius, (float)maxCircleRadius * 1.8f, format);
                //Console.WriteLine(labels.Length + " labels");
                //if (i < labels.Length)
                //{
                //    g.DrawString(labels[i].ToString(), font, black, xs[i] + maxCircleRadius, 0, format);
                //    //Console.WriteLine("Label: " + labels[i]);
                //}
            }
        }

        private void onSizeChanged(object sender, SizeChangedEventArgs e)
        {
            layoutVisuals(canvases.Count);
        }

        private void MouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            int i = canvases.IndexOf((Canvas)sender);
            if (i >= 0)
                if (curValues != null && i < curValues.Length)
                    ValueChange.Invoke(canvases[i], new ValueChangeArgs()
                    {
                        Index = i,
                        Value = (curValues[i] > 0.5 ? 0.0 : 1.0)
                    });
        }
    }
}
