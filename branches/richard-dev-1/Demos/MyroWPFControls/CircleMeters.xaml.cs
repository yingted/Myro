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

namespace Myro.WPFControls
{
    /// <summary>
    /// Interaction logic for CircleMeters.xaml
    /// </summary>
    public partial class CircleMeters : UserControl
    {
        List<Canvas> canvases = new List<Canvas>();
        Color curColor = Brushes.DarkGray.Color;

        public CircleMeters()
        {
            InitializeComponent();
            //AddVisualChild(mainVisual);
            //AddLogicalChild(mainVisual);
        }

        public void setColor(Color color)
        {
            curColor = color;
            layoutVisuals(canvases.Count);
        }

        private void layoutVisuals(int count)
        {
            double[] centers = new double[count];
            double widthOne = this.ActualHeight;
            mainCanvas.Children.Clear();
            canvases.Clear();
            if (count > 0)
            {
                double widthEach = this.ActualWidth / (double)count;
                double offset = (widthEach - widthOne) / 2;
                for (int i = 0; i < count; i++)
                {
                    centers[i] = widthEach * i + offset;
                    //mainVisual.Children.Add(new DrawingVisual());
                    Canvas c = new Canvas();
                    c.SetValue(Canvas.LeftProperty, centers[i] - (widthOne / 2.0));
                    c.SetValue(Canvas.TopProperty, 0.0);
                    c.Width = widthOne;
                    c.Height = widthOne;
                    Path p = new Path();
                    if (curColor != null)
                        p.Fill = new SolidColorBrush(curColor);
                    c.Children.Add(p);
                    canvases.Add(c);
                    mainCanvas.Children.Add(c);
                }
            }
        }

        public void setData(double[] values, string[] labels, double min, double max)
        {
            double maxCircleRadius = this.ActualHeight / 2;
            if (values.Length != canvases.Count)
                layoutVisuals(values.Length);
            //Console.WriteLine("L: " + vals[0] + "  R: " + vals[1]);
            List<double> radii = new List<double>(
                from v in values
                let normalized = (v - min) / (max - min)
                let normalizedBounded = (normalized > 0 ? normalized : 0)
                select normalizedBounded * maxCircleRadius);
            //StringFormat format = StringFormat.GenericDefault;
            //format.Alignment = StringAlignment.Center;
            //Font font = new Font("Sans Serif", 7);
            for (int i = 0; i < values.Length; i++)
            {
                //Console.WriteLine("Radius: " + radii.ElementAt(i));
                double offset = maxCircleRadius - radii[i];
                ((Path)canvases[i].Children[0]).Data = new EllipseGeometry(
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
    }
}
