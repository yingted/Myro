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

namespace Myro.GUI.WPFControls
{
    /// <summary>
    /// Interaction logic for CircleMeters.xaml
    /// </summary>
    public partial class CircleMeters : UserControl
    {
        /// <summary>
        /// Arguments for the ValueChange event, which this control raises
        /// when the user clicks on one of the meters to change its value.
        /// </summary>
        public class ValueChangeArgs
        {
            /// <summary>
            /// The Vector index of the modified value
            /// </summary>
            public int Index { get; internal set; }
            /// <summary>
            /// The new value
            /// </summary>
            public double Value { get; internal set; }
        }
        /// <summary>
        /// The delegate type for the ValueChange event, which this control raises
        /// when the user clicks on one of the meters to change its value.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public delegate void ValueChangeHandler(object sender, ValueChangeArgs e);
        /// <summary>
        /// The ValueChange event, which this control raises
        /// when the user clicks on one of the meters to change its value.
        /// </summary>
        public event ValueChangeHandler ValueChange;

        #region private variables
        List<Canvas> canvases = new List<Canvas>();
        List<TextBlock> labels = new List<TextBlock>();
        List<Label> valueLabels = new List<Label>();
        Color curColor = Brushes.DarkGray.Color;
        double[] curValues = null;
        static double widthOne = 30;
        #endregion

        /// <summary>
        /// Constructor, nothing unusual.
        /// </summary>
        public CircleMeters()
        {
            InitializeComponent();
            //AddVisualChild(mainVisual);
            //AddLogicalChild(mainVisual);
        }

        /// <summary>
        /// Change the color of the circle meters.  This must be called or
        /// the meters will have the default gray color.
        /// </summary>
        /// <param name="color"></param>
        public void SetColor(Color color)
        {
            curColor = color;
            layoutVisuals(canvases.Count);
        }

        /// <summary>
        /// Helper method that populates the private variables containing
        /// the UI visuals.
        /// </summary>
        /// <param name="count"></param>
        private void layoutVisuals(int count)
        {
            //double[] centers = new double[count];
            //double widthOne = 30; // this.ActualHeight;
            mainGrid.Children.Clear();
            mainGrid.ColumnDefinitions.Clear();
            for (int i = 0; i < count; i++)
                mainGrid.ColumnDefinitions.Add(new ColumnDefinition());
            canvases.Clear();
            labels.Clear();
            valueLabels.Clear();
            if (count > 0)
            {
                double widthEach = this.ActualWidth / (double)count;
                double offset = (widthEach - widthOne) / 2;
                for (int i = 0; i < count; i++)
                {
                    double left = widthEach * i + offset;
                    //mainVisual.Children.Add(new DrawingVisual());
                    Canvas c = new Canvas()
                    {
                        HorizontalAlignment = HorizontalAlignment.Center,
                        VerticalAlignment = VerticalAlignment.Center,
                        Width = widthOne,
                        Height = widthOne,
                    };
                    //c.SetValue(Canvas.LeftProperty, left);
                    //c.SetValue(Canvas.TopProperty, 0.0);
                    c.SetValue(Grid.RowProperty, 0);
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
                    int myI = i;
                    c.MouseLeftButtonDown += delegate(object sender, MouseButtonEventArgs e) { valueChangeHelper(myI, e); };
                    canvases.Add(c);

                    TextBlock l = new TextBlock()
                    {
                        VerticalAlignment = VerticalAlignment.Top,
                        HorizontalAlignment = HorizontalAlignment.Center,
                        TextAlignment = TextAlignment.Center,
                        TextWrapping = TextWrapping.WrapWithOverflow,
                        Margin = new Thickness(0),
                        Padding = new Thickness(0),
                        Foreground = Brushes.CadetBlue,
                    };
                    //l.SetValue(Canvas.TopProperty, 0.0);
                    //l.SetValue(Canvas.LeftProperty, 0.0);
                    l.SetValue(Grid.RowProperty, 1);
                    l.MouseLeftButtonDown += delegate(object sender, MouseButtonEventArgs e) { valueChangeHelper(myI, e); };
                    labels.Add(l);

                    Label lv = new Label()
                    {
                        VerticalAlignment = VerticalAlignment.Center,
                        HorizontalAlignment = HorizontalAlignment.Center,
                        Padding = new Thickness(0),
                        Foreground = Brushes.Black,
                        FontSize = 9,
                    };
                    lv.SetValue(Grid.RowProperty, 0);
                    lv.MouseLeftButtonDown += delegate(object sender, MouseButtonEventArgs e) { valueChangeHelper(myI, e); };
                    valueLabels.Add(lv);

                    Grid g = new Grid()
                    {
                        VerticalAlignment = VerticalAlignment.Top,
                        HorizontalAlignment = HorizontalAlignment.Stretch,
                    };
                    g.SetValue(Grid.ColumnProperty, i);
                    g.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });
                    g.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });
                    g.Children.Add(c);
                    g.Children.Add(lv);
                    g.Children.Add(l);
                    mainGrid.Children.Add(g);
                }
            }
        }

        /// <summary>
        /// Update the meters.  Call this method every time new data is available
        /// </summary>
        /// <param name="values">The numeric meter values</param>
        /// <param name="labels">The string labels for the meters</param>
        /// <param name="min">The minimum meter value (for circle scaling)</param>
        /// <param name="max">The maximum meter value (for circle scaling)</param>
        public void SetData(double[] values, string[] labels, double min, double max)
        {
            this.curValues = values;
            double maxCircleRadius = widthOne / 2;
            if (values.Length != canvases.Count)
                layoutVisuals(values.Length);

            List<double> radii = new List<double>(
                from v in values
                let normalized = (v - min) / (max - min)
                let normalizedBoundedM = (normalized > 0 ? normalized : 0)
                let normalizedBounded = (normalizedBoundedM < 1 ? normalizedBoundedM : 1)
                select normalizedBounded * maxCircleRadius);

            for (int i = 0; i < values.Length; i++)
            {
                double offset = maxCircleRadius - radii[i];
                ((Path)canvases[i].Children[0]).Data = new EllipseGeometry(
                    new Point(widthOne / 2.0, widthOne / 2.0), maxCircleRadius, maxCircleRadius);
                ((Path)canvases[i].Children[1]).Data = new EllipseGeometry(
                    new Point(widthOne / 2.0, widthOne / 2.0), radii[i], radii[i]);
                if (i < labels.Length)
                    this.labels[i].Text = labels[i];
                if (values[i] == 0.0 || values[i] == 1.0)
                    this.valueLabels[i].Content = "";
                else
                    this.valueLabels[i].Content = Math.Round(values[i], 2).ToString();
            }
        }

        /// <summary>
        /// Internal helper method called by the mouse button handlers of the circle
        /// meter visuals.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="e"></param>
        private void valueChangeHelper(int index, MouseButtonEventArgs e)
        {
            if (curValues != null && index >= 0 && index < curValues.Length)
                ValueChange.Invoke(canvases[index], new ValueChangeArgs()
                {
                    Index = index,
                    Value = (curValues[index] > 0.5 ? 0.0 : 1.0)
                });
        }
    }
}
