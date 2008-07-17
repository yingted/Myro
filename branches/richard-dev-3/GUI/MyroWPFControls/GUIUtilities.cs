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
using System.Windows.Shapes;

namespace Myro.GUI.WPFControls
{
    public static class GUIUtilities
    {
        public static void ReportUnexpectedException(Exception e)
        {
            Window w = new Window()
            {
                Title = "Unexpected exception occured",
                Width = 500,
                Height = 400,
                Background = new SolidColorBrush(Color.FromRgb(0xF0, 0xF0, 0xF0))
            };
            Grid g = new Grid();
            g.RowDefinitions.Add(new RowDefinition());
            g.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });

            FlowDocumentScrollViewer f = new FlowDocumentScrollViewer()
            {
                HorizontalScrollBarVisibility = ScrollBarVisibility.Auto,
                VerticalScrollBarVisibility = ScrollBarVisibility.Auto,
                BorderBrush = Brushes.Black,
                BorderThickness = new Thickness(0, 0, 0, 1),
            };
            f.SetValue(Grid.RowProperty, 0);
            f.Document = buildExceptionMessage(new FlowDocument()
            {
                Background = Brushes.White,
                TextAlignment = TextAlignment.Left,
                FontFamily = new FontFamily("Sans Serif")
            }, e, 4);
            g.Children.Add(f);

            Button b = new Button()
            {
                Padding = new Thickness(10, 2, 10, 2),
                Margin = new Thickness(10),
                Content = "Ok",
                HorizontalAlignment = HorizontalAlignment.Center,
                IsDefault = true,
            };
            b.SetValue(Grid.RowProperty, 1);
            b.Click += delegate { w.Close(); };
            g.Children.Add(b);

            w.Content = g;
            w.ShowDialog();
        }

        private static FlowDocument buildExceptionMessage(FlowDocument curMessage, Exception e, int remainingLevels)
        {
            if (e != null)
            {
                Paragraph p = new Paragraph();
                p.Inlines.Add(new Run("Exception of type "));
                p.Inlines.Add(new Bold(new Run(e.GetType().FullName)));
                if (e.Message != null && e.Message.Length > 0)
                {
                    p.Inlines.Add(new Run(": "));
                    p.Inlines.Add(new Run("\"" + e.Message + "\".") { Foreground = Brushes.Blue });
                }
                else
                    p.Inlines.Add(new Run(".  No message available."));
                p.Inlines.Add(new LineBreak());
                if (e.Source != null)
                {
                    p.Inlines.Add("Source: ");
                    p.Inlines.Add(e.Source.ToString());
                    p.Inlines.Add(new LineBreak());
                }
                if (e.StackTrace != null && e.StackTrace.Length > 0)
                    p.Inlines.Add(new Run("Stacktrace:\n" + e.StackTrace));
                else
                    p.Inlines.Add(new Run("No stacktrace available."));
                p.Inlines.Add(new LineBreak());
                curMessage.Blocks.Add(p);
                if (remainingLevels >= 0 && e.InnerException != null)
                {
                    curMessage.Blocks.Add(new Paragraph(new Run("Inner exception:")));
                    return buildExceptionMessage(curMessage, e.InnerException, remainingLevels - 1);
                }
                else
                    return curMessage;
            }
            else
                return curMessage;
        }
    }
}
