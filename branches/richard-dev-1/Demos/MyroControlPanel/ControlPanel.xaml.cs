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
using System.Threading;

using Myro;
using Myro.WPFControls;

namespace MyroControlPanel
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        Robot rbt;
        bool shouldExit = false;

        List<ServicePanelInfo> panelList = new List<ServicePanelInfo>()
        {
            new ServicePanelInfo() { Description="Bumpers / IR", Name="bumpers", Min=0.0, Max=1.0, Color=Colors.Blue },
            new ServicePanelInfo() { Description="Sonar", Name="sonar", Min=20.0, Max=0.0, Color=Colors.Tan },
            new ServicePanelInfo() { Description="Stall", Name="stall", Min=0.0, Max=1.0, Color=Colors.Red },
            new ServicePanelInfo() { Description="Light sensors", Name="light", Min=2000.0, Max=0.0, Color=Colors.DeepSkyBlue },
            new ServicePanelInfo() { Description="Line sensors", Name="line", Min=0.0, Max=1.0, Color=Colors.DarkGray },
            new ServicePanelInfo() { Description="LEDs", Name="led", Min=0.0, Max=1.0, Color=Colors.OrangeRed }
        };
        List<ServicePanelInfo> livePanelList = new List<ServicePanelInfo>();

        public Window1()
        {
            InitializeComponent();
            rbt = new Robot("C:\\Microsoft Robotics Dev Studio 2008\\config\\Generic.manifest\\Generic.manifest.xml");
            new Thread(new ThreadStart(updateLoop)).Start();
        }

        private void updateLoop()
        {
            int delayMs = 100;
            int updateCounter = 0;
            Thread checker = null;
            while (!shouldExit)
            {
                if (updateCounter++ > (2000 / delayMs))
                {
                    updateCounter = 0;
                    if (checker == null || checker.IsAlive == false) {
                        checker = new Thread(new ThreadStart(checkNewPanels));
                        checker.Start();
                    }
                }
                //Console.WriteLine("update");
                Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal, new ThreadStart(update));
                Thread.Sleep(delayMs);
            }
        }

        private void update()
        {
            foreach (var pi in livePanelList)
            {
                try { pi.Meters.setData(rbt.Sensors.get(pi.Name), rbt.Sensors.getNames(pi.Name), pi.Min, pi.Max); }
                catch (Exception e) { }
            }
            //bumperMeters.setData(rbt.Sensors.get("bumpers"), rbt.Sensors.getNames("bumpers"), 0.0, 1.0);
            //drawCircleMeters(contactSensorImg, Color.MediumVioletRed, contacts, rbt.Sensors.getNames("bumpers"), 0.0, 1.0);
            //double[] stall = rbt.Sensors.get("stall");
            //drawCircleMeters(stallSensorImg, Color.Red, stall, rbt.Sensors.getNames("stall"), 0.0, 1.0);
            //double[] light = rbt.Sensors.get("light");
            //drawCircleMeters(lightSensorImg, Color.DeepSkyBlue, light, rbt.Sensors.getNames("light"), 2000.0, 0.0);
            //double[] sonar = rbt.Sensors.get("sonar");
            //drawCircleMeters(SonarImg, Color.Tan, sonar, rbt.Sensors.getNames("sonar"), 40.0, 0.0);
            //double[] line = rbt.Sensors.get("line");
            //drawCircleMeters(lineSensorImg, Color.DarkGray, line, rbt.Sensors.getNames("line"), 0.0, 1.0);
        }

        private void checkNewPanels()
        {
            List<ServicePanelInfo> toRemove = new List<ServicePanelInfo>();
            foreach (var pi in panelList)
            {
                try
                {
                    rbt.Sensors.getNames(pi.Name);

                    toRemove.Add(pi);
                    //servicePanel.Children.Add(grp);
                }
                catch (Exception e) { }
            }
            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                new ThreadStart(delegate()
                    {
                        foreach (var pi in toRemove)
                        {
                            pi.Meters = new CircleMeters()
                            {
                                Width = 200,
                                Height = 30,
                            };
                            var myPi = pi;
                            pi.Meters.ValueChange += (CircleMeters.ValueChangeHandler)
                                delegate(object sender, Myro.WPFControls.CircleMeters.ValueChangeArgs e)
                                {
                                    rbt.Sensors.set(myPi.Name, e.Index, e.Value);
                                };
                            pi.Meters.SetColor(pi.Color);
                            pi.Group = new GroupBox()
                            {
                                Header = pi.Description,
                                Margin = new Thickness(5.0, 5.0, 5.0, 10.0),
                                HorizontalAlignment = HorizontalAlignment.Left,
                                VerticalAlignment = VerticalAlignment.Top,
                                Content = pi.Meters
                            };
                            livePanelList.Add(pi);
                            panelList.Remove(pi);
                            servicePanel.Children.Add(pi.Group);
                        }
                    }));

        }

        private void BumperValueChange(object sender, Myro.WPFControls.CircleMeters.ValueChangeArgs e)
        {
            rbt.Sensors.set("bumpers", e.Index, e.Value);
        }

        private void OnClosed(object sender, EventArgs e)
        {
            shouldExit = true;
            rbt.Shutdown();
        }

        private void Frequency1Changed(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            if (freq1Label != null)
                if (freq1Slider.Value == freq1Slider.Minimum)
                    freq1Label.Content = "0";
                else
                    freq1Label.Content = Math.Round(freq1Slider.Value).ToString() + " Hz";
        }

        private void Frequency2Changed(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            if (freq2Label != null)
                if (freq2Slider.Value == freq2Slider.Minimum)
                    freq2Label.Content = "0";
                else
                    freq2Label.Content = Math.Round(freq2Slider.Value).ToString() + " Hz";
        }

        private void DurationChanged(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            if (freq2Label != null)
                durLabel.Content = Math.Round(durSlider.Value).ToString() + " ms";
        }

        private void PlayTone(object sender, MouseButtonEventArgs e)
        {
            try
            {
                rbt.Sound.beep(durSlider.Value,
                    (freq1Slider.Value == freq1Slider.Minimum ? 0.0 : freq1Slider.Value),
                    (freq2Slider.Value == freq2Slider.Minimum ? 0.0 : freq2Slider.Value));
            }
            catch (Exception) { }
        }

        private void SetLoud(object sender, RoutedEventArgs e)
        {
            try
            {
                if (loudCheck != null)
                    rbt.Sound.SetLoud(loudCheck.IsChecked.HasValue ? loudCheck.IsChecked.Value : true);
            }
            catch (Exception) { }
        }

    }

    class ServicePanelInfo
    {
        public string Description;
        public string Name;
        public double Min;
        public double Max;
        public Color Color;
        public CircleMeters Meters;
        public GroupBox Group;
    }
}
