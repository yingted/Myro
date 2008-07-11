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
using Myro.GUI.WPFControls;

namespace Myro.GUI.ControlPanel
{
    public partial class ControlPanel : UserControl
    {
        //Robot rbt = null;
        bool shouldExit = false;
        Thread updateThread = null;

        List<ServicePanelInfo> origPanelList = new List<ServicePanelInfo>()
        {
            new ServicePanelInfo() { Description="Bumpers / IR", Name="bumpers", Min=0.0, Max=1.0, Color=Colors.Blue },
            new ServicePanelInfo() { Description="IR Sensors", Name="ir", Min=0.0, Max=0.2, Color=Colors.DarkRed },
            new ServicePanelInfo() { Description="Sonar", Name="sonar", Min=20.0, Max=0.0, Color=Colors.Tan },
            new ServicePanelInfo() { Description="Stall", Name="stall", Min=0.0, Max=1.0, Color=Colors.Red },
            new ServicePanelInfo() { Description="Light sensors", Name="light", Min=2000.0, Max=0.0, Color=Colors.DeepSkyBlue },
            new ServicePanelInfo() { Description="Line sensors", Name="line", Min=0.0, Max=1.0, Color=Colors.DarkGray },
            new ServicePanelInfo() { Description="LEDs", Name="led", Min=0.0, Max=1.0, Color=Colors.Chartreuse }
        };
        List<ServicePanelInfo> panelList;
        List<ServicePanelInfo> livePanelList = new List<ServicePanelInfo>();

        int delayMs = 500;
        bool shouldUpdate = true;

        public ControlPanel()
        {
            InitializeComponent();
            reset();
        }

        private void reset()
        {
            if (livePanelList.Count > 0)
                Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal,
                    new ThreadStart(delegate()
                        {
                            try
                            {
                                foreach (var pi in livePanelList)
                                    servicePanel.Children.Remove(pi.Group);
                            }
                            catch (Exception e)
                            {
                                Console.WriteLine(e.ToString());
                            }
                        }));
            panelList = origPanelList;
            livePanelList.Clear();
        }

        public void SetRobot()
        {
            shouldExit = true;
            if (updateThread != null && updateThread.IsAlive)
                updateThread.Join();
            reset();

            //if (robot != null)
            //{
            shouldExit = false;
            //this.rbt = robot;
            updateThread = new Thread(new ThreadStart(updateLoop));
            updateThread.Start();
            //}
        }

        private void updateLoop()
        {
            int updateCounter = 0;
            Thread checker = null;
            //bool driveSet = false;
            while (!shouldExit)
            {
                //if (!driveSet && drive != null)
                //{
                //    drive.SetDrive(rbt.Movement);
                //    driveSet = true;
                //}

                if (shouldUpdate)
                {
                    if (updateCounter++ > (2000 / delayMs))
                    {
                        updateCounter = 0;
                        if (checker == null || checker.IsAlive == false)
                        {
                            checker = new Thread(new ThreadStart(checkNewPanels));
                            checker.Start();
                        }
                    }

                    // Copy the reference because checkNewPanels might swap this list for a new one
                    var myLivePanelList = livePanelList;
                    foreach (var pi in myLivePanelList)
                    {
                        var myPi = pi;
                        double[] values;
                        string[] names;
                        try
                        {
                            Robot.GetPairs(pi.Name, out names, out values);
                            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                                new ThreadStart(delegate() { myPi.Meters.setData(values, names, myPi.Min, myPi.Max); }));
                        }
                        catch (Exception) { }
                    }
                }
                Thread.Sleep(delayMs);
            }
        }

        private void checkNewPanels()
        {
            List<ServicePanelInfo> toRemove = new List<ServicePanelInfo>();
            List<Thread> threads = new List<Thread>();
            foreach (var pi in panelList)
            {
                var myPi = pi;
                Thread t = new Thread(new ThreadStart(
                    delegate()
                    {
                        try
                        {
                            //Console.WriteLine("Adding " + myPi.Name);
                            Robot.GetNames(myPi.Name);
                            lock (toRemove) { toRemove.Add(myPi); }
                        }
                        catch (Exception) { }
                    }));
                t.Start();
                threads.Add(t);
            }
            foreach (var t in threads)
                t.Join();
            ManualResetEvent signal = new ManualResetEvent(false);
            Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                new ThreadStart(delegate()
                    {
                        try
                        {
                            var newLivePanelList = new List<ServicePanelInfo>(livePanelList);
                            foreach (var pi in toRemove)
                            {
                                pi.Meters = new CircleMeters()
                                {
                                    //Width = 200,
                                    //Height = 30,
                                };
                                var myPi = pi;
                                pi.Meters.ValueChange += (CircleMeters.ValueChangeHandler)
                                    delegate(object sender, Myro.GUI.WPFControls.CircleMeters.ValueChangeArgs e)
                                    {
                                        Robot.Set(myPi.Name, e.Index, e.Value);
                                    };
                                pi.Meters.SetColor(pi.Color);
                                pi.Group = new GroupBox()
                                {
                                    Header = pi.Description,
                                    Margin = new Thickness(5.0, 5.0, 5.0, 10.0),
                                    HorizontalAlignment = HorizontalAlignment.Stretch,
                                    VerticalAlignment = VerticalAlignment.Top,
                                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                                    Content = pi.Meters
                                };
                                newLivePanelList.Add(pi);
                                panelList.Remove(pi);
                                servicePanel.Children.Add(pi.Group);
                            }
                            // Swap in reference to new list, changed atomically because updateLoop
                            // may be using the list to update sensor readings
                            livePanelList = newLivePanelList;
                        }
                        catch (Exception e)
                        {
                            Console.WriteLine(e.ToString());
                            throw;
                        }
                        finally { signal.Set(); }
                    }));
            signal.WaitOne();
        }

        //private void BumperValueChange(object sender, Myro.GUI.WPFControls.CircleMeters.ValueChangeArgs e)
        //{
        //    Robot.Set("bumpers", e.Index, e.Value);
        //}

        public void Dispose()
        {
            shouldExit = true;
            if (updateThread != null)
                updateThread.Join();
            if (drive != null)
                drive.Dispose();
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

        private void PlayTone(object sender, MouseEventArgs e)
        {
            try
            {
                Robot.Beep(durSlider.Value,
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
                    Robot.SetLoud(loudCheck.IsChecked.HasValue ? loudCheck.IsChecked.Value : true);
            }
            catch (Exception) { }
        }

        private void OnPollValueChanged(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            if (PollSliderGlow != null && pollingSlider != null)
            {
                delayMs = (int)(reversePollValue(pollingSlider.Value) * 1000.0);
                //PollSliderGlow.Duration = TimeSpan.FromMilliseconds(pollingSlider.Value);
                if (pollingSlider.Maximum - reversePollValue(pollingSlider.Value) <= .02 )
                {
                    shouldUpdate = false;
                    PollSliderGlow.Seek(pollingSlider, TimeSpan.FromMilliseconds(0), System.Windows.Media.Animation.TimeSeekOrigin.BeginTime);
                    PollSliderGlow.Pause(pollingSlider);
                }
                else
                {
                    shouldUpdate = true;
                    PollSliderGlow.Resume(pollingSlider);
                    PollSliderGlow.SetSpeedRatio(pollingSlider, 1.0 / reversePollValue(pollingSlider.Value));
                }
            }
        }

        private void OnPollLostMouse(object sender, MouseEventArgs e)
        {
        }

        private double reversePollValue(double value)
        {
            return pollingSlider.Maximum - pollingSlider.Value + pollingSlider.Minimum;
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
