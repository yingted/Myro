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

namespace MyroControlPanel
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        Robot rbt;

        public Window1()
        {
            InitializeComponent();
            rbt = new Robot("C:\\Microsoft Robotics Dev Studio 2008\\config\\Scribbler.manifest\\Scribbler.manifest.xml");
            new Thread(new ThreadStart(updateLoop)).Start();
        }

        private void updateLoop()
        {
            while (true)
            {
                //Console.WriteLine("update");
                Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal, new ThreadStart(update));
                Thread.Sleep(100);
            }
        }

        private void update()
        {
            bumperMeters.setData(rbt.Sensors.get("bumpers"), rbt.Sensors.getNames("bumpers"), 0.0, 1.0);
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

    }
}
