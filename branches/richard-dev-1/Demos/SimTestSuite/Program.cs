using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using Microsoft.Dss.Hosting;

using Myro;

namespace SimTestSuite
{
    class SimTestSuite
    {
        Robot scribbler;
        TestPanel panel;

        public void start()
        {
            scribbler = new Robot("C:\\Microsoft Robotics Dev Studio 2008\\config\\Create.manifest\\Create.manifest.xml");

            startGUI();
        }

        void startGUI()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            panel = new TestPanel(scribbler);
            Application.Run(panel);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            SimTestSuite test = new SimTestSuite();
            test.start();
        }
    }
}
