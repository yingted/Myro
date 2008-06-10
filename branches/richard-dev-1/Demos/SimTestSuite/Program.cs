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
            scribbler = new Robot("Config\\ScribblerSim.config.xml");

            //Console.WriteLine("Opening Scribbler...");
            //scribbler = new ScribblerBrain("Config\\IPRE.Scribbler.standard.config.xml");
            //Console.WriteLine("Scribbler is ready!");

            //new Thread(new ThreadStart(bumperLoop)).Start();
            //new Thread(new ThreadStart(driveLoop)).Start();

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
