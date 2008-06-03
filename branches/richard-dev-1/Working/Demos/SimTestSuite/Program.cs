using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using Microsoft.Dss.Hosting;

using IPREFoundationClasses;

namespace SimTestSuite
{
    class SimTestSuite
    {
        ScribblerBrain scribbler;
        TestPanel panel;

        public void start()
        {
            Console.WriteLine("Starting DSS environment...");
            DssEnvironment.Initialize(50000, 50001);
            AdapterBank adapters = new AdapterBank("Config\\Scribbler.config.xml", true);

            //Console.WriteLine("Opening Scribbler...");
            //scribbler = new ScribblerBrain("Config\\IPRE.Scribbler.standard.config.xml");
            //Console.WriteLine("Scribbler is ready!");

            //new Thread(new ThreadStart(bumperLoop)).Start();
            //new Thread(new ThreadStart(driveLoop)).Start();

            startGUI();
        }

        void bumperLoop()
        {
            while (true)
            {
                Console.WriteLine("[ " + scribbler.getContact(0) + ", " + scribbler.getContact(1) + " ]");
                Thread.Sleep(500);
            }
        }

        void driveLoop()
        {
            scribbler.ForwardFor(1.0f, 10.0f);
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
