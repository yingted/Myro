using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.Threading;
using IPREFoundationClasses;
using IPREFoundationClasses.RobotGUI;

namespace MyProgram
{
    partial class Program
    {
        #region Main
        static RobotBrain rbt;
        static Form1 robotgui;
        static Logger logger;

        [STAThread]
        static void Main(string[] args)
        {
            Console.WriteLine("hello scribby");
            rbt = new NxtBrain(@"C:\Microsoft Robotics Studio (1.5)\samples\IPRE\Foundation\IPREFoundationClasses\IPREFoundationClasses\NxtConfig\IPRE.LEGO.NXT.TriBot.config.xml");
            //rbt = new ScribblerBrain(@"C:\Microsoft Robotics Studio (1.5)\samples\IPRE\Foundation\IPREFoundationClasses\IPREFoundationClasses\IPRE.Scribbler.standard.config.xml");
            //rbt = new SRV1Brain(@"C:\Microsoft Robotics Studio (1.5)\samples\IPRE\Surveyor\SRV-1\Srv1Services\Config\IPRE.Surveyor.SRV1.Vehicle.config.xml");
            //rbt = new CreateBrain(@"C:\Microsoft Robotics Studio (1.5)\samples\IPRE\Foundation\IPREFoundationClasses\IPREFoundationClasses\CreateConfig\IPRE.iRobot.DriveBumper.config.xml");

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            robotgui = new Form1(rbt);
            logger = new Logger(robotgui);
            logger.Add(new LogEntry("SRV ready !!!"));

            Thread th = new Thread(new ThreadStart(work));
            th.Start();

            Application.Run(robotgui);
        }
        #endregion


        #region Impl
        public static void SetMotorsFor(float leftPower, float rightPower, float seconds)
        {
            rbt.SetMotorsFor(leftPower, rightPower, seconds);
        }

        public static float getSonar(int position)
        {
            return rbt.getSonar(position);
        }

        public static void TurnRightFor(float power,float seconds)
        {
            rbt.TurnRightFor(power,seconds);
        }

        public static void SetMotors(float leftPower, float rightPower)
        {
            rbt.SetMotors(leftPower, rightPower);
        }

        public static void Stop()
        {
            rbt.Stop();
        }

        public static void PlaySong(string filename)
        {
            rbt.PlaySong((MyroSong)rbt.ReadSong(filename));
        }

        #endregion
    }
}
