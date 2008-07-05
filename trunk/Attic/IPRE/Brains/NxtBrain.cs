using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using IPREFoundationClasses.RobotGUI;

namespace IPREFoundationClasses
{
    public class NxtBrain : RobotBrain
    {
        public NxtBrain(string configFile) : base(configFile) 
        {
            NxtInit();
        }

        void NxtInit()
        {
            Bind();
            Thread.Sleep(10000);
            ParseConfig();
        }
    }
}
