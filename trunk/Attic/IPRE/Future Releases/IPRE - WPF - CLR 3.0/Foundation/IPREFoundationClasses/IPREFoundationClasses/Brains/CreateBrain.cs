using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using IPREFoundationClasses.RobotGUI;

namespace IPREFoundationClasses
{
    public class CreateBrain : RobotBrain
    {
        public CreateBrain(string configFile) : base(configFile) 
        {
            CreateInit();
        }

        void CreateInit()
        {
            Bind();
            Thread.Sleep(10000);
            ParseConfig();
        }
    }
}
