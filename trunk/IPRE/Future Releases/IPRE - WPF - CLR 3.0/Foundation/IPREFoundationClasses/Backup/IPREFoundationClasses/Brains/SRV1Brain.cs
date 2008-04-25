using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Xml.XPath;
using System.Windows.Forms;
using IPREFoundationClasses.RobotGUI;

namespace IPREFoundationClasses
{
    public class SRV1Brain : RobotBrain
    {
        public SRV1Brain(string configFile) : base(configFile) 
        {
            SrvInit();
        }

        void SrvInit()
        {
            Bind();
            Thread.Sleep(10000);
            ParseConfig();
        }
    }
}
