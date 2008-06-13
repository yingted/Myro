using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;
using Myro.API;

namespace Myro
{
    public class Robot
    {
        AdapterBank bank;
        public MyroSensors Sensors { get; private set; }
        public MyroMovement Movement { get; private set; }
        public IMyroSound Sound { get; private set; }

        public Robot(string configFile)
        {
            //Console.Write("Starting DSS environment...");
            //DssEnvironment.Initialize(50000, 50001);
            //Console.WriteLine("Done");
            bank = new AdapterBank(configFile, true);
            //bank.WaitForAdapters(TimeSpan.FromMilliseconds(100000));
            //Console.WriteLine("All adapters are attached!");

            Sensors = new MyroSensors(bank);
            Movement = new MyroMovement(bank);
            Sound = new MyroSound(bank);
        }
    }
}
