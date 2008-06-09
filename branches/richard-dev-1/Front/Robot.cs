using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;

namespace Myro
{
    public class Robot
    {
        AdapterBank bank;
        public Myro.API.MyroSensors Sensors { get; private set; }
        public Myro.API.MyroMovement Movement { get; private set; }
        public Myro.API.IMyroSound Sound { get; private set; }

        public Robot(string configFile)
        {
            //Console.Write("Starting DSS environment...");
            //DssEnvironment.Initialize(50000, 50001);
            //Console.WriteLine("Done");
            bank = new AdapterBank(configFile, true);
            bank.WaitForAdapters(TimeSpan.FromMilliseconds(100000));
            Console.WriteLine("All adapters are attached!");
            Sensors = new Myro.API.MyroSensors(bank);
            Movement = new Myro.API.MyroMovement(bank.GetAdapterSpec("drive"));
            try
            {
                Sound = new Myro.API.MyroSound(bank.GetAdapterSpec("tonegen"));
            }
            catch (UnknownAdapterNameException)
            {
                Sound = null;
            }
        }
    }
}
