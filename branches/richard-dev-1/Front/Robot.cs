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
            bank.WaitForAdapters(TimeSpan.FromMilliseconds(100000));
            Console.WriteLine("All adapters are attached!");

            Sensors = new MyroSensors(bank);

            // Doing this so this class still works when these services are not present
            try { Movement = new MyroMovement(bank.GetAdapterSpec("drive")); }
            catch (UnknownAdapterNameException) { Movement = new MyroMovement(new AdapterSpec(AdapterTypeEnum.Drive, "drive")); }

            try { Sound = new MyroSound(bank.GetAdapterSpec("tonegen")); }
            catch (UnknownAdapterNameException) { Sound = new MyroSound(new AdapterSpec(AdapterTypeEnum.Vector, "tonegen")); }
        }
    }
}
