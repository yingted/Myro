using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;
using Microsoft.Dss.Hosting;

namespace Myro
{
    public class Robot
    {
        AdapterBank bank;
        public Sensors Sensors { get; private set; }

        public Robot(string configFile)
        {
            Console.Write("Starting DSS environment...");
            DssEnvironment.Initialize(50000, 50001);
            Console.WriteLine("Done");
            bank = new AdapterBank(configFile, true);
            bank.WaitForAdapters(TimeSpan.FromMilliseconds(10000));
            Console.WriteLine("All adapters are attached!");
            Sensors = new Sensors(bank);
        }
    }
}
