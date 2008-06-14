using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;
using Myro.API;
using Microsoft.Dss.Hosting;
using System.IO;

namespace Myro
{
    public class Robot
    {
        AdapterBank bank;
        public MyroSensors Sensors { get; private set; }
        public MyroMovement Movement { get; private set; }
        public IMyroSound Sound { get; private set; }

        public Robot(string manifestFile)
        {
            Console.Write("Starting DSS environment...");
            DssEnvironment.Initialize(50000, 50001, "file://" + Path.GetFullPath(manifestFile));
            Console.WriteLine("Done");
            bank = new AdapterBank(new List<IAdapterFactory>() {
                new Myro.Adapters.DriveAdapterFactory(),
                new Myro.Adapters.VectorAdapterFactory()
            });

            Sensors = new MyroSensors(bank);
            Movement = new MyroMovement(bank);
            Sound = new MyroSound(bank);
        }
    }
}
