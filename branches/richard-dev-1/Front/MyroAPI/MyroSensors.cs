using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;

namespace Myro.API
{
    /// <summary>
    /// From these methods, the following exceptions may be thrown:
    /// UnknownAdapterNameException if the adapter name is invalid.
    /// AdapterArgumentException if the tag or index is invalid.
    /// AdapterOperationException if something goes wrong internally.
    /// </summary>
    public class MyroSensors : IMyroSensor
    {
        AdapterBank bank;
        public MyroSensors(AdapterBank bank)
        {
            this.bank = bank;
        }

        public double[] get(string name)
        {
            try
            {
                return bank.GetAdapterSpec(name).GetVectorAdapter().GetAllElements().ToArray();
            }
            catch (UnknownAdapterNameException)
            {
                return new double[] { 0.0 };
            }
        }

        public double get(string name, int index)
        {
            try
            {
                return bank.GetAdapterSpec(name).GetVectorAdapter().Get(index);
            }
            catch (UnknownAdapterNameException)
            {
                return 0.0;
            }
        }

        public double get(string name, string tag)
        {
            try
            {
                return bank.GetAdapterSpec(name).GetVectorAdapter().Get(tag);
            }
            catch (UnknownAdapterNameException)
            {
                return 0.0;
            }
        }

    }
}
