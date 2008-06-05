using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;

namespace Myro
{
    /// <summary>
    /// From these methods, the following exceptions may be thrown:
    /// UnknownAdapterNameException if the adapter name is invalid.
    /// AdapterArgumentException if the tag or index is invalid.
    /// AdapterOperationException if something goes wrong internally.
    /// </summary>
    public class Sensors
    {
        AdapterBank bank;
        public Sensors(AdapterBank bank)
        {
            this.bank = bank;
        }

        public double[] get(string name)
        {
            return bank.GetAdapterSpec(name).GetVectorAdapter().Get();
        }

        public double get(string name, int index)
        {
            return bank.GetAdapterSpec(name).GetVectorAdapter().Get(index);
        }

        public double get(string name, string tag)
        {
            return bank.GetAdapterSpec(name).GetVectorAdapter().Get(tag);
        }

    }
}
