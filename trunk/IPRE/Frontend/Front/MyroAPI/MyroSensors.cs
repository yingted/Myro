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

        public double[] Get(string name)
        {
                return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.GetAllElements().ToArray();
        }

        public string[] GetNames(string name)
        {
            return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.GetState().Keys.ToArray();
        }

        public void GetPairs(string name, out double[] values, out string[] names)
        {
            var state = bank.GetAdapterSpec<VectorAdapter>(name).Adapter.GetState();
            values = state.Values.ToArray();
            names = state.Keys.ToArray();
        }

        public double Get(string name, int index)
        {
            return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Get(index);
        }

        public double Get(string name, string tag)
        {
            return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Get(tag);
        }

        public void Set(string name, int index, double value)
        {
            bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Set(index, value);
        }

        public void Set(string name, string tag, double value)
        {
            bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Set(tag, value);
        }


    }
}
