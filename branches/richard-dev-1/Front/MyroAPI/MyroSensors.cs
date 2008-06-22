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
            { return ((Myro.Adapters.VectorAdapter)bank.GetAdapterSpec(name).Adapter).GetAllElements().ToArray(); }
            catch (Exception)
            { return new double[] { 0.0 }; }
        }

        public string[] getNames(string name)
        {
            try
            { return ((Myro.Adapters.VectorAdapter)bank.GetAdapterSpec(name).Adapter).GetState().Keys.ToArray(); }
            catch (Exception)
            { return new string[0]; }
        }

        public double get(string name, int index)
        {
            try
            { return ((Myro.Adapters.VectorAdapter)bank.GetAdapterSpec(name).Adapter).Get(index); }
            catch (Exception)
            { return 0.0; }
        }

        public double get(string name, string tag)
        {
            try
            { return ((Myro.Adapters.VectorAdapter)bank.GetAdapterSpec(name).Adapter).Get(tag); }
            catch (Exception)
            { return 0.0; }
        }

        public void set(string name, int index, double value)
        {
            try
            { ((VectorAdapter)bank.GetAdapterSpec(name).Adapter).Set(index, value); }
            catch (Exception)
            { }
        }

        public void set(string name, string tag, double value)
        {
            try
            { ((VectorAdapter)bank.GetAdapterSpec(name).Adapter).Set(tag, value); }
            catch (Exception)
            { }
        }


    }
}
