using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;

namespace Myro.API
{
    class MyroSensors
    {
        VectorAdapter bumper;

        public MyroSensors(AdapterBank bank)
        {
            bumper = bank.GetAdapterSpec(

        public double[] get(string name)
        {

    }
}
