using System;
using System.Collections.Generic;
using System.Text;

namespace Myro.API
{
    
    public interface IMyroSensor
    {
        double[] get(string sensorID);
        double get(string sensorID, int position);
        double get(string sensorID, string position);
        //float getLight(int position);
        //float getIR(int position);
        //float getLine(int position);
        //float getStall();
    }
}
