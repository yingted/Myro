using System;
using System.Collections.Generic;
using System.Text;

namespace Myro.API
{
    
    public interface IMyroSensor
    {
        double[] Get(string sensorID);
        double Get(string sensorID, int position);
        double Get(string sensorID, string position);
        //float getLight(int position);
        //float getIR(int position);
        //float getLine(int position);
        //float getStall();
    }
}
