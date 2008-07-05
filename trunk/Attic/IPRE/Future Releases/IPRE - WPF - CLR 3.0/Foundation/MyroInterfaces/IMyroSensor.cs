using System;
using System.Collections.Generic;
using System.Text;

namespace MyroInterfaces
{
    
    public interface IMyroSensor
    {
        float[] get(string sensorID);
        float get(string sensorID, int position);
        float getLight(int position);
        float getIR(int position);
        float getLine(int position);
        float getStall();
    }
}
