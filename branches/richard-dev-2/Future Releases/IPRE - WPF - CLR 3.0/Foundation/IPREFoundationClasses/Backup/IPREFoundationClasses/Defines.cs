using System;
using System.Collections.Generic;
using System.Text;

namespace IPREFoundationClasses
{
    /// <summary>
    /// Structure containing #define equivalent entries.
    /// </summary>
    public class Defines
    {
        public static float INVALID_FLOAT = float.MinValue;
        public static int INVALID_INT = int.MinValue;
        private static Dictionary<SensorTypes, String> SensorString = new Dictionary<SensorTypes, string>();
        private static bool SSInitialized = false;

        public static Dictionary<SensorTypes, String>  getSensorString()
        {
            if(!SSInitialized)
            {
                SensorString.Clear();
                SensorString.Add(SensorTypes.IRSensor, "ir");
                SensorString.Add(SensorTypes.LightSensor, "light");
                SensorString.Add(SensorTypes.LineSensor, "line");
                SensorString.Add(SensorTypes.SoundSensor, "sound");
                SensorString.Add(SensorTypes.UltraSonicSonar, "ultrasonic");
                SensorString.Add(SensorTypes.StallSensor, "stall");
                SensorString.Add(SensorTypes.ContactSensorArray, "contact");
                SSInitialized = true;
            }
            return SensorString;
        }

    };


}
