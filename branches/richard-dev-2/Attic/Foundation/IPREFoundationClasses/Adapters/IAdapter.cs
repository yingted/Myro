using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;

namespace Myro.Adapters
{
    public class AdapterCreationException : Exception
    {
        public AdapterCreationException(string reason)
            : base(reason)
        {
        }
        public AdapterCreationException(string reason, Exception innerException)
            : base(reason, innerException)
        {
        }
    }

    public class AdapterOperationException : Exception
    {
        public AdapterOperationException(string reason)
            : base(reason)
        {
        }
        public AdapterOperationException(W3C.Soap.Fault failure)
            : base("Fault received", failure.ToException())
        {
        }
        public AdapterOperationException(string reason, Exception innerException)
            : base(reason, innerException)
        {
        }
    }

    /// <summary>
    /// Every adapter class must implement this IAdapter interface
    /// </summary>
    public interface IAdapter<S>
    {
        /// <summary>
        /// Returns the ServiceInfoType (containing the service URI, among other things)
        /// </summary>
        ServiceInfoType ServiceInfo { get; }

        S get();
        void set(S state);
    }




    ///// <summary>
    ///// All adapter types
    ///// </summary>
    //public enum AdapterTypes
    //{
    //    DriveAdapter,
    //    AnalogSensorAdapter,
    //    AnalogSensorArrayAdapter,
    //    WebcamAdapter,
    //    ContactSensorArrayAdapter,
    //    SonarAdapter,
    //    ToneGeneratorAdapter,
    //    LEDArrayAdapter,
    //    TextToSpeech,
    //    OtherAdapter
    //}

    ///// <summary>
    ///// All sensor types
    ///// </summary>
    //public enum SensorTypes
    //{
    //    IRSensor,
    //    LightSensor,
    //    LineSensor,
    //    AnalogSensor,
    //    BumperSensor,
    //    StallSensor,
    //    ContactSensorArray,
    //    DiffDrive,
    //    ToneGenerator,
    //    LEDArray,
    //    SoundSensor,
    //    UltraSonicSonar,
    //    OtherSensor
    //}
}
