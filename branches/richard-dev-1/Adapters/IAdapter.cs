using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using Myro;

namespace Myro.Adapters
{
    //public class AdapterCreationException : Exception
    //{
    //    public AdapterCreationException(string reason)
    //        : base(reason)
    //    {
    //    }
    //    public AdapterCreationException(string reason, Exception innerException)
    //        : base(reason, innerException)
    //    {
    //    }
    //}

    //public class AdapterOperationException : Exception
    //{
    //    public AdapterOperationException(string reason)
    //        : base(reason)
    //    {
    //        Console.WriteLine("*** AdapterOperationException *** " + reason);
    //    }
    //    //public AdapterOperationException(W3C.Soap.Fault failure)
    //    //    : base(Strings.FaultReceived, failure.ToException())
    //    //{
    //    //    Console.WriteLine("*** AdapterOperationException *** " + failure.Reason[0]);
    //    //}
    //    public AdapterOperationException(string reason, Exception innerException)
    //        : base(reason, innerException)
    //    {
    //        Console.WriteLine("*** AdapterOperationException *** " + reason);
    //    }
    //}

    public class AdapterArgumentException : Exception
    {
        public AdapterArgumentException(string reason)
            : base(reason)
        {
        }
    }

    /// <summary>
    /// Every adapter class must implement this IAdapter interface
    /// </summary>
    public interface IAdapter
    {
        /// <summary>
        /// Returns the ServiceInfoType (containing the service URI, among other things)
        /// </summary>
        ServiceInfoType ServiceInfo { get; }

        //AdapterTypeEnum AdapterType { get; }
    }

    public interface IAdapterFactory
    {
        /// <summary>
        /// This should contain the list of contracts to which this adapter
        /// can connect.
        /// </summary>
        List<string> SupportedContracts { get; }

        /// <summary>
        /// This method should create the adapter from the given service 
        /// record.  The contract of the service record will be one of the 
        /// contracts in the SupportedContracts property.
        /// </summary>
        /// <param name="service"></param>
        /// <returns></returns>
        IAdapter Create(ServiceInfoType service);
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
