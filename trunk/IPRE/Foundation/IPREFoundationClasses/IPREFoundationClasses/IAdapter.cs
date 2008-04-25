using System;
using System.Collections.Generic;
using System.Text;

namespace IPREFoundationClasses
{
    /// <summary>
    /// All adapter types
    /// </summary>
    public enum AdapterTypes
    {
        DriveAdapter,
        AnalogSensorAdapter,
        AnalogSensorArrayAdapter,
        WebcamAdapter,
        ContactSensorArrayAdapter,
        SonarAdapter,
        ToneGeneratorAdapter,
        LEDArrayAdapter,
        TextToSpeech,
        OtherAdapter
    }

    /// <summary>
    /// All sensor types
    /// </summary>
    public enum SensorTypes
    {
        IRSensor,
        LightSensor,
        LineSensor,
        AnalogSensor,
        BumperSensor,
        StallSensor,
        ContactSensorArray,
        DiffDrive,
        ToneGenerator,
        LEDArray,
        SoundSensor,
        UltraSonicSonar,
        OtherSensor
    }

    /// <summary>
    /// Every adapter class must implement this IAdapter interface
    /// </summary>
    public interface IAdapter
    {
        /// <summary>
        /// Returns the type of adapter
        /// </summary>
        AdapterTypes AdapterType { get; }

        /// <summary>
        /// Returns the URI of the service assicociated with this adapter object.
        /// </summary>
        string ServiceUri { get; }
    }
}
