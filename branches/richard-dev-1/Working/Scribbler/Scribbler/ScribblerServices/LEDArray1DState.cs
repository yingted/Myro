//-----------------------------------------------------------------------
//  1 Dimensional LED Array 
//  Generic Service Contract
// 
//  Can control LEDs through:
//    * Individual on/off commands
//    * All on/off commands
//    * 'Binary' display (up to 32 LEDs)
//    * 'Volume' display (0 = all off, 0.5 = half LEDs on, 1 = all on)
//
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.ComponentModel;

using Microsoft.Robotics.PhysicalModel;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Robotics.PhysicalModel.Proxy;

namespace Microsoft.Robotics.Services.LEDArray
{

    /// <summary>
    /// Definition of a LED
    /// </summary>
    [DataContract]
    public class LED
    {
        private int _hardwareIdentifier;
        private bool _state;
        private Pose _pose;
        private DateTime _timeStamp;

        /// <summary>
        /// Descriptive identifier number for this sensor
        /// </summary>
        [DataMember]
        [DataMemberConstructor(Order = 1)]
        [Description("Identifies the hardware port.")]
        public int HardwareIdentifier
        {
            get { return this._hardwareIdentifier; }
            set { this._hardwareIdentifier = value; }
        }

        /// <summary>
        /// Last time sensor was updated
        /// </summary>
        [DataMember]
        [Browsable(false)]
        [Description("The timestamp for the sensor update.")]
        public DateTime TimeStamp
        {
            get { return this._timeStamp; }
            set { this._timeStamp = value; }
        }

        /// <summary>
        /// The state of the binary sensor
        /// </summary>
        [DataMember]
        [Browsable(false)]
        [Description("Identifies the state of the sensor.")]
        public bool State
        {
            get { return this._state; }
            set { this._state = value; }
        }

        /// <summary>
        /// Position and orientation
        /// </summary>
        [DataMember]
        [Description("The position and orientation of the sensor.")]
        public Pose Pose
        {
            get { return _pose; }
            set { _pose = value; }
        }
    }

    /// <summary>
    /// A list of LEDs
    /// </summary>
    [DataContract]
    public class LED1DArrayState
    {
        private List<LED> _leds;

        /// <summary>
        /// The list of LEDs
        /// </summary>
        [DataMember]
        [Description("The set of LEDs.")]
        public List<LED> LEDs
        {
            get { return this._leds; }
            set { this._leds = value; }
        }
    }

}
