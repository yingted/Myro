//-----------------------------------------------------------------------
//  1 Dimensional LED Array 
//  Generic Service Contract
// 
//  Can control LEDs through:
//    * Individual on/off commands
//    * 'Binary' display (up to 32 LEDs)
//    * 'Volume' display (0 = all off, 0.5 = half LEDs on, 1 = all on)
//
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.ServiceModel.Dssp;

using System;
using System.Net;
using System.Collections.Generic;

using W3C.Soap;
using System.ComponentModel;

namespace Microsoft.Robotics.Services.LEDArray
{
    /// <summary>
    /// The Dss Contract Definition
    /// </summary>
    [DisplayName("Generic 1D LED Array")]
    [Description("Provides access to LED Array.")]
    public static class Contract
    {
        /// <summary>
        /// Contract Identifier
        /// </summary>
        public const string Identifier = "http://schemas.microsoft.com/2007/01/led1darray.html";
    }

    /// <summary>
    /// LEDArray Port
    /// </summary>
    [ServicePort]
    public class LED1DArrayOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop, 
        Get, 
        Replace,
        SetSingle,
        SetBinary,
        SetVolume> 
    {
    }

    /// <summary>
    /// Operation Get: Gets the state
    /// </summary>
    [Description("Gets the current state of the LED array.")]
    public class Get : Get<GetRequestType, PortSet<LED1DArrayState, Fault>>
    {
    }

    /// <summary>
    /// Operation Replace: Replaces state of LED array
    /// </summary>
    [Description("Changes the entire LED array state.")]
    public class Replace : Replace<LED1DArrayState, PortSet<DefaultReplaceResponseType, Fault>>
    {
    }

    /// <summary>
    /// Operation SetSingle: Sets the state of a single LED
    /// </summary>
    [Description("Sets the state of a single LED.")]
    public class SetSingle : Update<SetSingleRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    /// <summary>
    /// Operation SetBinary: Sets the state of all the LEDs with a binary output notation
    /// </summary>
    [Description("Sets the state of a single LED.")]
    public class SetBinary : Update<SetBinaryRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    /// <summary>
    /// Operation SetVolume: Sets the state of all the LEDs with a volume output notation
    /// (0 = off, 0.5 = half on, 1 = all on)
    /// </summary>
    [Description("Sets the state of a single LED.")]
    public class SetVolume : Update<SetVolumeRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    #region Message Body Types

    /// <summary>
    /// Body of the SetOne message
    /// </summary>
    [DataContract]
    [DataMemberConstructor]
    public class SetSingleRequest
    {
        private int _which;
        private bool _state;

        /// <summary>
        /// Which LED to set (HardwareIdentifier number)
        /// </summary>
        [DataMember]
        public int Which
        {
            get { return this._which; }
            set { this._which = value; }
        }

        /// <summary>
        /// New state of LED
        /// </summary>
        [DataMember]
        public bool State
        {
            get { return this._state; }
            set { this._state = value; }
        }
    }

    /// <summary>
    /// Body of the SetBinary message
    /// </summary>
    [DataContract]
    [DataMemberConstructor]
    public class SetBinaryRequest
    {
        private uint _binary;

        /// <summary>
        /// new state of LED array in binary notation
        /// </summary>
        public uint Binary
        {
            get { return this._binary; }
            set { this._binary = value; }
        }
    }

    /// <summary>
    /// Body of the SetVolume message
    /// </summary>
    [DataContract]
    [DataMemberConstructor]
    public class SetVolumeRequest
    {
        private double _volume;

        /// <summary>
        /// new state of LED array in volume notation
        /// (0 = off, 0.5 = half on, 1 = all on)
        /// </summary>
        public double Volume
        {
            get { return this._volume; }
            set { this._volume = value; }
        }
    }
    #endregion


}
