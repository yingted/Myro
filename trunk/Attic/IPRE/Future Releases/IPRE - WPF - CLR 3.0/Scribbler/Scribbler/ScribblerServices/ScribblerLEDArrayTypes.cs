using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Security.Permissions;
using xml = System.Xml;
using W3C.Soap;
using led = IPREGenericContracts.LEDarray.Proxy;

namespace IPRE.ScribblerLEDArray
{

    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/scribblerledarray.html";
    }

    /// <summary>
    /// Main state of service
    /// </summary>
    [DataContract]
    public class ScribblerLEDArrayState
    {
        private List<led.LEDVector> _leds;

        /// <summary>
        /// The list of LEDs
        /// </summary>
        [DataMember]
        public List<led.LEDVector> LEDs
        {
            get { return this._leds; }
            set { this._leds = value; }
        }
    }


    /// <summary>
    /// ScribblerLEDArray Port
    /// </summary>
    [ServicePort]
    public class ScribblerLEDArrayOperations : PortSet<
        DsspDefaultLookup,
        DsspDefaultDrop,
        Get,
        Replace,
        SetSingle,
        SetVector>
    {
    }

    /// <summary>
    /// http get
    /// returns entire state
    /// </summary>
    public class Get : Get<GetRequestType, PortSet<ScribblerLEDArrayState, Fault>> { }

    /// <summary>
    /// replaces entire state
    /// </summary>
    public class Replace : Replace<ScribblerLEDArrayState, PortSet<DefaultReplaceResponseType, Fault>> { }

    /// <summary>
    /// Operation SetSingle: Sets the state of a single LED
    /// </summary>
    public class SetSingle : Update<SetSingleRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    /// <summary>
    /// Operation SetBinary: Sets the state of all the LEDs in a vector
    /// </summary>
    public class SetVector : Update<SetVectorRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    /// <summary>
    /// Body of the SetSingle message
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
    /// Body of the SetVector message
    /// </summary>
    [DataContract]
    [DataMemberConstructor]
    public class SetVectorRequest
    {
        private int _which;
        private led.LEDVector _state;

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
        public led.LEDVector State
        {
            get { return this._state; }
            set { this._state = value; }
        }
    }

}
