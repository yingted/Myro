//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTSoundTypes.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;

using W3C.Soap;
using Microsoft.Dss.Core.Attributes;
using System.ComponentModel;

namespace Microsoft.Robotics.Services.LegoNxt.Sound
{
    
    public static class Contract
    {
        public const string Identifier = "http://schemas.microsoft.com/2006/06/legonxtsound.html";
    }

    [DataContract]
    public class SoundState
    {
        private DateTime _timestamp;
        private int _hardwareIdentifier;
        private int _soundReading;

        /// <summary>
        /// TimeStamp of this sample
        /// </summary>
        [DataMember]
        [Browsable (false)]
        [Description("The timestamp of the most recent NXT sound sensor reading.")]
        public DateTime TimeStamp
        {
            get { return _timestamp; }
            set { _timestamp = value; }
        }

        /// <summary>
        /// Hardware port identifier
        /// </summary>
        [DataMember]
        [Description("The identifier for the hardware port.")]
        public int HardwareIdentifier
        {
            get { return _hardwareIdentifier; }
            set { _hardwareIdentifier = value; }
        }

        /// <summary>
        /// The sound reading
        /// </summary>
        [DataMember]
        [Browsable(false)]
        [Description("The current reading from the NXT sound input sensor.")]
        public int SoundReading
        {
          get { return _soundReading; }
          set { _soundReading = value; }
        }
    }

    [ServicePort]
    public class LegoNxtSoundOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop, 
        Get, 
        Replace,
        ReliableSubscribe,
        Subscribe>
    {
    }

    [Description("Gets the NXT sound sensor state.")]
    public class Get : Get<GetRequestType, PortSet<SoundState, Fault>> { }

    [Description("Changes (or indicates a change to) the entire NXT sound state.")]
    public class Replace : Replace<SoundState, PortSet<DefaultReplaceResponseType, Fault>>
    {
        /// <summary>
        /// Default Constructor
        /// </summary>
        public Replace() { }

        /// <summary>
        /// Initialization Constructor
        /// </summary>
        public Replace(SoundState state)
        {
            this.Body = state;
        }

    }

    public class Subscribe : Subscribe<SubscribeRequestType, PortSet<SubscribeResponseType, Fault>> { }
    public class ReliableSubscribe : Subscribe<ReliableSubscribeRequestType, DsspResponsePort<SubscribeResponseType>, LegoNxtSoundOperations> { }

}
