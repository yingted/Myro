//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTBatteryTypes.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.ServiceModel.Dssp;

using System;
using System.Net;
using System.Collections.Generic;

using W3C.Soap;
using Microsoft.Dss.Core.Attributes;
using battery = Microsoft.Robotics.Services.Battery.Proxy;
using System.ComponentModel;


namespace Microsoft.Robotics.Services.LegoNxt.Battery
{
    
    public static class Contract
    {
        public const string Identifier = "http://schemas.microsoft.com/2006/06/legonxtbattery.html";
    }

    [DataContract]
    public class LegoNxtBatteryState : battery.BatteryState
    {
        private int _pollDelayTime;
        
        /// <summary>
        /// How long to wait between battery queries (in ms)
        /// </summary>
        [DataMember]
        [Description ("How long to wait between battery querires (in ms).")]
        public int PollDelayTime
        {
            get { return _pollDelayTime; }
            set { _pollDelayTime = value; }
        }

    }

    [ServicePort]
    public class LegoNxtBatteryOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop, 
        Get,
        HttpGet>
    {
    }

    [Description("Gets the NXT battery state.")]
    public class Get : Get<GetRequestType, PortSet<LegoNxtBatteryState, Fault>> { }
    [Description("Indicates a change to (or changes) the entire NXT battery state.")]
    public class Replace : Replace<LegoNxtBatteryState, PortSet<DefaultReplaceResponseType, Fault>> { }
}
