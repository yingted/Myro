//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTBackupBeepTypes.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Core.DsspHttp;

using System;
using System.Net;
using System.Collections.Generic;

using W3C.Soap;
using Microsoft.Dss.Core.Attributes;
using System.ComponentModel;

namespace Microsoft.Robotics.Services.LegoNxt.BackupBeep
{
    
    public static class Contract
    {
        public const string Identifier = "http://schemas.microsoft.com/2006/06/legonxtbackupbeep.html";
    }

    [DataContract]
    public class LegoNxtBackupBeepState
    {
        private int _playDuration;
        private int _pauseDuration;
        private int _frequency;
        private int _motor1;
        private int _motor2;
        

        [DataMember]
        [Description ("Identifies how long to play the tone.")]
        public int PlayDuration
        {
            get { return _playDuration; }
            set { _playDuration = value; }
        }

        [DataMember]
        [Description("Identifies how long to pause before playing the next tone.")]
        public int PauseDuration
        {
            get { return _pauseDuration; }
            set { _pauseDuration = value; }
        }

        [DataMember]
        [Description("The frequency of the tone.")]
        public int Frequency
        {
          get { return _frequency; }
          set { _frequency = value; }
        }
        
        [DataMember]
        [Description("The device in motor port A.")]
        public int Motor1Port
        {
            get { return _motor1; }
            set { _motor1 = value; }
        }

        [DataMember]
        [Description ("The device in motor port B.")]
        public int Motor2Port
        {
            get { return _motor2; }
            set { _motor2 = value; }
        }
    }

    [ServicePort]
    public class LegoNxtBackupBeepOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop, 
        Get, 
        Replace,
        ReliableSubscribe,
        Subscribe,
        HttpGet>
    {
    }

    [Description("Gets the Backup Beep state.")]
    public class Get : Get<GetRequestType, PortSet<LegoNxtBackupBeepState, Fault>> { }

    [Description("Changes (or indicates a change to) the entire state of the Backup Beep.")]
    public class Replace : Replace<LegoNxtBackupBeepState, PortSet<DefaultReplaceResponseType, Fault>> 
    {
        /// <summary>
        /// Default Constructor
        /// </summary>
        public Replace()
        {
            this.Body = new LegoNxtBackupBeepState();
        }

        /// <summary>
        /// Initialization Constructor
        /// </summary>
        public Replace(LegoNxtBackupBeepState state)
        {
            this.Body = state;
        }

    }

    public class Subscribe : Subscribe<SubscribeRequestType, PortSet<SubscribeResponseType, Fault>> { }
    public class ReliableSubscribe : Subscribe<ReliableSubscribeRequestType, DsspResponsePort<SubscribeResponseType>, LegoNxtBackupBeepOperations> { }

}
