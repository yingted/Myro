//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTBumperTypes.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;

using System;
using System.Net;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Security.Permissions;
using xml = System.Xml;
using W3C.Soap;

using bumper = Microsoft.Robotics.Services.ContactSensor.Proxy;
using nxt = Microsoft.Robotics.Services.LegoNxt.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;

/* Notes on Lego NXT Binary Sensor
 *   Identifier must be the port number on the NXT Brick. Range: 1 - 4.
 */

namespace Microsoft.Robotics.Services.LegoNxt.ContactSensor
{
    /// <summary>
    /// ContactSensorArray Port
    /// </summary>
    [ServicePort]
    public class ContactSensorArrayOperations : PortSet<
        DsspDefaultLookup,
        DsspDefaultDrop,
        bumper.Get,
        bumper.Replace,
        bumper.Update,
        bumper.ReliableSubscribe,
        bumper.Subscribe,
        UpdateConfig,
        QueryConfig,
        HttpGet>
    {
    }

    /// <summary>
    /// Update the LEGO specific bumper configuration.
    /// </summary>
    public class UpdateConfig : Update<List<LegoNxtBumperConfig>, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    /// <summary>
    /// Update the LEGO specific bumper configuration.
    /// </summary>
    public class QueryConfig : Query<LegoNxtBumperState, PortSet<LegoNxtBumperState, Fault>>
    {
    }

    /// <summary>
    /// Additional configuration which is necessary to 
    /// configure any LEGO sensor as a contact sensor.
    /// </summary>
    [DataContract]
    public class LegoNxtBumperConfig
    {
        private int _hardwareIdentifier;
        private nxt.SensorDefinition.SensorType _sensorType;
        private string _name;
        private int _thresholdLow;
        private int _thresholdHigh;

        /// <summary>
        /// Indentifies the LEGO Port (1-4) in which this sensor is plugged.
        /// </summary>
        [DataMember]
        [Description ("The NXT brick port (1-4) in which the sensor is plugged.")]
        public int HardwareIdentifier
        {
            get { return _hardwareIdentifier; }
            set { _hardwareIdentifier = value; }
        }

        /// <summary>
        /// The name of this sensor
        /// </summary>
        [DataMember]
        [Description("The descriptive name for this sensor.")]
        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        /// <summary>
        /// One of the LegoBrickConfig.AvailableSensorTypes
        /// </summary>
        [DataMember]
        [Description("The NXT sensor types.")]
        public nxt.SensorDefinition.SensorType SensorType
        {
            get { return _sensorType; }
            set { _sensorType = value; }
        }

        /// <summary>
        /// The lowest value which will trigger a bumper event
        /// </summary>
        [DataMember]
        [Description("The lowest value which will trigger a contact event.")]
        public int ThresholdLow
        {
            get { return _thresholdLow; }
            set { _thresholdLow = value; }
        }

        /// <summary>
        /// The highest value which will trigger a bumper event
        /// </summary>
        [DataMember]
        [Description("The highest value which will trigger a contact event.")]
        public int ThresholdHigh
        {
            get { return _thresholdHigh; }
            set { _thresholdHigh = value; }
        }

    }

    /// <summary>
    /// LEGO NXT Bumper Status
    /// </summary>
    [DataContract]
    [Description ("NXT touch sensor status.")]
    public enum LegoNxtBumperStatus
    {
        /// <summary>
        /// Not Initialized
        /// </summary>
        Uninitialized = 0,

        /// <summary>
        /// Service has started, but not yet configured
        /// </summary>
        NotYetConfigured = 1,

        /// <summary>
        /// Waiting for LEGO NXT Brick to complete initialization
        /// </summary>
        WaitingForNXT = 2,

        /// <summary>
        /// Receiving sensor data from LEGO NXT
        /// </summary>
        ConnectedToNXT = 3,

        /// <summary>
        /// Service is ending
        /// </summary>
        ShuttingDown = 4,
    }

    /// <summary>
    /// A list of bumper configurations
    /// </summary>
    [DataContract]
    [Description("The sensor state.")]
    public class LegoNxtBumperState
    {
        [DataMember]
        [Description("The sensor configuration list.")]
        public List<LegoNxtBumperConfig> BumperConfigList;

        [DataMember]
        [Description("The state of the set of sensors.")]
        public bumper.ContactSensorArrayState ContactSensorArrayState;

        [DataMember]
        [Description("The current sensor status.")]
        public LegoNxtBumperStatus Status;
    }

    

    /// <summary>
    /// LEGO Bumper Contract 
    /// </summary>
    public static class Contract
    {
        public const string Identifier = "http://schemas.microsoft.com/2006/06/lego.nxt.bumper.html";
    }



    #region Private Operations

    class InternalOperations : Port<UpdateStatus>
    {
    }


    /// <summary>
    /// Update the Service Status
    /// </summary>
    class UpdateStatus : Update<LegoNxtBumperStatus, PortSet<DefaultUpdateResponseType, Fault>>
    {
        /// <summary>
        /// Default setting is True
        /// </summary>
        public UpdateStatus() { }

        /// <summary>
        /// Initialization Constructor
        /// </summary>
        public UpdateStatus(LegoNxtBumperStatus status)
        {
            this.Body = status;
        }
    }

    #endregion

}
