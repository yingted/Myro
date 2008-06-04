//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNxtTypes.cs $ $Revision$
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using Microsoft.Ccr.Core;
using legonxt = Microsoft.Robotics.Services.LegoNxt;
using http = Microsoft.Dss.Core.DsspHttp;
using soap = W3C.Soap;
using dssp = Microsoft.Dss.ServiceModel.Dssp;

using Microsoft.Dss.Core.DsspHttp;
using System.Collections.ObjectModel;
using Microsoft.Dss.Core.Attributes;
using System.ComponentModel;

namespace Microsoft.Robotics.Services.LegoNxt
{
    
    public static class Contract
    {
        public const string Identifier = "http://schemas.microsoft.com/robotics/2006/05/legonxt.html";
    }

    [ServicePort]
    public class LegoNxtOperations : PortSet<
        dssp.DsspDefaultLookup, 
        dssp.DsspDefaultDrop, 
        Get, 
        HttpGet,
        HttpPost,
        Configure,
        SelectiveSubscribe, //IMPORTANT: Because SelectiveSubscribe inherits from Subscribe, it needs to go on top.
        Subscribe,
        UpdateStatus, // Notification only
        SendLegoGetBatteryLevel,
        SendLegoGetCurrentProgramName,
        SendLegoPlaySoundFile,
        SendLegoPlayTone,
        SendLegoSetBrickName,
        SendLegoStartProgram,
        SendLegoStopProgram,
        SendLegoStopSoundPlayback,
        SendLegoCommand>    // IMPORTANT: All other "SendLego..." commands inherit from SendLegoCommand and must be listed first.
    {
    }


    [Description("Gets the complete state of the LEGO NXT brick.")]
    public class Get : dssp.Get<dssp.GetRequestType, PortSet<LegoNxtState, soap.Fault>> { }

    #region Update State Operation Types
    public class UpdateStatePort : PortSet<UpdateStatus, UpdateResponse, ReplaceState, SensorNotification> { }


    [Description("Indicates a change to (or changes) the NXT Brick status.")]
    public class UpdateStatus : dssp.Update<LegoStatusNotification, PortSet<dssp.DefaultUpdateResponseType, soap.Fault>>
    {
        /// <summary>
        /// Default Constructor
        /// </summary>
        public UpdateStatus() { }

        /// <summary>
        /// Initialization Constructor
        /// </summary>
        public UpdateStatus(LegoConnectionStatus status)
        {
            this.Body = new LegoStatusNotification(status);
        }
    }

    public class ReplaceState : dssp.Replace<LegoNxtState, PortSet<dssp.DefaultReplaceResponseType, soap.Fault>>
    {
        /// <summary>
        /// Default Constructor
        /// </summary>
        public ReplaceState() { }

        /// <summary>
        /// Initialization Constructor
        /// </summary>
        public ReplaceState(LegoNxtState body)
        {
            this.Body = body;
        }
    }
    public class UpdateResponse : dssp.Update<LegoResponse, PortSet<dssp.DefaultUpdateResponseType, soap.Fault>>
    {
        /// <summary>
        /// Default Constructor
        /// </summary>
        public UpdateResponse() { }

        /// <summary>
        /// Initialization Constructor
        /// </summary>
        public UpdateResponse(LegoResponse body)
        {
            this.Body = body;
        }
    }

    #endregion

    [Description("Configures (or indicates a change to) the entire state of the NXT Brick.")]
    public class Configure : dssp.Replace<LegoNxtState, PortSet<dssp.DefaultReplaceResponseType, soap.Fault>>
    {
        /// <summary>
        /// Default Constructor
        /// </summary>
        public Configure() { }

        /// <summary>
        /// Initialization Constructor
        /// </summary>
        public Configure(LegoNxtState config)
        {
            this.Body = config;
        }
    }

    public class Subscribe : dssp.Subscribe<dssp.SubscribeRequestType, PortSet<dssp.SubscribeResponseType, soap.Fault>> { }

    [Description("Sends a command to the NXT brick.")]
    public class SendLegoCommand : dssp.Submit<LegoCommand, PortSet<LegoResponse, soap.Fault>> 
    {
        public SendLegoCommand() { }
        public SendLegoCommand(LegoCommand cmd) 
        {
            this.Body = cmd;
        }
    }


    /// <summary>
    /// Selectivly subscribe to the NXT sensors and motors
    /// Subscribe by sensor type and port
    /// Possible names: Touch, Sonar, Light, Sound, Angle, Encoder, Button, Motor
    /// Possible ports: 1, 2, 3, 4
    /// </summary>
    public class SelectiveSubscribe : dssp.Subscribe<CustomSubscribeRequestType, PortSet<dssp.SubscribeResponseType, soap.Fault>> { }

    /// <summary>
    /// For custom subscriptions
    /// </summary>
    [DataContract]
    public class CustomSubscribeRequestType : dssp.SubscribeRequestType
    {
        /// <summary>
        /// The list of sensors to subscribe to
        /// </summary>
        [DataMember]
        [Description("The list of available sensors.")]
        public Collection<SensorDefinition> Sensors;

    }

    [DataContract]
    public class SensorValue
    {
        [DataMember]
        [Description("The current value of the sensor.")]
        public int Value;

        public SensorValue() { }
        public SensorValue(int val)
        {
            Value = val;
        }
    }

    [DataContract]
    public class SensorDefinition
    {        
        /// <summary>
        /// The type of the sensor you want to subscribe to
        /// possible values: Touch, Sonar, Light, Sound, Angle, Encoder, Button, Motor
        /// </summary>
        [DataMember]
        [Description("The sensor type (touch, sonar, light, sound, angle, encoder, button, motor).")]
        public SensorType Type;

        /// <summary>
        /// The port of the sensor you want to subscribe to
        /// possible values: 1, 2, 3, 4
        /// NOTE: Angle, Encoder, and Button sensors cannot be on port 4
        /// </summary>
        [DataMember]
        [Description("The input port to which the sensor is connected (1, 2, 3, or 4).\nAngle, encoder, and button sensors cannot be on port 4.")]
        public int Port;

        public SensorDefinition() { }
        public SensorDefinition(SensorType type, int port) 
        {
            Type = type;
            Port = port;
        }

        /// <summary>
        /// Get notification type
        /// </summary>
        /// <param name="sensor"></param>
        /// <returns></returns>
        public static SensorType GetSensorType(string sensor)
        {
            try
            {
                return (SensorType)Enum.Parse(typeof(SensorType), sensor, true);
            }
            catch
            {
                return SensorType.Null;
            }
        }

        /// <summary>
        /// Get notification type
        /// </summary>
        /// <param name="sensor"></param>
        /// <returns></returns>
        public static SensorType GetNotificationType(int sensor)
        {
            try
            {
                SensorType notificationType = (SensorType)sensor;
                return notificationType;
            }
            catch
            {
                return SensorType.Null;
            }
        }

        /// <summary>
        /// Microsoft defined sensor identifiers.
        /// This must be kept in sync with the
        /// MSRS program which runs on the LEGO 
        /// NXT Brick.
        /// </summary>
        [DataContract]
        [Description("Sensor notification request.")]
        public enum SensorType
        {
            /// <summary>
            /// No Notifications selected
            /// </summary>
            [Description("No notifications.")]
            Null = 0,

            /// <summary>
            /// Request Motor Angle notifications
            /// </summary>
            [Description("Motor shaft angle.")]
            Angle = 1,

            /// <summary>
            /// Request LEGO Brick button pressed notifications
            /// </summary>
            [Description("NXT brick button pressed.")]
            Button = 2,

            /// <summary>
            /// Request Motor Encoder notifications
            /// </summary>
            [Description("Motor encoder.")]
            Encoder = 3,

            /// <summary>
            /// Request Light On Sensor notifications
            /// </summary>
            [Description("Light sensor with LED on.")]
            LightOn = 4,

            /// <summary>
            /// Request Sonar Sensor notifications
            /// </summary>
            [Description("Ultrasonic sensor.")]
            Sonar = 5,

            /// <summary>
            /// Request Sound Sensor notifications
            /// </summary>
            [Description("Sound sensor.")]
            Sound = 6,

            /// <summary>
            /// Request Touch Sensor notifications 
            [Description("Touch sensor.")]
            /// </summary>
            Touch = 7,

            /// <summary>
            /// Request Light Off Sensor notifications
            /// </summary>
            [Description("Light sensor with the LED off.")]
            LightOff = 8,

            /// <summary>
            /// Request Motor power notifications
            /// </summary>
            [Description("Motor power.")]
            Motor = 9,

        }
    }

}
