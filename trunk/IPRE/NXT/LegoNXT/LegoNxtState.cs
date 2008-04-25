//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNxtState.cs $ $Revision$
//-----------------------------------------------------------------------

using System;
using System.ComponentModel;

using Microsoft.Dss.Core.Attributes;

namespace Microsoft.Robotics.Services.LegoNxt
{

    #region NXT State & Types Data Contracts

    [DataContract]
    [Description ("The NXT configuration state.")]
    public class LegoNxtState : ICloneable
    {
        private int _comport;
        private int _baudrate;
        private LegoButtons _legoButtons;

        private LegoConnectionStatus _connected;
        private int[] _sensorPort = new int[4];
        private int[] _motorSensorPort = new int[3];
        private int[] _buttonSensorPort = new int[3];
        private int[] _motorOutputPort = new int[3];
        private double _batteryVoltage;
        private LegoBrickConfig _brickConfig;
        private string _brickName;


        #region Connection

        [DataMember]
        [Description ("The PC serial port used for the connection.")]
        public int ComPort
        {
            get { return this._comport; }
            set { this._comport = value; }
        }

        [DataMember]
        [Description ("The baud rate setting for the connection.")]
        public int BaudRate
        {
            get { return this._baudrate; }
            set { this._baudrate = value; }
        }

        [DataMember]
        [Browsable (false)]
        [Description ("The connection state.")]
        public LegoConnectionStatus Connected
        {
            get { return _connected; }
            set { _connected = value; }
        }
        #endregion

        [DataMember]
        [Description("The set of buttons on the NXT brick.")]
        public LegoButtons LegoButtons
        {
            get { return _legoButtons; }
            set { _legoButtons = value; }
        }

        [DataMember]
        [Description("A sensor port on the NXT brick.")]
        public int[] SensorPort
        {
            get { return this._sensorPort; }
            set { this._sensorPort = value; }
        }

        [DataMember]
        [Description("A rotation sensor port on the NXT brick.")]
        public int[] MotorSensorPort
        {
            get { return this._motorSensorPort; }
            set { this._motorSensorPort = value; }
        }

        [DataMember]
        [Description("A button on the NXT brick.")]
        public int[] ButtonSensorPort
        {
            get { return this._buttonSensorPort; }
            set { this._buttonSensorPort = value; }
        }

        [DataMember]
        [Description("A motor output port on the NXT brick.")]
        public int[] MotorOutputPort
        {
            get { return this._motorOutputPort; }
            set { this._motorOutputPort = value; }
        }

        [DataMember]
        [Description("The battery voltage on the NXT brick.")]
        public double BatteryVoltage
        {
            get { return this._batteryVoltage; }
            set { this._batteryVoltage = value; }
        }

        [DataMember]
        [Description("The current configuration of the NXT brick.")]
        public LegoBrickConfig BrickConfig
        {
            get { return this._brickConfig; }
            set { this._brickConfig = value; }
        }

        [DataMember]
        [Description("The descriptive identifier for the NXT brick.")]
        public string BrickName
        {
            get { return this._brickName; }
            set { this._brickName = value; }
        }

        #region ICloneable Members

        public object Clone()
        {
            LegoNxtState config = new LegoNxtState();
            config.ComPort = this.ComPort;
            config.Connected = this.Connected;
            config.SensorPort = this.SensorPort;
            config.MotorSensorPort = this.MotorSensorPort;
            config.ButtonSensorPort = this.ButtonSensorPort;
            config.MotorOutputPort = this.MotorOutputPort;
            config.BatteryVoltage = this.BatteryVoltage;
            config.BrickConfig = this.BrickConfig;
            return config;
        }

        #endregion
    }

    [DataContract]
    [DataMemberConstructor]
    public class SensorConfig
    {
        private SensorDefinition.SensorType _type = SensorDefinition.SensorType.Null;
        private int _lowThresh;
        private int _highThresh;
        private bool _externalRange;

        [DataMember(IsRequired=true)]
        [Description("The sensor type.")]
        public SensorDefinition.SensorType Type
        {
            get { return _type; }
            set { _type = value; }
        }

        [DataMember]
        [Description("The lowest value which will trigger a sensor event.")]
        public int LowThresh
        {
            get { return _lowThresh; }
            set { _lowThresh = value; }
        }
        [DataMember]
        [Description("The highest value which will trigger a contact event.")]
        public int HighThresh
        {
            get { return _highThresh; }
            set { _highThresh = value; }
        }
        [DataMember]
        [Description("Specifies whether to use the Low and High Threshhold")]
        public bool ExternalRange
        {
            get { return _externalRange; }
            set { _externalRange = value; }
        }
    }

    [DataContract]
    [DataMemberConstructor]
    public class MotorConfig
    {
        private string _type = "Null";
        private int _lowThresh;
        private int _highThresh;
        private bool _externalRange;
        private int _ticksPerRevolution = 6;

        [DataMember]
        [Description("The device type.")]
        public string Type
        {
            get { return _type; }
            set { _type = value; }
        }
        [DataMember]
        [Description("The lowest value which will trigger a sensor event.")]
        public int LowThresh
        {
            get { return _lowThresh; }
            set { _lowThresh = value; }
        }
        [DataMember]
        [Description("The highest value which will trigger a sensor event.")]
        public int HighThresh
        {
            get { return _highThresh; }
            set { _highThresh = value; }
        }
        [DataMember]
        [Description("Specifies whether to use the Low and High Threshhold")]
        public bool ExternalRange
        {
            get { return _externalRange; }
            set { _externalRange = value; }
        }
        [DataMember]
        [Description("The number of ticks per revolution.")]
        public int TicksPerRevolution
        {
            get { return _ticksPerRevolution; }
            set { _ticksPerRevolution = value; }
        }
    }

    [DataContract]
    [DataMemberConstructor]
    [Description("The current configuration of the NXT brick.")]
    public class LegoBrickConfig
    {
        private SensorConfig[] _sensorPort;
        private MotorConfig[] _motorPort;
        private string[] _ButtonPort;
       
        //for xslt
        #region For XSLT
        private string[] _availableSensorTypes;
        private string[] _availableMotorSensorTypes;
        private string[] _availableButtonSensorTypes;
        
        #endregion        

        [DataMember]
        [Description("The sensor port configuration.")]
        public SensorConfig[] SensorPort
        {
            get { return _sensorPort; }
            set { _sensorPort = value; }
        }

        [DataMember]
        [Description("The motor port configuration.")]
        public MotorConfig[] MotorPort
        {
            get { return _motorPort; }
            set { _motorPort = value; }
        }

        [DataMember]
        [Description("The NXT brick button configuration.")]
        public string[] ButtonPort
        {
            get { return _ButtonPort; }
            set { _ButtonPort = value; }
        }


        #region For XSLT
        [DataMember]
        [Description("The available sensor types.")]
        [Browsable(false)]
        public string[] AvailableSensorTypes
        {
            get { return _availableSensorTypes; }
            set { _availableSensorTypes = value; }
        }

        [DataMember]
        [Description("The available motor types.")]
        [Browsable(false)]
        public string[] AvailableMotorSensorTypes
        {
            get { return _availableMotorSensorTypes; }
            set { _availableMotorSensorTypes = value; }
        }

        [DataMember]
        [Description("The available button types.")]
        [Browsable(false)]
        public string[] AvailableButtonSensorTypes
        {
            get { return _availableButtonSensorTypes; }
            set { _availableButtonSensorTypes = value; }
        } 
        #endregion


        public LegoBrickConfig()
        {
            SensorPort = new SensorConfig[4];
            SensorPort[0] = new SensorConfig();
            SensorPort[1] = new SensorConfig();
            SensorPort[2] = new SensorConfig();
            SensorPort[3] = new SensorConfig();

            MotorPort = new MotorConfig[3];
            MotorPort[0] = new MotorConfig();
            MotorPort[1] = new MotorConfig();
            MotorPort[2] = new MotorConfig();

            ButtonPort = new string[3] { "Null", "Null", "Null" };

            //for xslt
            #region For XSLT
            AvailableSensorTypes = new string[6] { "Null", "Touch", "LightOn", "LightOff", "Sound", "Sonar" };
            AvailableMotorSensorTypes = new string[3] { "Null", "Angle", "Encoder" };
            AvailableButtonSensorTypes = new string[2] { "Null", "Button" };
            #endregion
        }
    }

    /// <summary>
    /// Lego Connection Status
    /// </summary>
    [DataContract]
    [Description("The NXT connection state.")]
    public enum LegoConnectionStatus
    {
        /// <summary>
        /// Not Connected
        /// </summary>
        [Description("Not Connected")]
        NotConnected = 0x00,

        /// <summary>
        /// Connected Pc to Lego
        /// </summary>
        Connected = 0x01,

        /// <summary>
        /// ConnectedLegoToPc
        /// </summary>
        ConnectedLegoToPc = 0x02,

        /// <summary>
        /// A connection is established
        /// but the msrs program on the LEGO 
        /// is not running.
        /// </summary>
        MSRSProgramTerminated = 0x03,
    }

    /// <summary>
    /// LEGO Buttons
    /// </summary>
    [DataContract]
    [Description("The buttons on the NXT brick.")]
    public class LegoButtons
    {

        /// <summary>
        /// Cancel Button
        /// </summary>
        [DataMember]
        [Description("The Cancel button.")]
        public bool Cancel;

        /// <summary>
        /// Right Button
        /// </summary>
        [DataMember]
        [Description("The Right button.")]
        public bool Right;
        
        /// <summary>
        /// Left Button
        /// </summary>
        [DataMember]
        [Description("The Left button.")]
        public bool Left;
        
        /// <summary>
        /// Enter Button
        /// </summary>
        [DataMember]
        [Description("The Enter button.")]
        public bool Enter;
    }

    /// <summary>
    /// Represents the status of the LEGO NXT Service
    /// </summary>
    [DataContract]
    public class LegoStatusNotification
    {
        public LegoStatusNotification() { }
        public LegoStatusNotification(LegoConnectionStatus status) { this.Status = status; }

        [DataMember]
        public LegoConnectionStatus Status;
    }

    #endregion

}
