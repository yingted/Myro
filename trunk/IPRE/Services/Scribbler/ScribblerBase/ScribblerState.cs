// Copyright (c) Microsoft Corporation.  All rights reserved.

//------------------------------------------------------------------------------
// ScribblerState.cs
//
//
//      Ben Axelrod 08/28/2006
//
//------------------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;
using Microsoft.Dss.Core.Attributes;
using W3C.Soap;


namespace Myro.Services.Scribbler.ScribblerBase
{

    /// <summary>
    /// Main state of service
    /// </summary>
    [DataContract()]
    public class ScribblerState
    {
        /// <summary>
        /// Name of robot.
        /// <remarks>Up to 8 characters</remarks>
        /// </summary>
        [DataMember]
        public string RobotName;

        /// <summary>
        /// The presence of no obstacle about 10 inches in the front left of robot
        /// </summary>
        [DataMember]
        public bool IRLeft;

        /// <summary>
        /// The presence of no obstacle about 10 inches in the front right of robot
        /// </summary>
        [DataMember]
        public bool IRRight;

        [DataMember]
        public short ObstacleLeft;

        [DataMember]
        public short ObstacleCenter;

        [DataMember]
        public short ObstacleRight;

        [DataMember]
        public bool LineLeft;

        [DataMember]
        public bool LineRight;

        [DataMember]
        public bool Stall;

        [DataMember]
        public int LightLeft;

        [DataMember]
        public int LightRight;

        [DataMember]
        public int LightCenter;

        //[DataMember]
        //public SensorConfig LightLeftConfig;

        //[DataMember]
        //public SensorConfig LightRightConfig;

        //[DataMember]
        //public SensorConfig LightCenterConfig;

        /// <summary>
        /// Speed of left motor.
        /// 0 = Full speed backwards
        /// 100 = stopped
        /// 200 = full speed forwards
        /// </summary>
        [DataMember]
        public int MotorLeft;

        /// <summary>
        /// Speed of right motor.
        /// 0 = Full speed backwards
        /// 100 = stopped
        /// 200 = full speed forwards
        /// </summary>
        [DataMember]
        public int MotorRight;

        [DataMember]
        public bool LEDLeft;

        [DataMember]
        public bool LEDRight;

        [DataMember]
        public bool LEDCenter;

        [DataMember]
        public bool LEDFront;

        [DataMember]
        public byte LEDBack;

        /// <summary>
        /// Whether or not service is connected to the physical robot through a COM port
        /// </summary>
        [DataMember]
        public bool Connected;

        /// <summary>
        /// Which COM port robot is connected to if any.
        /// <remarks>Setting this less than or equal to 0 will force the service to scan 
        /// all COM ports for available robots.</remarks>
        /// </summary>
        [DataMember]
        public int ComPort;

        /// <summary>
        /// The timestamp of the last time *BOTH* bumpers were updated
        /// </summary>
        [DataMember]
        public DateTime BumpersLastUpdated;

        /// <summary>
        /// The timestamp of the last time *BOTH* line sensors were updated
        /// </summary>
        [DataMember]
        public DateTime LineSensorsLastUpdated;

        /// <summary>
        /// The timestamp of the last time *ALL* light sensors were updated
        /// </summary>
        [DataMember]
        public DateTime LightSensorsLastUpdated;
    }



    /// <summary>
    /// Custom subscriptions
    /// IRLeft, IRRight, LineLeft, LineRight, Stall, LightLeft, LightRight, LightCenter, Motors
    /// </summary>
    [DataContract]
    public class MySubscribeRequestType : SubscribeRequestType
    {
        //The list of sensors to subscribe to
        [DataMember]
        public List<string> Sensors;
    }



    /// <summary>
    /// An array of bytes to send to Scribbler
    /// </summary>
    [DataContract]
    public class ScribblerCommand
    {
        [DataMember]
        public byte CommandType;
        [DataMember]
        public byte[] Data;
        [DataMember]
        public bool HasEcho;
        [DataMember]
        public int ResponseLength;

        [DataMember]
        public Nullable<byte> EndMarker1;
        [DataMember]
        public Nullable<byte> EndMarker2;

        /// <summary>
        /// Standard constructor
        /// </summary>
        public ScribblerCommand()
        { }

        public ScribblerCommand(ScribblerHelper.Commands command, byte[] data, bool hasEcho, int responseLength)
        {
            CommandType = (byte)command;
            Data = data;
            HasEcho = hasEcho;
            ResponseLength = responseLength;
        }

        /// <summary>
        /// For "get" commands and simple "set" commands
        /// </summary>
        /// <param name="command"></param>
        public ScribblerCommand(ScribblerHelper.Commands command)
        {
            Data = new byte[ScribblerHelper.CommandSize(command) - 1];
            CommandType = (byte)command;
            HasEcho = ScribblerHelper.HasEcho(command);
            ResponseLength = ScribblerHelper.ReturnSize(command);
        }

        public ScribblerCommand(ScribblerHelper.Commands command, byte data)
        {
            Data = new byte[ScribblerHelper.CommandSize(command) - 1];
            CommandType = (byte)command;
            Data[0] = data;
            HasEcho = ScribblerHelper.HasEcho(command);
            ResponseLength = ScribblerHelper.ReturnSize(command);
        }

        /// <summary>
        /// For setMotors
        /// </summary>
        /// <param name="command"></param>
        /// <param name="data1"></param>
        /// <param name="data2"></param>
        public ScribblerCommand(ScribblerHelper.Commands command, byte data1, byte data2)
        {
            Data = new byte[ScribblerHelper.CommandSize(command) - 1];
            CommandType = (byte)command;
            Data[0] = data1;
            Data[1] = data2;
            HasEcho = ScribblerHelper.HasEcho(command);
            ResponseLength = ScribblerHelper.ReturnSize(command);
        }

        /// <summary>
        /// For setName
        /// </summary>
        /// <param name="command"></param>
        /// <param name="data"></param>
        public ScribblerCommand(ScribblerHelper.Commands command, string data)
        {
            Data = new byte[ScribblerHelper.CommandSize(command) - 1];
            CommandType = (byte)command;
            int length = Math.Min(data.Length, 8);
            char[] tmp = data.ToCharArray(0, length);
            for (int i = 0; i < length; i++)
                Data[i] = (byte)tmp[i];
            for (int i = length; i < 8; i++)
                Data[i] = 0;
            HasEcho = ScribblerHelper.HasEcho(command);
            ResponseLength = ScribblerHelper.ReturnSize(command);
        }

        /// <summary>
        /// for speaker commands
        /// </summary>
        /// <param name="command"></param>
        /// <param name="duration"></param>
        /// <param name="frequency1"></param>
        /// <param name="frequency2"></param>
        public ScribblerCommand(ScribblerHelper.Commands command, int duration, int frequency1, int frequency2)
        {
            Data = new byte[ScribblerHelper.CommandSize(command) - 1];
            CommandType = (byte)command;

            Data[0] = (byte)(duration >> 8);
            Data[1] = (byte)duration;

            Data[2] = (byte)(frequency1 >> 8);
            Data[3] = (byte)frequency1;

            Data[4] = (byte)(frequency2 >> 8);
            Data[5] = (byte)frequency2;
            HasEcho = ScribblerHelper.HasEcho(command);
            ResponseLength = ScribblerHelper.ReturnSize(command);
        }

        /// <summary>
        /// For setAllLEDs command
        /// </summary>
        public ScribblerCommand(ScribblerHelper.Commands command, bool left, bool center, bool right)
        {
            Data = new byte[ScribblerHelper.CommandSize(command) - 1];
            CommandType = (byte)command;
            Data[0] = left ? (byte)1 : (byte)0;
            Data[1] = center ? (byte)1 : (byte)0;
            Data[2] = right ? (byte)1 : (byte)0;
            HasEcho = ScribblerHelper.HasEcho(command);
            ResponseLength = ScribblerHelper.ReturnSize(command);
        }

    }

    /// <summary>
    /// response from scribbler
    /// </summary>
    [DataContract()]
    public class ScribblerResponse : ScribblerCommand
    {
        public ScribblerResponse()
        { }

        public ScribblerResponse(int length)
        {
            base.Data = new byte[length];
        }

        public ScribblerResponse(byte[] indata)
        {
            base.CommandType = indata[indata.Length - 1];
            base.Data = new byte[indata.Length - 1];
            for (int i = 0; i < indata.Length - 1; i++)
                base.Data[i] = indata[i];
        }
    }


    /// <summary>
    /// motor command data
    /// </summary>
    [DataContract]
    [DataMemberConstructor]
    public class SetMotorsBody
    {
        /// <summary>
        /// 0 for full speed backwards, 100 = stopped, 200 = full speed forwards
        /// </summary>
        [DataMember]
        public int LeftSpeed;

        /// <summary>
        /// 0 for full speed backwards, 100 = stopped, 200 = full speed forwards
        /// </summary>
        [DataMember]
        public int RightSpeed;

        public SetMotorsBody() { }

        public SetMotorsBody(int leftSpeed, int rightSpeed)
        {
            LeftSpeed = leftSpeed;
            RightSpeed = rightSpeed;
        }
    }

    [DataContract]
    public class SetLedBody
    {
        /// <summary>
        /// which LED to set (0, 1, 2).  3 for all.
        /// <remarks>0 = left, 1 = center (note: center LED is used for message indication on scribbler), 2 = right</remarks>
        /// </summary>
        [DataMember]
        public int LED;

        /// <summary>
        /// State to set LED to
        /// </summary>
        [DataMember]
        public bool State;

        public SetLedBody() { }

        /// <summary>
        /// Parameterized constructor
        /// </summary>
        /// <param name="which">Which LED to set: 0 = left, 1 = center, 2 = right</param>
        /// <param name="state">State to set LED: true = on, false = off</param>
        public SetLedBody(int which, bool state)
        {
            LED = which;
            State = state;
        }
    }

    [DataContract]
    public class SetAllLedsBody
    {
        [DataMember]
        public bool LeftLED;

        [DataMember]
        public bool RightLED;

        [DataMember]
        public bool CenterLED;

        public SetAllLedsBody() { }

        public SetAllLedsBody(bool left, bool center, bool right)
        {
            LeftLED = left;
            CenterLED = center;
            RightLED = right;
        }
    }

    [DataContract]
    [DataMemberConstructor]
    public class SetLEDFrontBody
    {
        [DataMember]
        public bool FrontLED;
    }

    [DataContract]
    [DataMemberConstructor]
    public class SetLEDBackBody
    {
        [DataMember]
        public byte BackLED;
    }



    [DataContract]
    public class SetNameBody
    {
        [DataMember]
        public string NewName;

        public SetNameBody() { }

        public SetNameBody(string newname)
        {
            NewName = newname;
        }
    }


    [DataContract]
    public class PlayToneBody
    {
        /// <summary>
        /// units: ms
        /// </summary>
        [DataMember]
        public double Duration;

        /// <summary>
        /// units: Hz
        /// </summary>
        [DataMember]
        public int Frequency1;

        /// <summary>
        /// units: Hz
        /// </summary>
        [DataMember]
        public int Frequency2;

        public PlayToneBody()
        {
        }

        public PlayToneBody(double duration, int freq1, int freq2)
        {
            Duration = duration;
            Frequency1 = freq1;
            Frequency2 = freq2;
        }
    }

    [DataContract]
    [DataMemberConstructor]
    public class SetLoudBody
    {
        [DataMember]
        public bool IsLoud;
    }

}
