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
        private byte   _commandType;
        private byte[] _data;

        [DataMember]
        public byte CommandType
        {
            get { return _commandType; }
            set { _commandType = value; }
        }

        [DataMember]
        public byte[] Data
        {
            get { return _data; }
            set { _data = value; }
        }

        /// <summary>
        /// Standard constructor
        /// </summary>
        public ScribblerCommand()
        { }

        /// <summary>
        /// For "get" commands and simple "set" commands
        /// </summary>
        /// <param name="command"></param>
        public ScribblerCommand(byte command)
        {
            _data = new byte[8];
            _commandType = command;
        }

        /// <summary>
        /// For setMotors
        /// </summary>
        /// <param name="command"></param>
        /// <param name="data1"></param>
        /// <param name="data2"></param>
        public ScribblerCommand(byte command, byte data1, byte data2)
        {
            _data = new byte[8];
            _commandType = command;
            _data[0] = data1;
            _data[1] = data2;
        }

        /// <summary>
        /// For setName
        /// </summary>
        /// <param name="command"></param>
        /// <param name="data"></param>
        public ScribblerCommand(byte command, string data)
        {
            _data = new byte[8];
            _commandType = command;
            int length = Math.Min(data.Length, 8);
            char[] tmp = data.ToCharArray(0, length);
            for (int i = 0; i < length; i++)
                _data[i] = (byte)tmp[i];
            for (int i = length; i < 8; i++)
                _data[i] = 0;
        }

        /// <summary>
        /// for speaker commands
        /// </summary>
        /// <param name="command"></param>
        /// <param name="duration"></param>
        /// <param name="frequency1"></param>
        /// <param name="frequency2"></param>
        public ScribblerCommand(byte command, int duration, int frequency1, int frequency2)
        {
            _data = new byte[8];
            _commandType = command;

            _data[0] = (byte)(duration >> 8);
            _data[1] = (byte)duration;

            _data[2] = (byte)(frequency1 >> 8);
            _data[3] = (byte)frequency1;

            _data[4] = (byte)(frequency2 >> 8);
            _data[5] = (byte)frequency2;
        }

        /// <summary>
        /// For setAllLEDs command
        /// </summary>
        public ScribblerCommand(byte command, bool left, bool center, bool right)
        {
            _data = new byte[8];
            _commandType = command;
            _data[0] = left ? (byte)1 : (byte)0;
            _data[1] = center ? (byte)1 : (byte)0;
            _data[2] = right ? (byte)1 : (byte)0;
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
        public int Duration;

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

        public PlayToneBody(int duration, int freq1, int freq2)
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
