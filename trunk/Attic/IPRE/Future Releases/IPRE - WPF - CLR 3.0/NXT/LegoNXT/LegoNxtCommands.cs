//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNxtState.cs $ $Revision$
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;

using Microsoft.Robotics.Services.LegoNxt.Helper;
using System.IO.Ports;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using W3C.Soap;
using System.Diagnostics;
using System.ComponentModel;
using System.Text;

namespace Microsoft.Robotics.Services.LegoNxt
{

    #region Lego Command Base Types

    /// <summary>
    /// The base lego command format in which all commands inherit from
    /// </summary>
    [DataContract]
    public class LegoCommand
    {
        private bool _requireResponse;
        private byte[] _data;
        private LegoHelper.LegoCommandCode _commandCode;

        public readonly byte CommandType;

        [DataMember]
        public const int MaxCommandLength = 64;

        /// <summary>
        /// The Lego Command Code
        /// </summary>
        internal virtual LegoHelper.LegoCommandCode LegoCommandCode
        {
            get { return _commandCode; }
            set 
            {
                if (value != LegoHelper.LegoCommandCode.StartProgram || this.GetType() == typeof(LegoStartProgram))
                    _commandCode = value; 
            }
        }

        [DataMember]
        [Description ("Identifies whether to send an acknowledgement back on a command request.")]
        public virtual bool RequireResponse
        {
            get { return _requireResponse; }
            set
            {
                _requireResponse = value;
            }
        }

        /// <summary>
        /// Internal Data Structure
        /// </summary>
        internal virtual byte[] Data
        {
            get { return _data; }
            set { _data = value; }
        }

        public LegoCommand() { }

        public LegoCommand(int commandType, LegoHelper.LegoCommandCode command, int dataLength)
        {
            this.CommandType = (byte)commandType;
            this._commandCode = command;
            if (dataLength > 0)
                this._data = new byte[dataLength];
            else
                this._data = null;
        }

        public LegoCommand(int commandType, LegoHelper.LegoCommandCode command, int dataLength, byte[] data)
        {
            this.CommandType = (byte)commandType;
            this._commandCode = command;
            this.Data = data;

            if ((data != null) && (data.Length == dataLength))
                this.Data = data;
            else if (dataLength > 0)
            {
                this.Data = new byte[dataLength];
                if (data != null)
                {
                    for (int ix = 0; ix < dataLength && ix < data.Length; ix++)
                        this.Data[ix] = data[ix];
                }
            }
        }
    }

    /// <summary>
    /// The standard return package in which all return messages inherit from
    /// </summary>
    [DataContract]
    [Description("Lego command return value.")]
    public class LegoResponse : LegoCommand
    {
        public LegoResponse() { }
        public LegoResponse(int commandType, LegoHelper.LegoCommandCode command, int dataLength)
            : base(commandType, command, dataLength)
        {
        }

        public LegoResponse(int commandType, LegoHelper.LegoCommandCode command, int dataLength, byte[] data)
            : base(commandType, command, dataLength, data)
        {
        }

        [DataMember]
        [Description("Status of command request.")]
        public int Status
        {
            get
            {
                if (Data != null && Data.Length >= 1)
                    return Data[0];
                return 0;
            }
            set
            {
                if (Data == null || Data.Length < 1)
                    Data = new byte[1];
                Data[0] = (byte)value;
            }
        }

        [DataMember]
        [Description("The error code returned.")]
        public LegoErrorCode ErrorCode
        {
            get
            {
                if (Data != null && Data.Length >= 1)
                {
                    try
                    {
                        return (LegoErrorCode)Data[0];
                    }
                    catch
                    {
                        Debug.WriteLine("Unknown LEGO Return code: " + Data[0].ToString());
                        return LegoErrorCode.UnknownStatus;
                    }
                }
                return LegoErrorCode.UnknownStatus;
            }
            set
            {
                if (Data.Length >= 1)
                    Data[0] = (byte)value;
            }
        }
    }


    [DataContract]
    public class LegoResponseException : LegoResponse
    {
        public LegoResponseException()
            : base(0x02, 0, 0) { }

        public LegoResponseException(LegoCommand cmd, Exception ex)
            : base(0x02, cmd.LegoCommandCode, 0)
        {
            this.Data = LegoHelper.StringToData(ex.Message, ex.Message.Length + 1);
        }

        public LegoResponseException(LegoCommand cmd, string errorMessage)
            : base(0x02, cmd.LegoCommandCode, 0)
        {
            this.Data = LegoHelper.StringToData(errorMessage, errorMessage.Length + 1);
        }

        #region Hide base type DataMembers
        
        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion

        [DataMember]
        [Description("The error message text string.")]
        public string ErrorMessage
        {
            get
            {
                if (Data == null || Data.Length < 2)
                    return string.Empty;

                return LegoHelper.DataToString(Data, 0);
            }

            set
            {
                this.Data = LegoHelper.StringToData(value, value.Length + 1);
            }
        }

    }


    /// <summary>
    /// Sensor message coming from the sensor monitoring program on the NXT
    /// <remarks>Not in the standard Lego API</remarks>
    /// </summary>
    [DataContract]
    [Description("Sensor notification from NXT.")]
    public class SensorNotification : LegoCommand
    {
        public SensorNotification() { }
        public SensorNotification(int commandType, LegoHelper.LegoCommandCode command, int dataLength)
            : base(commandType, command, dataLength) { }

        [DataMember]
        [Description("The LEGO NXT mailbox where the message was deposited")]
        public int Mailbox
        {
            get
            {
                if (Data != null && Data.Length >= 6)
                    return Data[0] + 1;
                return -1;
            }
            set
            {
                if (Data != null && Data.Length > 0)
                    Data[0] = (byte)(value - 1);
            }
        }

        [DataMember]
        [Description("The message value in bytes.")]
        public int ValueBytes
        {
            get
            {
                if (Data != null && Data.Length >= 6)
                    return Data.Length - 5;
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The type of the message.")]
        public int Type
        {
            get
            {
                if (Data != null && Data.Length >= 6)
                    return LegoHelper.DataToInt(Data, 2, 1);
                return -1;
            }
            set
            {
                if (Data != null && Data.Length >= 6)
                {
                    // put the ASCII digit directly into the buffer.
                    Data[2] = (byte)(value.ToString()[0]);
                }
            }
        }

        [DataMember]
        [Description("The port the sensor is on.")]
        public int Port
        {
            get
            {
                if (Data != null && Data.Length >= 6)
                    return LegoHelper.DataToInt(Data, 3, 1);
                return -1;
            }
            set
            {
                if (Data != null && Data.Length >= 6)
                {
                    // put the ASCII digit directly into the buffer.
                    Data[3] = (byte)(value.ToString()[0]);
                }
            }
        }

        [DataMember]
        [Description("Sensor Value")]
        public int Value
        {
            get
            {
                if (Data != null && Data.Length >= 6)
                    return LegoHelper.DataToInt(Data, 4, ValueBytes);
                return -1;
            }
            set
            {
                string number = value.ToString();
                if (Data == null || Data.Length < (4+number.Length))
                {
                    byte[] oldData = Data;
                   Data = new byte[4 + number.Length];
                    if (oldData != null)
                        oldData.CopyTo(Data, 0);
                }
                int ix = 4;
                foreach (char c in number)
                {
                    // put the ASCII digit directly into the buffer.
                    Data[ix++] = (byte)c;
                }
            }
        }
    }

    #endregion

    #region Lego Enums

    [DataContract]
    [Description("The NXT output mode.")]
    public enum LegoOutputMode
    {
        /// <summary>
        /// Turn on the specified motor
        /// </summary>
        MotorOn = 0x01,

        /// <summary>
        /// Use run/brake instead of run/float in PWM
        /// </summary>
        Brake = 0x02,

        /// <summary>
        /// Turns on the regulation
        /// </summary>
        Regulated = 0x04,

        /// <summary>
        /// Power Control Regulation
        /// </summary>
        PowerControl = 0x07,
    }

    [DataContract]
    [Description("Motor regulation mode.")]
    public enum LegoRegulationMode
    {
        /// <summary>
        /// No regulation will be enabled
        /// </summary>
        Idle = 0x00,

        /// <summary>
        /// Power control will be enabled on specified output
        /// </summary>
        MotorSpeed = 0x01,

        /// <summary>
        /// Synchronization will be enabled (Needs enabled on two outputs)
        /// </summary>
        Regulated = 0x02,
    }


    [DataContract]
    [Description("The motor running state.")]
    public enum LegoRunState
    {
        Idle = 0x00,
        RampUp = 0x10,
        Running = 0x20,
        RampDown = 0x40,
    }

    [DataContract]
    [Description("The sensor type.")]
    public enum LegoSensorType
    {
        NoSensor = 0x00,
        Switch = 0x01,
        Temperature = 0x02,
        Reflection = 0x03,
        Angle = 0x04,
        LightActive = 0x05,
        LightInactive = 0x06,
        SoundDb = 0x07,
        SoundDba = 0x08,
        Custom = 0x09,
        LowSpeed = 0x0A,
        Sonar = 0x0B, //previously LowSpeed9V
        NumberOfSensorTypes = 0x0C,
    }


    [DataContract]
    [Description("The translation mode of the LEGO NXT sensor.")]
    public enum LegoSensorMode
    {
        RawMode = 0x00,
        BooleanMode = 0x20,
        TransitionCountMode = 0x40,
        PeriodCounterMode = 0x60,
        PercentFullScaleMode = 0x80,
        CelsiusMode = 0xA0,
        FahrenheitMode = 0xC0,
        AngleStepsMode = 0xE0,
        SlopeMask = 0x1F,
        ModeMask = 0xE0,
    }

    /// <summary>
    /// Error codes returned by the LEGO NXT Brick
    /// </summary>
    [DataContract]
    [Description("Error code return by the NXT brick.")]
    public enum LegoErrorCode
    {
        Success = 0x00,
        UnknownStatus = 0x01,
        Exception = 0x02,
        PendingCommunicationTransactionInProgress = 0x20,
        SpecifiedMailboxQueueIsEmpty = 0x40,
        NoMoreHandles = 0x81,
        NoSpace = 0x82,
        NoMoreFiles = 0x83,
        EndOfFileExpected = 0x84,
        EndOfFile = 0x85,
        NotALinearFile = 0x86,
        FileNotFound = 0x87,
        HandleAllreadyClosed = 0x88,
        NoLinearSpace = 0x89,
        UndefinedError = 0x8A,
        FileIsBusy = 0x8B,
        NoWriteBuffers = 0x8C,
        AppendNotPossible = 0x8D,
        FileIsFull = 0x8E,
        FileExists = 0x8F,
        ModuleNotFound = 0x90,
        OutOfBoundary = 0x91,
        IllegalFileName = 0x92,
        IllegalHandle = 0x93,
        RequestFailed_FileNotFound = 0xBD,
        UnknownCommandOpcode = 0xBE,
        InsanePacket = 0xBF,
        DataContains_OutOfRange_Values = 0xC0,
        CommunicationBusError = 0xDD,
        NoFreeMemoryInCommunicationBuffer = 0xDE,
        SpecifiedChannelOrConnectionIsNotValid = 0xDF,
        SpecifiedChannelOrConnectionIsNotConfiguredOrBusy = 0xE0,
        NoActiveProgram = 0xEC,
        IllegalSizeSpecified = 0xED,
        IllegalMailboxQueueIdSpecified = 0xEE,
        AttemptedToAccessInvalidFieldOfAStructure = 0xEF,
        BadInputOrOutputSpecified = 0xF0,
        InsufficientMemoryAvailable = 0xFB,
        BadArguments = 0xFF,
    }
    #endregion

    // NOTE: Not all Lego commands and return package types implemented
    #region Lego API Commands

    #region LegoBootCommand
    /// <summary>
    /// Only accepted by USB.
    /// <remarks>Return package not implemented.</remarks>
    /// </summary>
    [DataContract]
    public class LegoBootCommand : LegoCommand
    {
        public LegoBootCommand()
            : base(0x01, LegoHelper.LegoCommandCode.BootCommand, 19)
        {
            this.Data = LegoHelper.StringToData("Let's dance: SAMBA", 19);
        }


    }

    [DataContract]
    public class LegoResponseBootCommand : LegoResponse
    {

        public LegoResponseBootCommand()
            : base(0x02, LegoHelper.LegoCommandCode.BootCommand, 5) 
        { 
            base.RequireResponse = true; 
        }

        public LegoResponseBootCommand(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.BootCommand, 5, cmd.Data)
        {
            base.RequireResponse = true;
        }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion

        [DataMember]
        [Description("LEGO NXT Boot Acknowledgement")]
        public string Message
        {
            get
            {
                if (Data.Length < 2)
                    return string.Empty;

                return LegoHelper.DataToString(Data, 1);
            }
            set
            {
                if (Data == null || Data.Length < (value.Length + 1))
                {
                    byte[] oldData = Data;
                    Data = new byte[value.Length + 1];
                    if (oldData != null) oldData.CopyTo(Data, 0);
                }
                LegoHelper.StringToData(value, value.Length + 1).CopyTo(Data, 1);
            }
        }

    }

    /// <summary>
    /// Request/Response definition for sending a LegoBootCommand
    /// </summary>
    public class SendLegoBootCommand : Submit<LegoBootCommand, PortSet<LegoResponseBootCommand, Fault>>
    {
    }

    #endregion

    #region LegoClose
    [DataContract]
    public class LegoClose : LegoCommand
    {
        private byte _handle;

        public LegoClose()
            : base(0x01, LegoHelper.LegoCommandCode.Close, 0) { base.RequireResponse = true; }

        public LegoClose(int handle)
            : base(0x01, LegoHelper.LegoCommandCode.Close, 1)
        {
            base.RequireResponse = true; 
            this.Handle = handle;
        }

        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get { return (int)_handle; }
            set
            {
                _handle = (byte)value;
                if (this.Data == null) this.Data = new byte[1];
                this.Data[0] = _handle;
            }
        }
    }

    [DataContract]
    public class LegoResponseClose : LegoResponse
    {
        public LegoResponseClose()
            : base(0x02, LegoHelper.LegoCommandCode.Close, 2) { }

        public LegoResponseClose(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.Close, 2, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                if (Data != null && Data.Length >= 2)
                {
                    Data[1] = (byte)value;
                }
            }
        }

    }
    #endregion

    #region LegoDelete
    [DataContract]
    public class LegoDelete : LegoCommand
    {
        private string _fileName = string.Empty;

        public LegoDelete()
            : base(0x01, LegoHelper.LegoCommandCode.Delete, 0) { base.RequireResponse = true; }

        public LegoDelete(string fileName)
            : base(0x01, LegoHelper.LegoCommandCode.Delete, 0)
        {
            base.RequireResponse = true;
            this.FileName = fileName;
        }


        [DataMember]
        [Description("The name of the file to be deleted.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                this.Data = LegoHelper.StringToData(_fileName, 20);
            }
        }

    }

    [DataContract]
    public class LegoResponseDelete : LegoResponse
    {
        public LegoResponseDelete()
            : base(0x02, LegoHelper.LegoCommandCode.Delete, 21) { }

        public LegoResponseDelete(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.Delete, 21, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The name of the file.")]
        public string FileName
        {
            get
            {
                if (Data.Length < 2)
                    return string.Empty;

                return LegoHelper.DataToString(Data, 1, Data.Length - 1);
            }
            set
            {
                if (Data == null || Data.Length < (value.Length + 1))
                {
                    byte[] oldData = Data;
                    Data = new byte[value.Length + 1];
                    if (oldData != null) oldData.CopyTo(Data, 0);
                }
                LegoHelper.StringToData(value, value.Length + 1).CopyTo(Data, 1);
            }
        }
    }

    /// <summary>
    /// Request/Response definition for sending a LegoDelete
    /// </summary>
    public class SendLegoDelete : Submit<LegoDelete, PortSet<LegoResponseDelete, Fault>>
    {
    }
    #endregion

    #region LegoStartProgram
    /// <summary>
    /// Starts a program on the NXT.  
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoStartProgram : LegoCommand
    {
        private string _fileName = string.Empty;

        public LegoStartProgram()
            : base(0x00, LegoHelper.LegoCommandCode.StartProgram, 20)
        {
        }

        public LegoStartProgram(string fileName)
            : base(0x00, LegoHelper.LegoCommandCode.StartProgram, 0)
        {
            this.FileName = fileName;
        }


        [DataMember]
        [Description("The name of the file to be started.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                this.Data = LegoHelper.StringToData(_fileName, 20);
            }
        }
    }
    /// <summary>
    /// Request/Response definition for sending a LegoStartProgram
    /// </summary>
    [Description("Sends the LEGO StartProgram command to the NXT brick.\nStarts a program stored on the NXT.")]
    public class SendLegoStartProgram : Submit<LegoStartProgram, PortSet<LegoResponse, Fault>>
    {
    }
    #endregion

    #region LegoPlaySoundFile
    /// <summary>
    /// Play a sound file
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoPlaySoundFile : LegoCommand
    {
        private string _fileName = string.Empty;
        private bool _loop = false;

        public LegoPlaySoundFile()
            : base(0x00, LegoHelper.LegoCommandCode.PlaySoundFile, 21)
        {
        }

        public LegoPlaySoundFile(string fileName, bool loop)
            : base(0x00, LegoHelper.LegoCommandCode.PlaySoundFile, 21)
        {
            this.FileName = fileName;
        }


        [DataMember]
        [Description("The name of the sound file to be played.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                LegoHelper.SetStringToData(this.Data, 1, _fileName, 20);
            }
        }

        [DataMember]
        [Description("Repeat the sound file")]
        public bool Loop
        {
            get { return _loop; }
            set
            {
                _loop = value;
                this.Data[0] = (byte)((value) ? 1 : 0);
            }
        }
    }


    /// <summary>
    /// Request/Response definition for sending a LegoPlaySoundFile
    /// </summary>
    [Description("Sends the LEGO PlaySoundFile command to the NXT brick.\nPlays a sound file that is stored on the NXT.")]
    public class SendLegoPlaySoundFile : Submit<LegoPlaySoundFile, PortSet<LegoResponse, Fault>>
    {
    }

    #endregion

    #region LegoPlayTone

    /// <summary>
    /// Plays a tone on the NXT.   
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoPlayTone : LegoCommand
    {
        private ushort _frequency;
        private ushort _duration;

        public LegoPlayTone()
            : base(0x00, LegoHelper.LegoCommandCode.PlayTone, 4) { }

        public LegoPlayTone(int frequency, int duration)
            : base(0x00, LegoHelper.LegoCommandCode.PlayTone, 4)
        {
            this.Frequency = frequency;
            this.Duration = duration;
        }


        /// <summary>
        /// 200 - 14000 Hz
        /// </summary>
        [DataMember]
        [DataMemberConstructor(Order = 1)]
        [Description("The frequency of the note.")]
        public int Frequency
        {
            get { return (int)_frequency; }
            set
            {
                _frequency = (ushort)value;
                if (this.Data == null) this.Data = new byte[4];
                LegoHelper.SetUShort(this.Data, 0, _frequency);
            }
        }

        /// <summary>
        /// Duration to play tome in ms
        /// </summary>
        [DataMember]
        [Description("The duration to play the note (in ms).")]
        [DataMemberConstructor(Order = 2)]
        public int Duration
        {
            get { return (int)_duration; }
            set
            {
                _duration = (ushort)value;
                if (this.Data == null) this.Data = new byte[4];
                LegoHelper.SetUShort(this.Data, 2, _duration);
            }
        }
    }

    /// <summary>
    /// Request/Response definition for sending a LegoPlayTone
    /// </summary>
    [Description("Sends the LEGO PlayTone command to the NXT brick.\nPlays a tone on the NXT.")]
    public class SendLegoPlayTone : Submit<LegoPlayTone, PortSet<LegoResponse, Fault>>
    {
    }
    #endregion

    #region LegoStopSoundPlayback

    /// <summary>
    /// Stop sound playback on the NXT.   
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoStopSoundPlayback : LegoCommand
    {
        public LegoStopSoundPlayback()
            : base(0x00, LegoHelper.LegoCommandCode.StopSoundPlayback, 0) { }

    }

    /// <summary>
    /// Request/Response definition for sending a LegoStopSoundPlayback
    /// </summary>
    [Description("Sends the LEGO StopSoundPlayback command to the NXT brick.\nStops a sound playing on the NXT.")]
    public class SendLegoStopSoundPlayback : Submit<LegoStopSoundPlayback, PortSet<LegoResponse, Fault>>
    {
    }
    #endregion

    #region LegoStopProgram

    /// <summary>
    /// Stop sound playback on the NXT.   
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoStopProgram : LegoCommand
    {
        public LegoStopProgram()
            : base(0x00, LegoHelper.LegoCommandCode.StopProgram, 0) 
        {
            // Default return a status
            base.RequireResponse = true;
        }

    }

    /// <summary>
    /// Request/Response definition for sending a LegoStopProgram
    /// </summary>
    [Description("Sends the LEGO StopProgram command to the NXT brick.\nStops a program running on the NXT.")]
    public class SendLegoStopProgram : Submit<LegoStopProgram, PortSet<LegoResponse, Fault>>
    {
    }
    #endregion

    #region LegoFindFirst
    [DataContract]
    public class LegoFindFirst : LegoCommand
    {
        private string _fileName = string.Empty;

        public LegoFindFirst()
            : base(0x01, LegoHelper.LegoCommandCode.FindFirst, 0)
        {
            base.RequireResponse = true;
        }

        public LegoFindFirst(string fileName)
            : base(0x01, LegoHelper.LegoCommandCode.FindFirst, 0)
        {
            base.RequireResponse = true;
            this.FileName = fileName;
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        [DataMember]
        [Description("The name of the file.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                this.Data = LegoHelper.StringToData(_fileName, 20);
            }
        }

    }

    [DataContract]
    public class LegoResponseFindFirst : LegoResponse
    {
        public LegoResponseFindFirst()
            : base(0x02, LegoHelper.LegoCommandCode.FindFirst, 26)
        {
        }

        public LegoResponseFindFirst(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.FindFirst, 26, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion

        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                if (Data != null && Data.Length >= 2)
                    Data[1] = (byte)value;
            }
            
        }

        [DataMember]
        [Description("The name of the file.")]
        public string FileName
        {
            get
            {
                if (Data.Length < 3)
                    return string.Empty;

                return LegoHelper.DataToString(Data, 2, Data.Length - 6);
            }
            set
            {
                string newFilename = value;
                if (value.Length > 19)
                    newFilename = value.Substring(0, 19);

                if (Data == null || Data.Length < (value.Length + 2))
                {
                    byte[] oldData = Data;
                    Data = new byte[value.Length + 2];
                    if (oldData != null) oldData.CopyTo(Data, 0);
                }
                LegoHelper.StringToData(value, value.Length + 2).CopyTo(Data, 2);
            }
        }

        [DataMember]
        [Description("The size of the file.")]
        public long FileSize
        {
            get
            {
                if (Data.Length >= 26)
                    return (long)BitConverter.ToUInt32(Data, 22);
                return -1;
            }
            set
            {
                if (Data.Length >= 26)
                    LegoHelper.SetUInt32(Data, 22, value);
            }
        }

    }
    #endregion

    #region LegoGetCurrentProgramName
    [DataContract]
    public class LegoGetCurrentProgramName : LegoCommand
    {
        public LegoGetCurrentProgramName()
            : base(0x00, LegoHelper. LegoCommandCode.GetCurrentProgramName, 0)
        {
            base.RequireResponse = true;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }
    }

    [DataContract]
    public class LegoResponseGetCurrentProgramName : LegoResponse
    {
        public LegoResponseGetCurrentProgramName()
            : base(0x02, LegoHelper.LegoCommandCode.GetCurrentProgramName, 22) { }

        public LegoResponseGetCurrentProgramName(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.GetCurrentProgramName, 22, cmd.Data) { }


        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The name of the file.")]
        public string FileName
        {
            get
            {
                if (Data == null || Data.Length < 2)
                    return string.Empty;

                return LegoHelper.DataToString(Data, 1);
            }
            set
            {
                if (Data == null || Data.Length < (value.Length + 1))
                {
                    byte[] oldData = Data;
                    Data = new byte[value.Length + 1];
                    if (oldData != null) oldData.CopyTo(Data, 0);
                }
                LegoHelper.StringToData(value, value.Length + 1).CopyTo(Data, 1);
            }
        }

    }

    /// <summary>
    /// Request/Response definition for sending a LegoGetCurrentProgramName
    /// </summary>
    [Description("Sends the LEGO GetCurrentProgramName to the NXT brick.\nGets the NXT's currently running program name.")]
    public class SendLegoGetCurrentProgramName : Submit<LegoGetCurrentProgramName, PortSet<LegoResponseGetCurrentProgramName, Fault>>
    {
    }

    #endregion

    #region LegoGetDeviceInfo

    [DataContract]
    public class LegoGetDeviceInfo : LegoCommand
    {
        public LegoGetDeviceInfo()
            : base(0x01, LegoHelper.LegoCommandCode.GetDeviceInfo, 0)
        {
            base.RequireResponse = true;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }
    }

    [DataContract]
    public class LegoResponseGetDeviceInfo : LegoResponse
    {
        public LegoResponseGetDeviceInfo()
            : base(0x02, LegoHelper.LegoCommandCode.GetDeviceInfo, 31) { }

        public LegoResponseGetDeviceInfo(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.GetDeviceInfo, 31, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The descriptive name of the NXT brick.")]
        public string BrickName
        {
            get
            {
                if (Data == null || Data.Length < 31)
                    return string.Empty;

                return LegoHelper.DataToString(Data, 1, 15);
            }
            set
            {

                string newValue = value ?? string.Empty;
                if (newValue.Length > 14)
                    newValue = newValue.Substring(0, 14);

                if (Data == null || Data.Length < 31)
                {
                    byte[] oldData = Data;
                    Data = new byte[31];
                    if (oldData != null) oldData.CopyTo(Data, 0);
                }
                LegoHelper.StringToData(newValue, newValue.Length + 1).CopyTo(Data, 1);
            }
        }

        [DataMember]
        [Description("The BlueTooth address.")]
        public string BluetoothAddress
        {
            get
            {
                if (Data == null || Data.Length < 31)
                    return string.Empty;

                StringBuilder sb = new StringBuilder();
                for (int ix = 16; ix < 23; ix++)
                    sb.Append(Data[ix].ToString() + ".");
                sb.Length--;
                return sb.ToString();

            }
            set
            {
                string[] values = value.Split('.');
                if (values.Length != 7)
                    Debug.WriteLine("Bluetooth address is not valid.");

                int ix = 16;
                foreach(string number in values)
                {
                    byte v;
                    if (byte.TryParse(number, out v))
                    {
                        Data[ix] = v;
                    }
                    ix++;
                }
            }
        }

        [DataMember]
        [Description("The BlueTooth signal strength.")]
        public long BluetoothSignalStrength
        {
            get
            {
                if (Data.Length >= 31)
                    return (long)BitConverter.ToUInt32(Data, 23);
                return -1;
            }
            set
            {
                if (Data.Length >= 31)
                    LegoHelper.SetUInt32(Data, 23, value);
            }
        }

        [DataMember]
        [Description("The amount of memory available.")]
        public long FreeMemory
        {
            get
            {
                if (Data.Length >= 31)
                    return (long)BitConverter.ToUInt32(Data, 27);
                return -1;
            }
            set
            {
                if (Data.Length >= 31)
                    LegoHelper.SetUInt32(Data, 27, value);
            }
        }

    }

    /// <summary>
    /// Request/Response definition for sending a LegoGetDeviceInfo
    /// </summary>
    [Description("Sends the LEGO GetDeviceInfo command to the NXT brick.")]
    public class SendLegoGetDeviceInfo : Submit<LegoGetDeviceInfo, PortSet<LegoResponseGetDeviceInfo, Fault>>
    {
    }

    #endregion


    #region LegoGetFirmwareVersion

    [DataContract]
    public class LegoGetFirmwareVersion : LegoCommand
    {
        public LegoGetFirmwareVersion()
            : base(0x01, LegoHelper.LegoCommandCode.GetFirmwareVersion, 0)
        {
            base.RequireResponse = true;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }
    }

    [DataContract]
    public class LegoResponseGetFirmwareVersion : LegoResponse
    {
        public LegoResponseGetFirmwareVersion()
            : base(0x02, LegoHelper.LegoCommandCode.GetFirmwareVersion, 5)
        {
        }

        public LegoResponseGetFirmwareVersion(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.GetFirmwareVersion, 5, cmd.Data)
        {
        }

        #region Hide base type DataMembers

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The minor protocol version number.")]
        public int MinorProtocolVersion
        {
            get
            {
                if (Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                if (Data.Length >= 2)
                    Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The major protocol version number.")]
        public int MajorProtocolVersion
        {
            get
            {
                if (Data.Length >= 3)
                    return Data[2];
                return -1;
            }
            set
            {
                if (Data.Length >= 3)
                    Data[2] = (byte)value;
            }
        }

        [DataMember]
        [Description("The minor firmware version number.")]
        public int MinorFirmwareVersion
        {
            get
            {
                if (Data.Length >= 4)
                    return Data[3];
                return -1;
            }
            set
            {
                if (Data.Length >= 4)
                    Data[3] = (byte)value;
            }
        }

        [DataMember]
        [Description("The major firmware version number.")]
        public int MajorFirmwareVersion
        {
            get
            {
                if (Data.Length >= 5)
                    return Data[4];
                return -1;
            }
            set
            {
                if (Data.Length >= 5)
                    Data[4] = (byte)value;
            }
        }
    }

    /// <summary>
    /// Request/Response definition for sending a LegoGetFirmwareVersion
    /// </summary>
    public class SendLegoGetFirmwareVersion : Submit<LegoGetFirmwareVersion, PortSet<LegoResponseGetFirmwareVersion, Fault>>
    {
    }

    #endregion

    #region LegoOpenWrite
    [DataContract]
    public class LegoOpenWrite : LegoCommand
    {
        private string _fileName;
        private UInt32 _fileSize;

        public LegoOpenWrite()
            : base(0x01, LegoHelper.LegoCommandCode.OpenWrite, 0)
        {
            base.RequireResponse = true;
        }

        public LegoOpenWrite(string file, int size)
            : base(0x01, LegoHelper.LegoCommandCode.OpenWrite, 24)
        {
            base.RequireResponse = true;
            this.FileName = file;
            this.FileSize = size;
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        [DataMember]
        [Description("The name of the file to be opened for writing.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                if (this.Data == null) this.Data = new byte[24];
                LegoHelper.SetStringToData(this.Data, 0, _fileName, 20);
            }
        }

        [DataMember]
        [Description("The size of the file.")]
        public int FileSize
        {
            get { return (int)_fileSize; }
            set
            {
                _fileSize = (UInt32)value;
                if (this.Data == null) this.Data = new byte[24];
                LegoHelper.SetUInt32(this.Data, 20, _fileSize);
            }
        }


    }

    [DataContract]
    public class LegoResponseOpenWrite : LegoResponse
    {
        public LegoResponseOpenWrite()
            : base(0x02, LegoHelper.LegoCommandCode.OpenWrite, 2) { }

        public LegoResponseOpenWrite(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.OpenWrite, 2, cmd.Data) { }


        #region Hide base type DataMembers

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

    }
    #endregion

    #region LegoOpenWriteLinear
    [DataContract]
    public class LegoOpenWriteLinear : LegoCommand
    {
        private string _fileName;
        private UInt32 _fileSize;

        public LegoOpenWriteLinear()
            : base(0x01, LegoHelper. LegoCommandCode.OpenWriteLinear, 0)
        {
            base.RequireResponse = true;
        }

        public LegoOpenWriteLinear(string file, int size)
            : base(0x01, LegoHelper.LegoCommandCode.OpenWriteLinear, 24)
        {
            base.RequireResponse = true;
            this.FileName = file;
            this.FileSize = size;
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        [DataMember]
        [Description("The name of the file.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                if (this.Data == null) this.Data = new byte[24];
                LegoHelper.SetStringToData(this.Data, 0, _fileName, 20);
            }
        }

        [DataMember]
        [Description("The size of the file.")]
        public int FileSize
        {
            get { return (int)_fileSize; }
            set
            {
                _fileSize = (UInt32)value;
                if (this.Data == null) this.Data = new byte[24];
                LegoHelper.SetUInt32(this.Data, 20, _fileSize);
            }
        }


    }

    [DataContract]
    public class LegoResponseOpenWriteLinear : LegoResponse
    {
        public LegoResponseOpenWriteLinear()
            : base(0x02, LegoHelper.LegoCommandCode.OpenWriteLinear, 2) { }

        public LegoResponseOpenWriteLinear(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.OpenWriteLinear, 2, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }

        }

    }
    #endregion

    #region LegoOpenWriteData
    [DataContract]
    public class LegoOpenWriteData : LegoCommand
    {
        private string _fileName;
        private UInt32 _fileSize;

        public LegoOpenWriteData()
            : base(0x01, LegoHelper.LegoCommandCode.OpenWriteData, 0)
        {
            base.RequireResponse = true;
        }

        public LegoOpenWriteData(string file, int size)
            : base(0x01, LegoHelper.LegoCommandCode.OpenWriteData, 23)
        {
            base.RequireResponse = true;
            this.FileName = file;
            this.FileSize = size;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        [DataMember]
        [Description("The name of the file opened for writing.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                if (this.Data == null) this.Data = new byte[23];
                LegoHelper.SetStringToData(this.Data, 0, _fileName, 20);
            }
        }

        [DataMember]
        [Description("The size of the file.")]
        public int FileSize
        {
            get { return (int)_fileSize; }
            set
            {
                _fileSize = (UInt32)value;
                if (this.Data == null) this.Data = new byte[23];
                LegoHelper.SetUInt32(this.Data, 20, _fileSize);
            }
        }


    }

    [DataContract]
    public class LegoResponseOpenWriteData : LegoResponse
    {
        public LegoResponseOpenWriteData()
            : base(0x02, LegoHelper.LegoCommandCode.OpenWriteData, 2) { }

        public LegoResponseOpenWriteData(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.OpenWriteData, 2, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

    }
    #endregion

    #region LegoWrite
    [DataContract]
    public class LegoWrite : LegoCommand
    {
        private byte _handle;
        private byte[] _writeData;

        public LegoWrite()
            : base(0x01, LegoHelper.LegoCommandCode.Write, 0)
        {
            base.RequireResponse = true;
        }

        public LegoWrite(int handle, byte[] writeData)
            : base(0x01, LegoHelper.LegoCommandCode.Write, 0)
        {
            base.RequireResponse = true;
            this.Handle = handle;
            this.WriteData = writeData;
        }


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get { return (int)_handle; }
            set
            {
                _handle = (byte)value;
                if (this.Data == null) this.Data = new byte[1];
                this.Data[0] = _handle;
            }
        }

        [DataMember]
        [Description("The data to be written.")]
        public byte[] WriteData
        {
            get { return _writeData; }
            set
            {
                _writeData = value;
                if (this.Data == null) this.Data = new byte[_writeData.Length + 1];
                this.Data = LegoHelper.AppendData(this.Data, 1, _writeData);
            }
        }


    }

    [DataContract]
    public class LegoResponseWrite : LegoResponse
    {
        public LegoResponseWrite()
            : base(0x02, LegoHelper.LegoCommandCode.Write, 4) { }

        public LegoResponseWrite(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.Write, 4, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The number of bytes written.")]
        public int BytesWritten
        {
            get
            {
                if (Data != null && Data.Length >= 4)
                    return (int)BitConverter.ToUInt16(Data, 2);
                return -1;
            }
            set { LegoHelper.SetUShort(Data, 2, value); }
        }

    }
    #endregion

    #region LegoOpenRead
    [DataContract]
    public class LegoOpenRead : LegoCommand
    {
        private string _fileName = string.Empty;

        public LegoOpenRead()
            : base(0x01, LegoHelper.LegoCommandCode.OpenRead, 0)
        {
            base.RequireResponse = true;
        }

        public LegoOpenRead(string fileName)
            : base(0x01, LegoHelper.LegoCommandCode.OpenRead, 0)
        {
            base.RequireResponse = true;
            this.FileName = fileName;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        [DataMember]
        [Description("The name of the file opened for reading.")]
        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                this.Data = LegoHelper.StringToData(_fileName, 20);
            }
        }

    }

    [DataContract]
    public class LegoResponseOpenRead : LegoResponse
    {
        public LegoResponseOpenRead()
            : base(0x02, LegoHelper.LegoCommandCode.OpenRead, 6) { }

        public LegoResponseOpenRead(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.OpenRead, 6, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The size of the file.")]
        public long FileSize
        {
            get
            {
                if (Data.Length >= 6)
                    return (long)BitConverter.ToUInt32(Data, 2);
                return -1;
            }
            set
            {
                LegoHelper.SetUInt32(Data, 2, value);
            }
        }

    }
    #endregion

    #region LegoRead
    [DataContract]
    public class LegoRead : LegoCommand
    {
        private byte _handle;
        private int _bytesToRead;

        public LegoRead()
            : base(0x01, LegoHelper.LegoCommandCode.Read, 0)
        {
            base.RequireResponse = true;
        }

        public LegoRead(int handle, int bytesToRead)
            : base(0x01, LegoHelper.LegoCommandCode.Read, 3)
        {
            base.RequireResponse = true;
            this.Handle = handle;
            this.BytesToRead = bytesToRead;
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get { return (int)_handle; }
            set
            {
                _handle = (byte)value;
                if (this.Data == null) this.Data = new byte[3];
                this.Data[0] = _handle;
            }
        }

        [DataMember]
        [Description("The number of bytes to read.")]
        public int BytesToRead
        {
            get { return (int)_bytesToRead; }
            set
            {
                _bytesToRead = value;
                if (this.Data == null) this.Data = new byte[3];
                LegoHelper.SetUShort(this.Data, 1, _bytesToRead);
            }
        }
    }

    [DataContract]
    public class LegoResponseRead : LegoResponse
    {
        public LegoResponseRead()
            : base(0x02, LegoHelper.LegoCommandCode.Read, 0) { }

        public LegoResponseRead(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.Read, 0)
        {
            this.Data = cmd.Data;
        }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The handle to the data.")]
        public int Handle
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The number of bytes read.")]
        public int BytesRead
        {
            get
            {
                if (Data != null && Data.Length >= 4)
                    return (int)BitConverter.ToUInt16(Data, 2);
                return -1;
            }
            set { LegoHelper.SetUShort(Data, 2, value); }
        }

        [DataMember]
        [Description("The data read.")]
        public byte[] ReadData
        {
            get
            {
                if (Data != null && Data.Length >= 5)
                {
                    byte[] r = new byte[Data.Length - 4];
                    int ix = 4;
                    int iy = 0;
                    while (ix < Data.Length && iy < r.Length)
                        r[iy++] = this.Data[ix++];
                    return r;
                }
                return null;
            }
            set
            {
                int ix = 4;
                int iy = 0;
                while (ix < Data.Length && iy < value.Length)
                    Data[ix++] = value[iy++];
            }
        }

    }

    /// <summary>
    /// Request/Response definition for sending a LegoRead
    /// </summary>
    public class SendLegoRead : Submit<LegoRead, PortSet<LegoResponseRead, Fault>>
    {
    }

    #endregion

    #region LegoMessageRead

    [DataContract]
    public class LegoMessageRead : LegoCommand
    {
        private int _remoteInbox;
        private int _localInbox;
        private bool _remove;

        public LegoMessageRead()
            : base(0x00, LegoHelper.LegoCommandCode.MessageRead, 0)
        {
            base.RequireResponse = true;
        }

        public LegoMessageRead(int remoteInbox, int localInbox, bool remove)
            : base(0x00, LegoHelper.LegoCommandCode.MessageRead, 3)
        {
            base.RequireResponse = true;
            this.RemoteInbox = remoteInbox;
            this.LocalInbox = localInbox;
            this.Remove = remove;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        /// <summary>
        /// Remove Inbox 0-9
        /// </summary>
        [DataMember]
        [Description("The remote communication port (0-9).")]
        public int RemoteInbox
        {
            get { return (int)_remoteInbox; }
            set
            {
                _remoteInbox = value;
                if (this.Data == null) this.Data = new byte[3];
                this.Data[0] = (byte)_remoteInbox;
            }
        }

        /// <summary>
        /// Local Inbox 0-9
        /// </summary>
        [DataMember]
        [Description("The local communication port (0-9).")]
        public int LocalInbox
        {
            get { return (int)_localInbox; }
            set
            {
                _localInbox = value;
                if (this.Data == null) this.Data = new byte[3];
                this.Data[1] = (byte)_localInbox;
            }
        }

        /// <summary>
        /// Clear message from remote inbox
        /// </summary>
        [DataMember]
        [Description("Identifies whether the message has been removed.")]
        public bool Remove
        {
            get { return _remove; }
            set
            {
                _remove = value;
                if (this.Data == null) this.Data = new byte[3];
                this.Data[2] = (byte)((_remove) ? 1 : 0);
            }
        }
    }

    [DataContract]
    public class LegoResponseMessageRead : LegoResponse
    {
        public LegoResponseMessageRead()
            : base(0x02, LegoHelper.LegoCommandCode.MessageRead, 62) { }

        public LegoResponseMessageRead(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.MessageRead, 62, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The local communication port.")]
        public int LocalInbox
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return (int)this.Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The size of the message.")]
        public int MessageSize
        {
            get
            {
                if (Data != null && Data.Length >= 3)
                    return (int)this.Data[2];
                return -1;
            }
            set
            {
                Data[2] = (byte)value;
            }
        }

        [DataMember]
        [Description("The message data read.")]
        public byte[] MessageReadData
        {
            get
            {
                if (Data != null && Data.Length >= 4)
                {
                    byte[] r = new byte[59];
                    this.Data.CopyTo(r, 3);
                    return r;
                }
                return null;
            }
            set
            {
                int ix = 3;
                int iy = 0;
                while (ix < Data.Length && iy < value.Length)
                    Data[ix++] = value[iy++];
            }
        }

    }


    #endregion


    #region LegoMessageWrite

    /// <summary>
    /// Send a message to the NXT that the NXT can read with a message block
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoMessageWrite : LegoCommand
    {
        private byte _inbox;
        private byte _size;
        private byte[] _messageData;

        public LegoMessageWrite()
            : base(0x00, LegoHelper.LegoCommandCode.MessageWrite, 0) { }

        public LegoMessageWrite(int inbox, byte[] messageData)
            : base(0x00, LegoHelper.LegoCommandCode.MessageWrite, 0)
        {
            this.Inbox = inbox;
            this.MessageSize = messageData.Length;
            this.MessageData = messageData;
        }

        public LegoMessageWrite(int inbox, string message)
            : base(0x00, LegoHelper.LegoCommandCode.MessageWrite, 0)
        {
            this.Inbox = inbox;
            this.MessageDataString = message;
            this.MessageSize = _messageData.Length;
        }


        [DataMember]
        [DataMemberConstructor(Order = 1)]
        [Description("LEGO NXT Inbox where message should be delivered")]
        public int Inbox
        {
            get { return (int)_inbox; }
            set
            {
                _inbox = (byte)value;
                if (this.Data == null) this.Data = new byte[2];
                this.Data[0] = _inbox;
            }
        }

        [DataMember]
        [Description("The size of the message to be written (0-60).")]
        public int MessageSize
        {
            get { return (int)_size; }
            set
            {
                if (value < 0 || value > 60)
                    throw new ArgumentOutOfRangeException("MessageSize must be a positive number no larger than 60 bytes");

                _size = (byte)value;
                if (this.Data == null)
                    this.Data = new byte[2 + value];
                this.Data[1] = _size;
            }
        }

        public byte[] MessageData
        {
            get { return _messageData; }
            set
            {
                int length = (value == null) ? 0 : value.Length;

                if (length > 60)
                    throw new ArgumentOutOfRangeException("MessageData must be no larger than 60 bytes");

                _messageData = value;
                if (this.Data == null || this.Data.Length != (_messageData.Length + 2))
                {
                    this.Data = new byte[_messageData.Length + 2];
                }
                MessageSize = length;
                this.Data = LegoHelper.AppendData(this.Data, 2, _messageData);
            }
        }

        /// <summary>
        /// Expose the message data as a string.
        /// </summary>
        [DataMember]
        [Description("The message data.")]
        [DataMemberConstructor(Order = 2)]
        public string MessageDataString
        {
            get
            {
                if (_messageData == null || _messageData.Length < 2)
                    return string.Empty;

                return LegoHelper.DataToString(_messageData, 0);
            }

            set
            {
                MessageData = LegoHelper.StringToData(value, value.Length + 1);
            }
        }


    }

    #endregion

    #region LegoKeepAlive
    [DataContract]
    public class LegoKeepAlive : LegoCommand
    {
        public LegoKeepAlive()
            : base(0x00, LegoHelper.LegoCommandCode.KeepAlive, 0)
        {
            base.RequireResponse = true;
        }


    }

    [DataContract]
    public class LegoResponseKeepAlive : LegoResponse
    {
        public LegoResponseKeepAlive()
            : base(0x02, LegoHelper.LegoCommandCode.KeepAlive, 5)
        {
        }

        public LegoResponseKeepAlive(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.KeepAlive, 5, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The number of milliseconds between KeepAlive messages")]
        public long SleepTimeMilliseconds
        {
            get
            {
                if (Data.Length >= 5)
                    return (long)BitConverter.ToUInt32(Data, 1);
                return -1;
            }
            set
            {
                if (Data.Length >= 5)
                    LegoHelper.SetUInt32(Data, 1, value);
            }
        }

    }


    #endregion

    #region LegoGetBatteryLevel
    [DataContract]
    public class LegoGetBatteryLevel : LegoCommand
    {
        public LegoGetBatteryLevel()
            : base(0x00, LegoHelper.LegoCommandCode.GetBatteryLevel, 0)
        {
            base.RequireResponse = true;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

    }


    [DataContract]
    public class LegoResponseGetBatteryLevel : LegoResponse
    {
        public LegoResponseGetBatteryLevel()
            : base(0x02, LegoHelper.LegoCommandCode.GetBatteryLevel, 3)
        {
        }

        public LegoResponseGetBatteryLevel(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.GetBatteryLevel, 3, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        /// <summary>
        /// Voltage in millivolts
        /// </summary>
        [DataMember]
        [Description("Indicates the voltage (in millivolts).")]
        public int Voltage
        {
            get { return (int)BitConverter.ToUInt16(Data, 1); }
            set { LegoHelper.SetUShort(Data, 1, value); }
        }
    }


    /// <summary>
    /// Request/Response definition for sending a LegoGetBatteryLevel
    /// </summary>
    [Description("Sends the LEGO GetBatteryLevel command to the NXT brick.\nGets the NXT's current battery level.")]
    public class SendLegoGetBatteryLevel : Submit<LegoGetBatteryLevel, PortSet<LegoResponseGetBatteryLevel, Fault>>
    {
        public SendLegoGetBatteryLevel()
        {
            this.Body = new LegoGetBatteryLevel();
        }
    }

    #endregion

    #region LegoGetButtonState
    /// <summary>
    /// NOTE: 0x01, 0x94, 0x01, 0x00, 0x04, 0x00, 0x20, 0x00, 0x04, 0x00
    /// </summary>
    [DataContract]
    public class LegoGetButtonState : LegoCommand
    {
        public LegoGetButtonState()
            : base(0x01, LegoHelper.LegoCommandCode.ReadIOMap, 8)
        {
            RequireResponse = true;

            // Set the Module
            LegoHelper.SetUInt32(this.Data, 0, 0x00040001);

            // Set the offset
            LegoHelper.SetUShort(this.Data, 4, 0x0020);

            // Set the number of bytes to read
            LegoHelper.SetUShort(this.Data, 6, 0x0004);
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

    }

    /// <summary>
    /// NOTE: Because this return package does not return the index of the button queried,
    /// something special will have to be done.
    /// </summary>
    [DataContract]
    public class LegoResponseGetButtonState : LegoResponse
    {
        public LegoResponseGetButtonState()
            : base(0x02, LegoHelper.LegoCommandCode.ReadIOMap, 11) { }

        public LegoResponseGetButtonState(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.ReadIOMap, 0)
        {
            this.Data = cmd.Data;
        }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion

        /// <summary>
        /// Determine if the LEGO response packet was a request for Button state.
        /// </summary>
        /// <param name="data"></param>
        /// <returns></returns>
        public static bool IsValidButtonStateResponse(LegoResponse legoResponse)
        {
            if (   (legoResponse == null )
                || (legoResponse.Data == null)
                || legoResponse.Data.Length != 11
                || legoResponse.LegoCommandCode != LegoHelper.LegoCommandCode.ReadIOMap
                || legoResponse.CommandType != 2 // Lego Return Code
                || legoResponse.Data[0] != 0x00 // success
                || legoResponse.Data[1] != 0x01 // offset byte 1
                || legoResponse.Data[2] != 0x00 // offset byte 2
                || legoResponse.Data[3] != 0x04 // offset byte 3
                || legoResponse.Data[4] != 0x00 // offset byte 4
                || legoResponse.Data[5] != 0x04 // data length byte 1
                || legoResponse.Data[6] != 0x00 // data length byte 2
                )
                return false;

            return true;
        }

        public LegoResponseGetButtonState(bool right, bool left, bool enter, bool cancel)
            : base(0x02, LegoHelper.LegoCommandCode.ReadIOMap, 11) 
        {
            PressedRight = right;
            PressedLeft = left;
            PressedEnter = enter;
            PressedCancel = cancel;
        }


        /// <summary>
        /// The number of bytes read from IO Mapped data
        /// </summary>
        private int BytesRead
        {
            get { return LegoHelper.GetUShort(this.Data, 5); }
        }

        /// <summary>
        /// The IO Mapped Data which is returned
        /// </summary>
        private byte[] MappedData
        {
            get
            {
                int bytesRead = this.BytesRead;
                if (Data == null || bytesRead == 0 || Data.Length < (7 + bytesRead))
                    return null;

                byte[] mappedData = new byte[bytesRead];
                for (int ix = 0; ix < bytesRead; ix++)
                    mappedData[ix] = Data[ix + 7];

                return mappedData;
            }
        }


        /// <summary>
        /// Right Button is pressed
        /// </summary>
        [DataMember]
        [Description("Indicates that the right button was pressed.")]
        [DataMemberConstructor(Order = 1)]
        public bool PressedRight
        {
            get
            {
                if (Data == null || Data.Length < 11)
                    return false;

                return (Data[8] & 0x80) == 0x80;
            }
            set
            {
                Data[8] = (byte)(value ? 0x80 : 0x00);
            }
        }

        /// <summary>
        /// Left button is pressed.
        /// </summary>
        [DataMember]
        [DataMemberConstructor(Order = 2)]
        [Description("Indicates the left button was pressed.")]
        public bool PressedLeft
        {
            get
            {
                if (Data == null || Data.Length < 11)
                    return false;

                return (Data[9] & 0x80) == 0x80;
            }
            set
            {
                Data[9] = (byte)(value ? 0x80 : 0x00);
            }
        }

        /// <summary>
        /// Enter button is pressed
        /// </summary>
        [DataMember]
        [DataMemberConstructor(Order = 3)]
        [Description("Indicates that the Enter button was pressed.")]
        public bool PressedEnter
        {
            get
            {
                if (Data == null || Data.Length < 11)
                    return false;

                return (Data[10] & 0x80) == 0x80;
            }
            set
            {
                Data[10] = (byte)(value ? 0x80 : 0x00);
            }
        }


        /// <summary>
        /// Cancel Button is pressed
        /// </summary>
        [DataMember]
        [DataMemberConstructor(Order = 4)]
        [Description("Indicates that the Cancel button was pressed.")]
        public bool PressedCancel
        {
            get
            {
                if (Data == null || Data.Length < 11)
                    return false;

                return (Data[7] & 0x80) == 0x80;
            }
            set
            {
                Data[7] = (byte)(value ? 0x80 : 0x00);
            }
        }

    }

    #endregion


    #region LegoGetInputValues

    [DataContract]
    public class LegoGetInputValues : LegoCommand
    {
        /// <summary>
        /// Range 0-3
        /// </summary>
        private int _inputPort;

        public LegoGetInputValues()
            : base(0x00, LegoHelper.LegoCommandCode.GetInputValues, 1)
        {
            base.RequireResponse = true;
        }

        public LegoGetInputValues(int inputPort)
            : base(0x00, LegoHelper.LegoCommandCode.GetInputValues, 1)
        {
            base.RequireResponse = true;
            InputPort = inputPort;
        }


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        /// <summary>
        /// 0,1,2,3
        /// </summary>
        [DataMember]
        [Description("The input port on the NXT brick (0, 1, 2, or 3).")]
        public int InputPort
        {
            get { return (int)_inputPort; }
            set
            {
                _inputPort = value;
                this.Data[0] = (byte)_inputPort;
            }
        }
    }

    [DataContract]
    public class LegoResponseGetInputValues : LegoResponse
    {
        public LegoResponseGetInputValues()
            : base(0x02, LegoHelper.LegoCommandCode.GetInputValues, 14)
        {
        }

        public LegoResponseGetInputValues(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.GetInputValues, 14, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The input port on the NXT brick (0, 1, 2, or 3).")]
        public int InputPort
        {
            get
            {
                if (Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                if (Data.Length >= 2)
                    Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("Are the InputValues Valid for this sensor?")]
        public bool Valid
        {
            get
            {
                if (Data.Length >= 3)
                    return (Data[2] == 0) ? false : true;
                return false;
            }
            set
            {
                if (Data.Length >= 3)
                    Data[2] = (byte)((value) ? 1 : 0);
            }
        }

        [DataMember]
        [Description("Is the specified sensor Calibrated?")]
        public bool Calibrated
        {
            get
            {
                if (Data.Length >= 4)
                    return (Data[3] == 0) ? false : true;
                return false;
            }
            set
            {
                if (Data.Length >= 4)
                    Data[3] = (byte)((value) ? 1 : 0);
            }
        }

        [DataMember]
        [Description("The LEGO NXT Sensor Type as defined by LEGO")]
        public LegoSensorType SensorType
        {
            get
            {
                if (Data.Length >= 5)
                    return (LegoSensorType)Data[4];

                return LegoSensorType.NoSensor;
            }
            set
            {
                if (Data.Length >= 5)
                    Data[4] = (byte)value;
            }
        }

        [DataMember]
        [Description("The Sensor Mode")]
        public LegoSensorMode SensorMode
        {
            get
            {
                if (Data.Length >= 6)
                    return (LegoSensorMode)Data[5];
                return LegoSensorMode.RawMode;
            }
            set
            {
                if (Data.Length >= 6)
                    Data[5] = (byte)value;
            }
        }

        [DataMember]
        [Description("The raw reading from the sensor.")]
        public int RawValue
        {
            get { return (int)BitConverter.ToUInt16(Data, 6); }
            set { LegoHelper.SetUShort(Data, 6, value); }
        }

        [DataMember]
        [Description("The normalized reading from the sensor.")]
        public int NormalizedValue
        {
            get { return (int)BitConverter.ToUInt16(Data, 8); }
            set { LegoHelper.SetUShort(Data, 8, value); }
        }

        [DataMember]
        [Description("The scaled reading from the sensor.")]
        public int ScaledValue
        {
            get { return (int)BitConverter.ToInt16(Data, 10); }
            set { LegoHelper.SetShort(Data, 10, value); }
        }


        [DataMember]
        [Description("The calibrated reading from the sensor.")]
        public int CalibratedValue
        {
            get { return (int)BitConverter.ToInt16(Data, 12); }
            set { LegoHelper.SetShort(Data, 12, value); }
        }
    }

    #endregion

    #region LegoGetOutputState
    [DataContract]
    public class LegoGetOutputState : LegoCommand
    {
        /// <summary>
        /// Range 0-2
        /// </summary>
        private int _outputPort;

        public LegoGetOutputState()
            : base(0x00, LegoHelper.LegoCommandCode.GetOutputState, 1)
        {
            base.RequireResponse = true;
        }

        public LegoGetOutputState(int outputPort)
            : base(0x00, LegoHelper.LegoCommandCode.GetOutputState, 1)
        {
            base.RequireResponse = true;
            OutputPort = outputPort;
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        /// <summary>
        /// 0, 1, 2
        /// </summary>
        [DataMember]
        [Description("The NXT output port (0,1, or 2).")]
        public int OutputPort
        {
            get { return (int)_outputPort; }
            set
            {
                _outputPort = value;
                this.Data[0] = (byte)_outputPort;
            }
        }
    }

    [DataContract]
    public class LegoResponseGetOutputState : LegoResponse
    {
        public LegoResponseGetOutputState()
            : base(0x02, LegoHelper.LegoCommandCode.GetOutputState, 23) { }

        public LegoResponseGetOutputState(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.GetOutputState, 23, cmd.Data) { }

        #region Hide base type DataMembers


        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The NXT output port (0, 1, or 2).")]
        public int OutputPort
        {
            get
            {
                if (Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The motor power setting (range -100 to +100).")]
        public int PowerSetPoint
        {
            get
            {
                if (Data.Length >= 3)
                    return Data[2];
                return -1;
            }
            set
            {
                Data[2] = (byte)value;
            }
        }

        [DataMember]
        [Description("The NXT output mode.")]
        public LegoOutputMode Mode
        {
            get
            {
                if (Data.Length >= 4)
                    return (LegoOutputMode)Data[3];
                return LegoOutputMode.Brake;
            }
            set
            {
                Data[3] = (byte)value;
            }
        }

        [DataMember]
        [Description("The NXT regulation mode.")]
        public LegoRegulationMode RegulationMode
        {
            get
            {
                if (Data.Length >= 5)
                    return (LegoRegulationMode)Data[4];
                return LegoRegulationMode.Idle;
            }
            set
            {
                Data[4] = (byte)value;
            }
        }

        /// <summary>
        /// The Motor Turn Ratio 
        /// <remarks>(-100 - 100)</remarks>
        /// </summary>
        [DataMember]
        [Description("Motor Turn Ratio")]
        public int TurnRatio
        {
            get
            {
                if (Data.Length >= 6)
                    return Data[5];
                return -1;
            }
            set
            {
                Data[5] = (byte)value;
            }
        }

        [DataMember]
        [Description("The Motor running state")]
        public LegoRunState RunState
        {
            get
            {
                if (Data.Length >= 7)
                    return (LegoRunState)Data[6];
                return LegoRunState.Idle;
            }
            set
            {
                Data[6] = (byte)value;
            }
        }

        [DataMember]
        [Description("The Motor Tachometer Limit")]
        public long TachoLimit
        {
            get { return (long)BitConverter.ToUInt32(Data, 7); }
            set
            {
                LegoHelper.SetUInt32(Data, 7, value);
            }
        }

        [DataMember]
        [Description("The Motor Tachometer Count")]
        public long TachoCount
        {
            get { return (long)BitConverter.ToInt32(Data, 11); }
            set { LegoHelper.SetUInt32(Data, 11, value); }
        }

        [DataMember]
        [Description("The Motor Block Tachometer Count")]
        public long BlockTachoCount
        {
            get { return (long)BitConverter.ToInt32(Data, 15); }
            set { LegoHelper.SetUInt32(Data, 15, value); }
        }

        [DataMember]
        [Description("The Motor Rotation Count")]
        public long RotationCount
        {
            get { return (long)BitConverter.ToInt32(Data, 19); }
            set { LegoHelper.SetUInt32(Data, 19, value); }
        }
    }


    #endregion

    #region LegoLSGetStatus

    [DataContract]
    public class LegoLSGetStatus : LegoCommand
    {
        int _port;

        public LegoLSGetStatus()
            : base(0x00, LegoHelper.LegoCommandCode.LSGetStatus, 1)
        {
            base.RequireResponse = true;
        }

        public LegoLSGetStatus(int port)
            : base(0x00, LegoHelper.LegoCommandCode.LSGetStatus, 1)
        {
            base.RequireResponse = true;
            Port = port;
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        /// <summary>
        /// 0,1,2,3
        /// </summary>
        [DataMember]
        [Description("The input port (0, 1, 2, or 3).")]
        public int Port
        {
            get { return (int)_port; }
            set
            {
                _port = value;
                this.Data[0] = (byte)_port;
            }
        }
    }

    [DataContract]
    public class LegoResponseLSGetStatus : LegoResponse
    {

        public LegoResponseLSGetStatus()
            : base(0x02, LegoHelper.LegoCommandCode.LSGetStatus, 2) { }

        public LegoResponseLSGetStatus(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.LSGetStatus, 2, cmd.Data) { }

        #region Hide base type DataMembers

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The number of bytes ready to read")]
        public int BytesReady
        {
            get
            {
                if (Data.Length >= 2)
                    return (int)Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }
    }


    #endregion

    #region LegoLSWrite

    /// <summary>
    /// 
    /// <remarks>Standard Return Package</remarks>
    /// </summary>
    [DataContract]
    public class LegoLSWrite : LegoCommand
    {
        private byte _port;
        private byte _txDataLength;
        private byte _rxDataLength;
        private byte[] _txData;

        public LegoLSWrite()
            : base(0x00, LegoHelper.LegoCommandCode.LSWrite, 0)
        {
            RequireResponse = true;
        }

        public LegoLSWrite(int port, byte[] txData, byte rxDataLength)
            : base(0x00, LegoHelper.LegoCommandCode.LSWrite, 0)
        {
            RequireResponse = true;
            this.Port = port;
            this.TXDataLength = txData.Length;
            this.TXData = txData;
            this.RXDataLength = rxDataLength;
        }

        [DataMember]
        [Description("The Port")]
        public int Port
        {
            get { return (int)_port; }
            set
            {
                _port = (byte)value;
                if (this.Data == null)
                    this.Data = new byte[3];
                this.Data[0] = _port;
            }
        }

        [DataMember]
        [Description("The transmitted data length.")]
        public int TXDataLength
        {
            get { return (int)_txDataLength; }
            set
            {
                _txDataLength = (byte)value;
                if (this.Data == null)
                    this.Data = new byte[3];
                this.Data[1] = _txDataLength;
            }
        }

        [DataMember]
        [Description("The received data length.")]
        public int RXDataLength
        {
            get { return (int)_rxDataLength; }
            set
            {
                _rxDataLength = (byte)value;
                if (this.Data == null)
                    this.Data = new byte[3];
                this.Data[2] = _rxDataLength;
            }
        }

        [DataMember]
        [Description("The transmitted data.")]
        public byte[] TXData
        {
            get { return _txData; }
            set
            {
                _txData = value;
                if (this.Data == null)
                    this.Data = new byte[_txData.Length + 3];
                this.Data = LegoHelper.AppendData(this.Data, 3, _txData);
            }
        }
    }


    #endregion

    #region LegoLSRead
    [DataContract]
    public class LegoLSRead : LegoCommand
    {
        int _port;

        public LegoLSRead()
            : base(0x00, LegoHelper.LegoCommandCode.LSRead, 1)
        {
            base.RequireResponse = true;
        }

        public LegoLSRead(int port)
            : base(0x00, LegoHelper.LegoCommandCode.LSRead, 1)
        {
            base.RequireResponse = true;
            Port = port;
        }

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        /// <summary>
        /// 0,1,2,3
        /// </summary>
        [DataMember]
        [Description("The input port (0, 1, 2, or 3).")]
        public int Port
        {
            get { return (int)_port; }
            set
            {
                _port = value;
                this.Data[0] = (byte)_port;
            }
        }
    }

    [DataContract]
    public class LegoResponseLSRead : LegoResponse
    {
        public LegoResponseLSRead()
            : base(0x02, LegoHelper.LegoCommandCode.LSRead, 18)
        {
        }

        public LegoResponseLSRead(LegoCommand cmd)
            : base(0x02, LegoHelper.LegoCommandCode.LSRead, 18, cmd.Data) { }

        #region Hide base type DataMembers

        /// <summary>
        /// Hide RequireResponse from proxy and always set it to true.
        /// </summary>
        [Description("Identifies whether to send an acknowledgement back on a command request.")]
        public override bool RequireResponse
        {
            get { return true; }
            set { base.RequireResponse = true; }
        }

        #endregion


        [DataMember]
        [Description("The number of bytes read.")]
        public int BytesRead
        {
            get
            {
                if (Data != null && Data.Length >= 2)
                    return Data[1];
                return -1;
            }
            set
            {
                Data[1] = (byte)value;
            }
        }

        [DataMember]
        [Description("The received data.")]
        public byte[] RXData
        {
            get
            {
                if (Data.Length < 3)
                    return new byte[1];

                byte[] rxdata = new byte[Data.Length - 2];
                for (int i = 0; i < rxdata.Length; i++)
                    rxdata[i] = Data[i + 2];
                return rxdata;
            }
            set
            {
                for (int ix = 0; ix < value.Length && ix < (Data.Length - 2); ix++)
                    Data[ix + 2] = value[ix];
            }
        }
        }


    #endregion

    #region LegoSetInputMode
    /// <summary>
    /// 
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoSetInputMode : LegoCommand
    {
        /// <summary>
        /// Range 0-3
        /// </summary>
        private int _inputPort;
        private LegoSensorType _sensorType;
        private LegoSensorMode _sensorMode;

        public LegoSetInputMode()
            : base(0x00, LegoHelper.LegoCommandCode.SetInputMode, 3)
        {
        }
        public LegoSetInputMode(int inputPort, LegoSensorType sensorType, LegoSensorMode sensorMode)
            : base(0x00, LegoHelper.LegoCommandCode.SetInputMode, 3)
        {
            InputPort = inputPort;
            SensorType = sensorType;
            SensorMode = sensorMode;
        }

        /// <summary>
        /// 0,1,2,3
        /// </summary>
        [DataMember]
        [Description("The input port on the NXT brick (0, 1, 2, or 3).")]
        public int InputPort
        {
            get { return (int)_inputPort; }
            set
            {
                _inputPort = value;
                this.Data[0] = (byte)_inputPort;
            }
        }

        /// <summary>
        /// Sensor Type
        /// </summary>
        [DataMember]
        public LegoSensorType SensorType
        {
            get { return _sensorType; }
            set
            {
                _sensorType = value;
                this.Data[1] = (byte)_sensorType;
            }
        }


        /// <summary>
        /// Sensor Type
        /// </summary>
        [DataMember]
        [Description("The translation mode of the LEGO NXT sensor.")]
        public LegoSensorMode SensorMode
        {
            get { return _sensorMode; }
            set
            {
                _sensorMode = value;
                this.Data[2] = (byte)_sensorMode;
            }
        }
    }

    #endregion

    #region LegoSetOutputState
    /// <summary>
    /// 
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoSetOutputState : LegoCommand
    {
        public LegoSetOutputState()
            : base(0x00, LegoHelper.LegoCommandCode.SetOutputState, 10) { }

        public LegoSetOutputState(int outputPort,
                                  int powerSetPoint,
                                  LegoOutputMode mode,
                                  LegoRegulationMode regulationMode,
                                  int turnRatio,
                                  LegoRunState runState,
                                  long rotationLimit)
            : base(0x00, LegoHelper.LegoCommandCode.SetOutputState, 10)
        {
            this.OutputPort = outputPort;
            this.PowerSetPoint = powerSetPoint;
            this.Mode = mode;
            this.RegulationMode = regulationMode;
            this.TurnRatio = turnRatio;
            this.RunState = runState;
            this.RotationLimit = rotationLimit;
        }

        public LegoSetOutputState(byte[] data)
            : base(0x00, LegoHelper.LegoCommandCode.SetOutputState, 10, data) { }


        /// <summary>
        /// 0,1,2 or 255 for all
        /// </summary>
        [DataMember, DataMemberConstructor(Order = 1)]
        [Description("The output port on the NXT brick (0, 1, 2, or 255 for all three.)")]
        public int OutputPort
        {
            get
            {
                if (Data.Length == 10)
                    return (int)Data[0];
                return -1;
            }
            set
            {
                this.Data[0] = (byte)value;
            }
        }

        /// <summary>
        /// Power Setpoint (range -100 to +100)
        /// </summary>
        [DataMember, DataMemberConstructor(Order = 2)]
        [Description("The motor power setting (range -100 to +100).")]
        public int PowerSetPoint
        {
            get
            {
                if (Data.Length == 10)
                    return (int)LegoHelper.GetSByte(this.Data, 1);
                return -1;
            }
            set
            {
                LegoHelper.SetSByte(this.Data, 1, value);
            }
        }

        /// <summary>
        /// Mode
        /// </summary>
        [DataMember, DataMemberConstructor(Order = 3)]
        [Description("The NXT output mode.")]
        public LegoOutputMode Mode
        {
            get
            {
                if (Data.Length == 10)
                    return (LegoOutputMode)Data[2];
                return LegoOutputMode.Brake;
            }
            set
            {
                this.Data[2] = (byte)value;
            }
        }

        /// <summary>
        /// Lego Regulation Mode
        /// </summary>
        [DataMember, DataMemberConstructor(Order = 4)]
        [Description("The NXT regulation mode.")]
        public LegoRegulationMode RegulationMode
        {
            get
            {
                if (Data.Length == 10)
                    return (LegoRegulationMode)Data[3];
                return LegoRegulationMode.Idle;
            }
            set
            {
                this.Data[3] = (byte)value;
            }
        }

        /// <summary>
        /// The Motor Turn Ratio 
        /// <remarks>(-100 - 100)</remarks>
        /// </summary>
        [DataMember, DataMemberConstructor(Order = 5)]
        [Description("The Motor Turn Ratio")]
        public int TurnRatio
        {
            get
            {
                if (Data.Length == 10)
                    return (int)LegoHelper.GetSByte(this.Data, 4);
                return -1;
            }
            set
            {
                LegoHelper.SetSByte(this.Data, 4, value);
            }
        }

        /// <summary>
        /// RunState
        /// </summary>
        [DataMember, DataMemberConstructor(Order = 6)]
        [Description("The Motor Run State")]
        public LegoRunState RunState
        {
            get
            {
                if (Data.Length == 10)
                    return (LegoRunState)Data[5];
                return LegoRunState.Idle;
            }
            set
            {
                this.Data[5] = (byte)value;
            }
        }

        /// <summary>
        /// Limit on number of complete wheel rotations
        /// </summary>
        [DataMember, DataMemberConstructor(Order = 7)]
        [Description("A limit on the number of complete wheel rotations")]
        public long RotationLimit
        {
            get
            {
                if (Data.Length == 10)
                    return (long)(long)BitConverter.ToUInt32(this.Data, 6);
                return -1;
            }
            set
            {
                if (this.Data == null) this.Data = new byte[10];
                LegoHelper.SetUInt32(this.Data, 6, value);
            }
        }
    }

    #endregion

    #region LegoSetBrickName
    /// <summary>
    /// Set the Lego Brick Name
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoSetBrickName : LegoCommand
    {
        private string _name = string.Empty;

        public LegoSetBrickName()
            : base(0x01, LegoHelper.LegoCommandCode.SetBrickName, 16)
        {
        }

        public LegoSetBrickName(string name)
            : base(0x01, LegoHelper.LegoCommandCode.SetBrickName, 0)
        {
            this.Name = name;
        }

        [DataMember]
        [DataMemberConstructor(Order = 1)]
        [Description("The descriptive identifier for the NXT brick.")]
        public string Name
        {
            get { return _name; }
            set
            {
                _name = value;
                LegoHelper.SetStringToData(this.Data, 0, _name, 16);
            }
        }

    }


    /// <summary>
    /// Request/Response definition for sending a LegoSetBrickName
    /// </summary>
    [Description("Sends the LEGO Set Brick Name to the NXT brick.\nSets the name of the NXT .")]
    public class SendLegoSetBrickName : Submit<LegoSetBrickName, PortSet<LegoResponse, Fault>>
    {
    }

    #endregion


    #region LegoResetInputScaledValue

    /// <summary>
    /// Reset Motor Position   
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoResetInputScaledValue : LegoCommand
    {
        private int _inputPort;

        public LegoResetInputScaledValue()
            : base(0x00, LegoHelper.LegoCommandCode.ResetInputScaledValue, 1) { }

        public LegoResetInputScaledValue(int inputPort)
            : base(0x00, LegoHelper.LegoCommandCode.ResetInputScaledValue, 1)
        {
            this.InputPort = inputPort;
        }


        /// <summary>
        /// Input Port 0-3
        /// </summary>
        [DataMember]
        [DataMemberConstructor(Order = 1)]
        [Description("The input port on the NXT brick (0-3).")]
        public int InputPort
        {
            get { return (int)_inputPort; }
            set
            {
                _inputPort = value;
                if (this.Data == null) this.Data = new byte[1];
                this.Data[0] = (byte)value;
            }
        }

    }


    #endregion

    #region LegoResetMotorPosition

    /// <summary>
    /// Reset Motor Position   
    /// <remarks>Standard return package.</remarks>
    /// </summary>
    [DataContract]
    public class LegoResetMotorPosition : LegoCommand
    {
        private int _outputPort;
        private bool _relative;

        public LegoResetMotorPosition()
            : base(0x00, LegoHelper.LegoCommandCode.ResetMotorPosition, 2) { }

        public LegoResetMotorPosition(int outputPort, bool relative)
            : base(0x00, LegoHelper.LegoCommandCode.ResetMotorPosition, 2)
        {
            this.OutputPort = outputPort;
            this.Relative = relative;
        }

        /// <summary>
        /// Output Port 0-2
        /// </summary>
        [DataMember]
        [Description("The NXT output port (0, 1, or 2).")]
        [DataMemberConstructor(Order = 1)]
        public int OutputPort
        {
            get { return (int)_outputPort; }
            set
            {
                _outputPort = value;
                if (this.Data == null) this.Data = new byte[2];
                this.Data[0] = (byte)value;
            }
        }

        /// <summary>
        /// Position relative to last movement or absolute?
        /// </summary>
        [DataMember]
        [Description("Identifies whether the position is relative to the last movement.")]
        [DataMemberConstructor(Order = 2)]
        public bool Relative
        {
            get { return _relative; }
            set
            {
                _relative = value;
                if (this.Data == null) this.Data = new byte[2];
                this.Data[1] = (byte)((_relative) ? 1 : 0);
            }
        }
    }


    #endregion

    #endregion

}
