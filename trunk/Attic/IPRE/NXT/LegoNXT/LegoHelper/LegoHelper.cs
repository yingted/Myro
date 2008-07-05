//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoHelper.cs $ $Revision$
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Robotics.Services.LegoNxt.Helper
{

    public class LegoHelper
    {
        public enum LegoCommandCode : byte
        {
            StartProgram = 0x00,
            StopProgram = 0x01,
            PlaySoundFile = 0x02,
            PlayTone = 0x03,
            SetOutputState = 0x04,
            SetInputMode = 0x05,
            GetOutputState = 0x06,
            GetInputValues = 0x07,
            ResetInputScaledValue = 0x08,
            MessageWrite = 0x09,
            ResetMotorPosition = 0x0A,
            GetBatteryLevel = 0x0B,
            StopSoundPlayback = 0x0C,
            KeepAlive = 0x0D,
            LSGetStatus = 0x0E,
            LSWrite = 0x0F,
            LSRead = 0x10,
            GetCurrentProgramName = 0x11,
            MessageRead = 0x13,
            OpenRead = 0x80,
            OpenWrite = 0x81,
            Read = 0x82,
            Write = 0x83,
            Close = 0x84,
            Delete = 0x85,
            FindFirst = 0x86,
            FindNext = 0x87,
            GetFirmwareVersion = 0x88,
            OpenWriteLinear = 0x89,
            OpenReadLinear = 0x8A,
            OpenWriteData = 0x8B,
            OpenAppendData = 0x8C,
            RequestFirstModule = 0x90,
            RequestNextModule = 0x91,
            CloseModuleHandle = 0x92,
            ReadIOMap = 0x94,
            WriteIOMap = 0x95,
            BootCommand = 0x97,
            SetBrickName = 0x98,
            GetDeviceInfo = 0x9B,
            DeleteUserFlash = 0xA0,
            PollCommandLength = 0xA1,
            PollCommand = 0xA2,
            BluetoothFactoryReset = 0xA4,
        }

        public enum LegoErrorCode : byte
        {
            Success = 0x00,
            NoMoreHandles = 0x81,
            NoSpace = 0x82,
            NoMoreFiles = 0x83,
            EndOfFileExpected = 0x84,
            EndOfFile = 0x85,
            NotLinearFile = 0x86,
            FileNotFound = 0x87,
            HandleAlreadyClosed = 0x88,
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
        }


        /// <summary>
        /// Convert string to lego data packet.
        /// If string is longer than maxlength, 
        /// the leftmost portion of the string 
        /// will be retained.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="bufferLength">Maximum length including null terminator</param>
        /// <returns></returns>
        public static byte[] StringToData(string value, int bufferLength)
        {
            if (value == null)
                throw new ArgumentNullException("value");

            if (value.Length >= bufferLength)
                value = value.Substring(0, (bufferLength - 1));

            byte[] result = new byte[bufferLength];
            int ix = 0;
            foreach (char c in value)
                result[ix++] = (byte)c;

            while (ix < bufferLength)
                result[ix++] = 0;

            return result;
        }

        public static bool SetStringToData(byte[] data, int startPosition, string value)
        {
            if (data == null)
                throw new ArgumentNullException("data");

            if (startPosition >= data.Length)
                throw new ArgumentOutOfRangeException("startPosition");

            if (value == null)
                throw new ArgumentNullException("value");

            return SetStringToData(data, startPosition, value, value.Length);
        }

        public static bool SetStringToData(byte[] data, int startPosition, string value, int length)
        {
            // Do we have room in the buffer?
            if (data == null)
                throw new ArgumentNullException("data");

            if (startPosition >= data.Length)
                throw new ArgumentOutOfRangeException("startPosition");

            // is length too long?
            if (length + startPosition > data.Length)
                throw new ArgumentOutOfRangeException("length");

            if (value == null)
                throw new ArgumentNullException("value");

            //shorten string if too long
            if (value.Length >= length)
                value = value.Substring(0, length);

            //write string
            int ix = startPosition;
            foreach (char c in value)
                data[ix++] = (byte)c;

            //fill up to length will null
            while (ix < length)
                data[ix++] = 0;

            return true;
        }

        public static string DataToString(byte[] data, int startPosition)
        {
            return DataToString(data, startPosition, data.Length - startPosition);
        }

        public static string DataToString(byte[] data, int startPosition, int maximumLength)
        {
            int length = 0;
            while (startPosition + length < maximumLength && data[startPosition + length] != 0)
                length++;
            return System.Text.Encoding.ASCII.GetString(data, startPosition, length);
        }

        public static int DataToInt(byte[] data, int startPosition, int length)
        {
            return int.Parse(System.Text.Encoding.ASCII.GetString(data, startPosition, length), System.Globalization.NumberFormatInfo.CurrentInfo);
        }

        public static int GetSByte(byte[] data, int startPosition)
        {
            // Do we have room in the buffer?
            if (data == null)
                throw new ArgumentNullException("data");

            if (startPosition >= data.Length)
                throw new ArgumentOutOfRangeException("startPosition");

            if (data[startPosition] >= 128)
            {
                int temp = (byte)data[startPosition] ^ (byte)0xFF;
                temp += 1;
                return -temp;
            }
            else
                return data[startPosition];
        }

        public static bool SetSByte(byte[] data, int startPosition, int value)
        {
            // Do we have room in the buffer?
            if (data == null)
                throw new ArgumentNullException("data");

            if (startPosition > data.Length - 1)
                throw new ArgumentOutOfRangeException("startPosition");

            if (value < 0)
                data[startPosition] = (byte)value;
            else
                data[startPosition] = (byte)value;
            return true;
        }

        /// <summary>
        /// Set a 16 bit unsigned integer in little-endian format
        /// </summary>
        /// <param name="data"></param>
        /// <param name="startPosition"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        public static bool SetUShort(byte[] data, int startPosition, int value)
        {
            // Do we have room in the buffer?
            if (data == null)
                throw new ArgumentNullException("data");

            if (startPosition > data.Length - 2)
                throw new ArgumentOutOfRangeException("startPosition");

            ushort uValue = (ushort)value;

            data[startPosition] = (byte)(uValue & 0xFF); //LSB is first
            data[startPosition + 1] = (byte)(uValue >> 8);
            return true;
        }

        /// <summary>
        /// Get a 16 bit unsigned little-endian integer from data and startPosition.
        /// </summary>
        /// <param name="data"></param>
        /// <param name="startPosition"></param>
        /// <returns></returns>
        public static int GetUShort(byte[] data, int startPosition)
        {
            if (data == null || data.Length < (startPosition + 2))
                return 0;

            return data[startPosition] + (data[startPosition + 1] * 0x100);
        }

        /// <summary>
        /// Set a signed 2-byte short integer
        /// </summary>
        /// <param name="data"></param>
        /// <param name="startPosition"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        public static bool SetShort(byte[] data, int startPosition, int value)
        {
            // Do we have room in the buffer?
            if (data == null)
                throw new ArgumentNullException("data");

            if (startPosition > data.Length - 2)
                throw new ArgumentOutOfRangeException("startPosition");

            data[startPosition] = (byte)(value & 0xFF); //LSB is first
            data[startPosition + 1] = (byte)(value >> 8);
            return true;
        }

        /// <summary>
        /// Set unsigned 32 bit integeger in little-endian format
        /// </summary>
        /// <param name="data"></param>
        /// <param name="startPosition"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        public static bool SetUInt32(byte[] data, int startPosition, long value)
        {
            // Do we have room in the buffer?
            if (data == null)
                throw new ArgumentNullException("data");

            if (startPosition > data.Length - 4)
                throw new ArgumentOutOfRangeException("startPosition");

            UInt32 uValue = (UInt32)value;

            data[startPosition] = (byte)(uValue & 0xFF);
            data[startPosition + 1] = (byte)((uValue >> 8) & 0xFF);
            data[startPosition + 2] = (byte)((uValue >> 16) & 0xFF);
            data[startPosition + 3] = (byte)((uValue >> 24) & 0xFF);
            return true;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="origdata"></param>
        /// <param name="startPosition">can be > origdata.Length</param>
        /// <param name="datatoappend"></param>
        /// <returns></returns>
        public static byte[] AppendData(byte[] originalData, int startPosition, byte[] dataToAppend)
        {
            //error check
            if (originalData == null)
                originalData = new byte[1];

            if (dataToAppend == null)
                return originalData;

            //copy over data to new array
            if (startPosition + dataToAppend.Length > originalData.Length)
            {
                byte[] newarray = new byte[startPosition + dataToAppend.Length];
                for (int i = 0; i < originalData.Length; i++)
                    newarray[i] = originalData[i];
                originalData = newarray;
            }

            //write data
            int ix = startPosition;
            foreach (byte b in dataToAppend)
                originalData[ix++] = b;

            return originalData;
        }


        public static string GetErrorMessage(int status)
        {
            switch (status)
            {
                case 0x00:
                    return "Success";
                case 0x81:
                    return "No more handles";
                case 0x82:
                    return "No Space";
                case 0x83:
                    return "No more files";
                case 0x84:
                    return "End of file expected";
                case 0x85:
                    return "End of file";
                case 0x86:
                    return "Not linear file";
                case 0x87:
                    return "File not found";
                case 0x88:
                    return "Handle already closed";
                case 0x89:
                    return "No linear space";
                case 0x8A:
                    return "Undefined error";
                case 0x8B:
                    return "File is buisy";
                case 0x8C:
                    return "No write buffers";
                case 0x8D:
                    return "Append not possible";
                case 0x8E:
                    return "File is full";
                case 0x8F:
                    return "File exists";
                case 0x90:
                    return "Module not found";
                case 0x91:
                    return "Out of boundary";
                case 0x92:
                    return "Illegal file name";
                case 0x93:
                    return "Illegal handle";
                default:
                    return "Unknown error code: " + status.ToString();
            }
        }

    }
}
