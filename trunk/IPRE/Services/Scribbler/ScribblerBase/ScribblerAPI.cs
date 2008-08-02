// Copyright (c) Microsoft Corporation.  All rights reserved.

//------------------------------------------------------------------------------
// Scribbler API Helper
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
using Myro.Utilities;


// Scribbler API Helper
// See scribbler_server.bs2 for more documentation
//
// API wishlist:
// * renumber message type commands to be in order
// * send the low byte first

namespace Myro.Services.Scribbler.ScribblerBase
{
    public static class ScribblerHelper
    {

        private const int SetMessageReturnLength = 12;
        private const int ScribblerOutMessageSize = 9;
        //public const int ImageWidth = 256;
        //public const int ImageHeight = 192;

        /// <summary>
        /// enumeration of Scribbler API commands
        /// </summary>
        public enum Commands
        {
            SOFT_RESET = 33,

            //UPDATE_FIRMWARE	    = 40,
            //SAVE_EEPROM	        = 41,
            //RESTORE_EEPROM	    = 42,
            //WATCHDOG_RESET	    = 43,

            //GET_PASS1           = 50,
            //GET_PASS2           = 51,
            //SET_PASS1           = 55,
            //SET_PASS2           = 56,

            //GET_NAME2           = 64,
            GET_ALL = 65,
            GET_ALL_BINARY = 66,
            GET_LIGHT_LEFT = 67,
            GET_LIGHT_CENTER = 68,
            GET_LIGHT_RIGHT = 69,
            GET_LIGHT_ALL = 70,
            GET_IR_LEFT = 71,
            GET_IR_RIGHT = 72,
            GET_IR_ALL = 73,
            GET_LINE_LEFT = 74,
            GET_LINE_RIGHT = 75,
            GET_LINE_ALL = 76,
            GET_STATE = 77,
            GET_NAME = 78,
            GET_STALL = 79,
            GET_INFO = 80,
            GET_DATA = 81,

            //GET_RLE             = 82,  // a segmented and run-length encoded image
            GET_IMAGE = 83,  // the entire 256 x 192 image in YUYV format
            GET_WINDOW = 84,  // the windowed image (followed by which window)
            GET_DONGLE_L_IR = 85,  // number of returned pulses when left emitter is turned on
            GET_DONGLE_C_IR = 86,  // number of returned pulses when center emitter is turned on
            GET_DONGLE_R_IR = 87,  // number of returned pulses when right emitter is turned on
            //GET_WINDOW_LIGHT    = 88,  // average intensity in the user defined region
            //GET_BATTERY         = 89,  // battery voltage
            //GET_SERIAL_MEM      = 90,  // with the address returns the value in serial memory
            //GET_SCRIB_PROGRAM   = 91,  // with offset, returns the scribbler program buffer
            GET_CAM_PARAM = 92,  // get the camera parameter at specified address
            //GET_IMAGE_COMPRESSED= 93,
            //GET_BLOB_WINDOW     = 94,
            //GET_BLOB            = 95,
            //SET_SINGLE_DATA     = 96,  // Sets a single byte of data in flash memory//

            SET_DATA = 97,
            SET_ECHO_MODE = 98,
            SET_LED_LEFT_ON = 99,
            SET_LED_LEFT_OFF = 100,
            SET_LED_CENTER_ON = 101,
            SET_LED_CENTER_OFF = 102,
            SET_LED_RIGHT_ON = 103,
            SET_LED_RIGHT_OFF = 104,
            SET_LED_ALL_ON = 105,
            SET_LED_ALL_OFF = 106,
            SET_LED_ALL = 107,
            SET_MOTORS_OFF = 108,
            SET_MOTORS = 109,
            SET_NAME = 110,
            SET_LOUD = 111,
            SET_QUIET = 112,
            SET_SPEAKER = 113,
            SET_SPEAKER_2 = 114,


            SET_DONGLE_LED_ON = 116,
            SET_DONGLE_LED_OFF = 117,
            //SET_RLE             = 118,
            //SET_NAME2           = 119,  // Format: 110 char1 char2 char3 char4 char5 char6 char7 char8
            SET_DONGLE_IR       = 120,
            //SET_SERIAL_MEM      = 121,
            //SET_SCRIB_PROGRAM   = 122,
            //SET_START_PROGRAM   = 123,
            //SET_RESET_SCRIBBLER = 124,
            //SET_SERIAL_ERASE    = 125,
            SET_DIMMER_LED = 126,
            SET_WINDOW = 127,
            //SET_FORWARDNESS     = 128,
            //SET_WHITE_BALANCE   = 129,
            //SET_NO_WHITE_BALANCE= 130,
            SET_CAM_PARAM = 131,  // set the camera parameter at specified address to a value

            //SET_UART0           = 132,
            //SET_PASS_BYTE       = 133,
            //SET_PASSTHROUGH     = 134,

            GET_JPEG_GRAY_HEADER = 135,
            GET_JPEG_GRAY_SCAN = 136,
            GET_JPEG_COLOR_HEADER = 137,
            GET_JPEG_COLOR_SCAN = 138
            // NOTE: If you add or modify these commands, you will also need to modify the functions below
            // as well as ScribblerResponseHandler in Scribbler.cs
        }

        public static class CameraParams
        {
            public const byte CAM_PID = 0x0A;
            public const byte CAM_PID_DEFAULT = 0x76;

            public const byte CAM_VER = 0x0B;
            public const byte CAM_VER_DEFAULT = 0x48;

            public const byte CAM_BRT = 0x06;
            public const byte CAM_BRT_DEFAULT = 0x80;

            public const byte CAM_EXP = 0x10;
            public const byte CAM_EXP_DEFAULT = 0x41;

            public const byte CAM_COMA = 0x12;
            public const byte CAM_COMA_DEFAULT = 0x14;
            public const byte CAM_COMA_WHITE_BALANCE_ON = (CAM_COMA_DEFAULT | (1 << 2));
            public const byte CAM_COMA_WHITE_BALANCE_OFF = (CAM_COMA_DEFAULT & ~(1 << 2));

            public const byte CAM_COMB = 0x13;
            public const byte CAM_COMB_DEFAULT = 0xA3;
            public const byte CAM_COMB_GAIN_CONTROL_ON = (CAM_COMB_DEFAULT | (1 << 1));
            public const byte CAM_COMB_GAIN_CONTROL_OFF = (CAM_COMB_DEFAULT & ~(1 << 1));
            public const byte CAM_COMB_EXPOSURE_CONTROL_ON = (CAM_COMB_DEFAULT | (1 << 0));
            public const byte CAM_COMB_EXPOSURE_CONTROL_OFF = (CAM_COMB_DEFAULT & ~(1 << 0));
        }

        /// <summary>
        /// Gives the number of bytes a message should have when coming back from scribbler.
        /// This includes the command type byte.
        /// </summary>
        /// <param name="cmd"></param>
        /// <returns></returns>
        public static int ReturnSize(Commands cmd)
        {
            // Negative size means variable data, limited by either the absolute value of the negative number or
            // 0x0A, whichever is smaller.
            switch (cmd)
            {
                case Commands.GET_STATE:
                    return 3;
                    break;
                case Commands.GET_IR_LEFT:
                    return 2;
                    break;
                case Commands.GET_IR_RIGHT:
                    return 2;
                    break;
                case Commands.GET_STALL:
                    return 2;
                    break;
                case Commands.GET_LIGHT_LEFT:
                    return 3;
                    break;
                case Commands.GET_LIGHT_CENTER:
                    return 3;
                    break;
                case Commands.GET_LIGHT_RIGHT:
                    return 3;
                    break;
                case Commands.GET_LINE_RIGHT:
                    return 2;
                    break;
                case Commands.GET_LINE_LEFT:
                    return 2;
                    break;
                case Commands.GET_NAME:
                    return 9;
                    break;
                case Commands.GET_LIGHT_ALL:
                    return 7;
                    break;
                case Commands.GET_IR_ALL:
                    return 3;
                    break;
                case Commands.GET_LINE_ALL:
                    return 3;
                    break;
                case Commands.GET_ALL:
                    return 12;
                    break;
                case Commands.GET_ALL_BINARY:
                    return 2;
                    break;
                case Commands.GET_INFO:
                    return -101;
                    break;
                case Commands.GET_DATA:
                    return 9;
                    break;
                case Commands.SET_MOTORS_OFF:
                case Commands.SET_MOTORS:
                case Commands.SET_LED_LEFT_ON:
                case Commands.SET_LED_LEFT_OFF:
                case Commands.SET_LED_CENTER_ON:
                case Commands.SET_LED_CENTER_OFF:
                case Commands.SET_LED_RIGHT_ON:
                case Commands.SET_LED_RIGHT_OFF:
                case Commands.SET_SPEAKER:
                case Commands.SET_SPEAKER_2:
                case Commands.SET_NAME:
                case Commands.SET_LED_ALL_ON:
                case Commands.SET_LED_ALL_OFF:
                case Commands.SET_LOUD:
                case Commands.SET_QUIET:
                case Commands.SET_LED_ALL:
                case Commands.SET_DATA:
                case Commands.SET_ECHO_MODE:
                    return SetMessageReturnLength;
                    break;
                case Commands.SET_DONGLE_LED_OFF:
                case Commands.SET_DONGLE_LED_ON:
                case Commands.SET_DIMMER_LED:
                case Commands.SOFT_RESET:
                case Commands.SET_WINDOW:
                case Commands.SET_CAM_PARAM:
                case Commands.SET_DONGLE_IR:
                    return 0;
                    break;
                case Commands.GET_DONGLE_C_IR:
                case Commands.GET_DONGLE_L_IR:
                case Commands.GET_DONGLE_R_IR:
                    return 2;
                    break;
                case Commands.GET_CAM_PARAM:
                    return 1;
                    break;
                case Commands.GET_JPEG_COLOR_HEADER:
                case Commands.GET_JPEG_COLOR_SCAN:
                case Commands.GET_JPEG_GRAY_HEADER:
                case Commands.GET_JPEG_GRAY_SCAN:
                    return -294912;
                case Commands.GET_IMAGE:
                    return MyroImageType.Color.Width * MyroImageType.Color.Height;
                    break;
                default:
                    Console.WriteLine("Unknown command - return size");
                    return 1;
                    break;
            }
        }

        public static int CommandSize(Commands cmd)
        {
            switch (cmd)
            {
                case Commands.GET_STATE:
                case Commands.GET_IR_LEFT:
                case Commands.GET_IR_RIGHT:
                case Commands.GET_STALL:
                case Commands.GET_LIGHT_LEFT:
                case Commands.GET_LIGHT_CENTER:
                case Commands.GET_LIGHT_RIGHT:
                case Commands.GET_LINE_RIGHT:
                case Commands.GET_LINE_LEFT:
                case Commands.GET_NAME:
                case Commands.GET_LIGHT_ALL:
                case Commands.GET_IR_ALL:
                case Commands.GET_LINE_ALL:
                case Commands.GET_ALL:
                case Commands.GET_ALL_BINARY:
                case Commands.GET_INFO:
                case Commands.GET_DATA:
                case Commands.SET_MOTORS_OFF:
                case Commands.SET_MOTORS:
                case Commands.SET_LED_LEFT_ON:
                case Commands.SET_LED_LEFT_OFF:
                case Commands.SET_LED_CENTER_ON:
                case Commands.SET_LED_CENTER_OFF:
                case Commands.SET_LED_RIGHT_ON:
                case Commands.SET_LED_RIGHT_OFF:
                case Commands.SET_SPEAKER:
                case Commands.SET_SPEAKER_2:
                case Commands.SET_NAME:
                case Commands.SET_LED_ALL_ON:
                case Commands.SET_LED_ALL_OFF:
                case Commands.SET_LOUD:
                case Commands.SET_QUIET:
                case Commands.SET_LED_ALL:
                case Commands.SET_DATA:
                case Commands.SET_ECHO_MODE:
                    return ScribblerOutMessageSize;
                    break;

                case Commands.SET_DONGLE_LED_OFF:
                case Commands.SET_DONGLE_LED_ON:
                case Commands.GET_DONGLE_C_IR:
                case Commands.GET_DONGLE_L_IR:
                case Commands.GET_DONGLE_R_IR:
                case Commands.GET_IMAGE:
                case Commands.GET_JPEG_COLOR_HEADER:
                case Commands.GET_JPEG_GRAY_HEADER:
                    return 1;
                    break;
                case Commands.GET_JPEG_COLOR_SCAN:
                case Commands.GET_JPEG_GRAY_SCAN:
                case Commands.SET_DIMMER_LED:
                case Commands.GET_CAM_PARAM:
                case Commands.SET_DONGLE_IR:
                    return 2;
                    break;
                case Commands.SOFT_RESET:
                    return 1;
                    break;
                case Commands.SET_WINDOW:
                    return 8;
                    break;
                case Commands.SET_CAM_PARAM:
                    return 3;
                    break;
                default:
                    Console.WriteLine("Unknown command - command size");
                    return 1;
                    break;
            }
        }

        public static bool HasEcho(Commands cmd)
        {
            switch (cmd)
            {
                case Commands.GET_STATE:
                case Commands.GET_IR_LEFT:
                case Commands.GET_IR_RIGHT:
                case Commands.GET_STALL:
                case Commands.GET_LIGHT_LEFT:
                case Commands.GET_LIGHT_CENTER:
                case Commands.GET_LIGHT_RIGHT:
                case Commands.GET_LINE_RIGHT:
                case Commands.GET_LINE_LEFT:
                case Commands.GET_NAME:
                case Commands.GET_LIGHT_ALL:
                case Commands.GET_IR_ALL:
                case Commands.GET_LINE_ALL:
                case Commands.GET_ALL:
                case Commands.GET_ALL_BINARY:
                case Commands.GET_INFO:
                case Commands.GET_DATA:
                case Commands.SET_MOTORS_OFF:
                case Commands.SET_MOTORS:
                case Commands.SET_LED_LEFT_ON:
                case Commands.SET_LED_LEFT_OFF:
                case Commands.SET_LED_CENTER_ON:
                case Commands.SET_LED_CENTER_OFF:
                case Commands.SET_LED_RIGHT_ON:
                case Commands.SET_LED_RIGHT_OFF:
                case Commands.SET_SPEAKER:
                case Commands.SET_SPEAKER_2:
                case Commands.SET_NAME:
                case Commands.SET_LED_ALL_ON:
                case Commands.SET_LED_ALL_OFF:
                case Commands.SET_LOUD:
                case Commands.SET_QUIET:
                case Commands.SET_LED_ALL:
                case Commands.SET_DATA:
                case Commands.SET_ECHO_MODE:
                    return true;
                    break;

                case Commands.SET_DONGLE_LED_OFF:
                case Commands.SET_DONGLE_LED_ON:
                case Commands.SET_DIMMER_LED:
                case Commands.GET_DONGLE_C_IR:
                case Commands.GET_DONGLE_L_IR:
                case Commands.GET_DONGLE_R_IR:
                case Commands.GET_IMAGE:
                case Commands.SOFT_RESET:
                case Commands.SET_WINDOW:
                case Commands.SET_CAM_PARAM:
                case Commands.GET_CAM_PARAM:
                case Commands.GET_JPEG_COLOR_HEADER:
                case Commands.GET_JPEG_COLOR_SCAN:
                case Commands.GET_JPEG_GRAY_HEADER:
                case Commands.GET_JPEG_GRAY_SCAN:
                case Commands.SET_DONGLE_IR:
                    return false;
                    break;
                default:
                    Console.WriteLine("Unknown command - command echo");
                    return false;
                    break;
            }
        }

        public static UInt16 GetShort(byte[] data, int offset)
        {
//            Console.WriteLine("GetShort:  byte1 = " + data[offset] + " byte2 = " + data[offset + 1]);
            return (UInt16)((data[offset] << 8) | data[offset + 1]);
        }


        public struct GetStatusDecomp
        {
            //ins
            public bool LineLeft;
            public bool LineRight;
            public bool Stall;

            //outs
            public bool LedLeft;
            public bool LedCenter;
            public bool LedRight;

            public GetStatusDecomp(byte ins, byte outs)
            {
                LineRight = (ins & 0x10) > 0;
                LineLeft = (ins & 0x20) > 0;
                Stall = (ins & 0x80) > 0;

                LedRight = (outs & 0x01) > 0;
                LedCenter = (outs & 0x02) > 0;
                LedLeft = (outs & 0x04) > 0;
            }
        }

        /// <summary>
        /// takes in the status byte from the 'AllBinary' command and 
        /// demomposes it into sensor data.
        /// <remarks>Format: 0b000 IRLeft IRRight Stall LineLeft LineRight</remarks>
        /// </summary>
        public struct AllBinaryDecomp
        {
            public bool IRLeft;
            public bool IRRight;
            public bool Stall;
            public bool LineLeft;
            public bool LineRight;

            public AllBinaryDecomp(byte b)
            {
                this.IRLeft = (b & 0x10) > 0;
                this.IRRight = (b & 0x08) > 0;
                this.Stall = (b & 0x04) > 0;
                this.LineLeft = (b & 0x02) > 0;
                this.LineRight = (b & 0x01) > 0;
            }
        }

    }

    public class ScribblerProtocolException : Exception
    {
        public ScribblerProtocolException() : base() { }
        public ScribblerProtocolException(string message) : base(message) { }
        public ScribblerProtocolException(string message, Exception innerException) : base(message, innerException) { }
    }

    public class ScribblerDataException : Exception
    {
        public ScribblerDataException() : base() { }
        public ScribblerDataException(string message) : base(message) { }
        public ScribblerDataException(string message, Exception innerException) : base(message, innerException) { }
    }

    public class ScribblerBadCOMPortException : Exception
    {
    }
}
