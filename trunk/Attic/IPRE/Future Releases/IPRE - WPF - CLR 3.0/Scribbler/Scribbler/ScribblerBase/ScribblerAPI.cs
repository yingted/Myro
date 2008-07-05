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


// Scribbler API Helper
// See scribbler_server.bs2 for more documentation
//
// API wishlist:
// * renumber message type commands to be in order
// * send the low byte first

namespace IPRE.ScribblerBase
{
    internal class ScribblerHelper
    {

        private static int SetMessageReturnLength = 12;

        /// <summary>
        /// enumeration of Scribbler API commands
        /// </summary>
        public enum Commands
        {
            SOFT_RESET          = 33,

            GET_ALL             = 65,
            GET_ALL_BINARY      = 66,
            GET_LIGHT_LEFT      = 67,
            GET_LIGHT_CENTER    = 68,
            GET_LIGHT_RIGHT     = 69,
            GET_LIGHT_ALL       = 70,
            GET_IR_LEFT         = 71,
            GET_IR_RIGHT        = 72,
            GET_IR_ALL          = 73,
            GET_LINE_LEFT       = 74,
            GET_LINE_RIGHT      = 75,
            GET_LINE_ALL        = 76,
            GET_STATE           = 77,
            GET_NAME            = 78,
            GET_STALL           = 79,
            GET_INFO            = 80,
            GET_DATA            = 81,

            SET_DATA            = 97,
            SET_ECHO_MODE       = 98,
            SET_LED_LEFT_ON     = 99,
            SET_LED_LEFT_OFF    = 100,
            SET_LED_CENTER_ON   = 101,
            SET_LED_CENTER_OFF  = 102,
            SET_LED_RIGHT_ON    = 103,
            SET_LED_RIGHT_OFF   = 104,
            SET_LED_ALL_ON      = 105,
            SET_LED_ALL_OFF     = 106,
            SET_LED_ALL         = 107,
            SET_MOTORS_OFF      = 108,
            SET_MOTORS          = 109,
            SET_NAME            = 110,
            SET_LOUD            = 111,
            SET_QUIET           = 112,
            SET_SPEAKER         = 113,
            SET_SPEAKER_2       = 114,

            // NOTE: If you add or modify these commands, you will also need to modify the functions below
            // as well as ScribblerResponseHandler in Scribbler.cs
        }

        /// <summary>
        /// Gives the number of bytes a message should have when coming back from scribbler.
        /// This includes the command type byte.
        /// </summary>
        /// <param name="cmd"></param>
        /// <returns></returns>
        public int ReturnSize(Commands cmd)
        {
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
                    return 37;
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
                case Commands.SOFT_RESET:
                    return 0;
                    break;
                default:
                    Console.WriteLine("Command missmatch");
                    return 1;
                    break;
            }
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
}
