//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoComm.cs $ $Revision$
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.IO.Ports;
using Microsoft.Ccr.Core;
using System.IO;
using Microsoft.Robotics.Services.LegoNxt.Helper;
using System.Diagnostics;
using Microsoft.Dss.ServiceModel.DsspServiceBase;


namespace Microsoft.Robotics.Services.LegoNxt
{
    internal class LegoConnection
    {
        private const int serialBufferLength = 640;
        private LegoNxtService _legoService;
        byte[] buffer;

        SerialPort serialPort = new System.IO.Ports.SerialPort();

        /// <summary>
        /// Pass in a pointer to the service base so we can do logging.
        /// </summary>
        /// <param name="serviceBase"></param>
        public LegoConnection(LegoNxtService legoService)
        {
            _legoService = legoService;
        }

        /// <summary>
        /// Open a serial port.
        /// </summary>
        /// <param name="comPort"></param>
        /// <param name="baudRate"></param>
        /// <returns>A Ccr Port for receiving serial port data</returns>
        internal bool Open(int comPort, int baudRate)
        {

            if (buffer == null)
                buffer = new byte[serialBufferLength];

            if (baudRate < 1200)
                baudRate = 115200;

            Close();

            string portName = "COM" + comPort.ToString(System.Globalization.NumberFormatInfo.InvariantInfo);
            if (serialPort == null)
            {
                serialPort = new SerialPort(portName, baudRate);
            }
            else
            {
                serialPort.PortName = portName;
                serialPort.BaudRate = baudRate;
            }
            serialPort.Encoding = Encoding.Default;
            serialPort.Parity = Parity.None;
            serialPort.DataBits = 8;
            serialPort.StopBits = StopBits.One;
            serialPort.WriteTimeout = 2000;
            serialPort.ReadTimeout = 2000;

            try
            {
                serialPort.Open();
            }
            catch
            {
                _legoService.LogConsoleError("Invalid Serial Port.");
                ShowLegoHelp(_legoService);
                return false;
            }
            return true;
        }

        /// <summary>
        /// DO NOT USE THIS COMMAND DIRECTLY.
        /// In LegoNxt.cs, post a message to _legoCommWithAckPort or _legoCommNoAckPort.
        /// </summary>
        /// <param name="cmd"></param>
        /// <returns></returns>
        internal LegoResponse SendCommand(LegoCommand cmd)
        {
            LegoResponse response = null;
            if (buffer != null && serialPort != null && serialPort.IsOpen)
            {
                // Add the bluetooth packet length 
                int ixBuffer = 0;
                // dataLength = commandType + commandCode + Data
                int packetLength = (2 + ((cmd.Data == null) ? 0 : cmd.Data.Length));
                buffer[ixBuffer++] = (byte)(packetLength % 256);
                buffer[ixBuffer++] = (byte)(packetLength / 256);

                // Add the bluetooth header bytes to get an actual packet count
                packetLength += 2;  

                buffer[ixBuffer++] = (byte)(cmd.CommandType + ((!cmd.RequireResponse) ? 0x80 : 0x00));
                buffer[ixBuffer++] = (byte)cmd.LegoCommandCode;

                // When requesting a response, clear the inbound buffer 
                if (cmd.RequireResponse && serialPort.BytesToRead > 0)
                    serialPort.DiscardInBuffer();

                int dataLength = (cmd.Data == null) ? 0 : cmd.Data.Length;
                int ixData = 0;
                while (ixData < dataLength || ixBuffer > 0)
                {
                    // Copy up to serialBufferLength from Data
                    while (ixBuffer < serialBufferLength && ixData < dataLength)
                        buffer[ixBuffer++] = cmd.Data[ixData++];

                    try
                    {
                        serialPort.Write(buffer, 0, ixBuffer);
                        ixBuffer = 0;
                    }
                    catch
                    {
                        _legoService.LogConsoleError("Serial Port Timeout.");
                        ShowLegoHelp(_legoService);
                        return null;
                    }
                }

                response = GetCommandResponse(cmd);
            }
            return response;
        }
        
        /// <summary>
        /// Display LEGO Help to the console
        /// </summary>
        public static void ShowLegoHelp(LegoNxtService legoService)
        {
            legoService.LogConsoleError("Please make sure the LEGO NXT brick is turned on, connected\r\n" 
                + "    via Bluetooth, and configured to the proper port.\r\n" 
                + "    If you are having difficulty connecting for the first time,\r\n"
                + "    please refer to the LEGO NXT readme found in your installation\r\n" 
                + "    directory at ...\\Samples\\Platforms\\LEGO\\NXT\\readme.htm\r\n");
        }

        /// <summary>
        /// Read Serial Port for a LEGO response
        /// </summary>
        /// <param name="cmd"></param>
        private LegoResponse GetCommandResponse(LegoCommand cmd)
        {
            if (cmd != null && cmd.RequireResponse && serialPort != null && serialPort.IsOpen)
            {
                int wait = 0;
                while (serialPort.BytesToRead == 0 && wait < 10)
                {
                    wait++;
                    if (wait < 10)
                        System.Threading.Thread.Sleep(100);
                    else
                    {
                        if (serialPort.CtsHolding && serialPort.DsrHolding)
                        {
                            // We might be connected in the wrong direction
                            _legoService.LogConsoleError("Have you connected from the LEGO NXT to the PC?");
                            ShowLegoHelp(_legoService);
                        }
                    }
                }

                try
                {
                    int btLength = serialPort.ReadByte() + (serialPort.ReadByte() * 256);
                    byte commandType = (byte)serialPort.ReadByte();
                    LegoHelper.LegoCommandCode command = (LegoHelper.LegoCommandCode)serialPort.ReadByte();
                    btLength -= 2;

                    if (cmd.LegoCommandCode != command)
                    {
                        // Garbled response.  
                        // Wait for remaining bytes
                        // return an error
                        System.Threading.Thread.Sleep(500);
                        serialPort.DiscardInBuffer();
                        IOException ex1 = new IOException("Garbled data received from LEGO NXT.  LEGO response code does not match LEGO command.");
                        return new LegoResponseException(cmd, ex1);
                    }

                    LegoResponse legoReceive = new LegoResponse(commandType, command, btLength);

                    if (btLength > 0)
                    {
                        legoReceive.Data = new byte[btLength];
                        serialPort.Read(legoReceive.Data, 0, btLength);
                    }

                    // Is this a valid starting type?
                    if (commandType != 0x00 && commandType != 0x01
                        && commandType != 0x02 && commandType != 0x80
                        && commandType != 0x81)
                    {
                        _legoService.LogConsoleInfo(string.Format("Invalid LEGO response command: {0}", commandType));
                    }

                    return legoReceive;

                }
                catch (ArgumentException ex)
                {
                    if (ex.Message == "Offset and length were out of bounds for the array or count is greater than the number of elements from index to the end of the source collection.")
                    {
                        _legoService.LogConsoleError("A connection error occured which may be caused by an invalid Baud Rate");
                        IOException ex3 = new IOException("Invalid Baud Rate");
                        return new LegoResponseException(cmd, ex3);
                    }
                    else
                    {
                        _legoService.LogConsoleError("A connection error occured while accessing the LEGO NXT serial port: " + ex.Message);
                        return new LegoResponseException(cmd, ex);
                    }
                }
                catch (TimeoutException ex)
                {
                    // Ignore timeouts for now.
                    _legoService.LogConsoleInfo("Timeout reading from LEGO NXT brick");
                    return new LegoResponseException(cmd, ex);
                }
                catch (IOException ex)
                {
                    _legoService.LogConsoleError(string.Format("Error reading from the serial port in CommBase(): {0}", ex.Message));
                    return new LegoResponseException(cmd, ex);
                }
                catch (Exception ex)
                {
                    _legoService.LogConsoleError(string.Format("Error reading from the serial port in CommBase(): {0}", ex.Message));
                    return new LegoResponseException(cmd, ex);
                }
            }

            LegoResponse response = new LegoResponse(2, cmd.LegoCommandCode, 1);

            if (cmd == null || cmd.RequireResponse)
            {
                response.ErrorCode = LegoErrorCode.UnknownStatus;

                if (cmd == null)
                    _legoService.LogConsoleError("Invalid LEGO Command: null");
                else if (serialPort == null || !serialPort.IsOpen)
                    _legoService.LogConsoleWarning(string.Format("Unable to retrieve response from LEGO command {0} because the serial port is not open.", cmd.LegoCommandCode));
            }

            return response;
        }


        /// <summary>
        /// Close the connection to a serial port.
        /// </summary>
        public void Close()
        {
            if (serialPort != null)
            {
                if (serialPort.IsOpen)
                {
                    serialPort.Close();
                }
            }
        }

        /// <summary>
        /// True when the serial port connection to the LEGO is open.
        /// </summary>
        public bool IsOpen
        {
            get { return serialPort != null && serialPort.IsOpen; }
        }


    }


    internal class LegoDataPort : PortSet<LegoResponse, SensorNotification, Exception> { }


}
