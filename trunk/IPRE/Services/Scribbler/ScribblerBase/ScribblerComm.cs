// Copyright (c) Microsoft Corporation.  All rights reserved.

//-----------------------------------------------------------------------
//  
//     
//      Ben Axelrod 08/28/2006
//
//-----------------------------------------------------------------------

//#define DEBUG
#undef DEBUG

using System;

using System.Text;
using System.IO;
using System.IO.Ports;
using System.Diagnostics;
using System.Globalization;
using System.Collections.Generic;
using Microsoft.Dss.Core.Attributes;

using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System.Threading;


namespace Myro.Services.Scribbler.ScribblerBase
{
    internal class ScribblerCom
    {
        private SerialPort _serialPort = null;

        private const int _baudRate = 38400;

        /// <summary>
        /// after this number of milliseconds without seeing data, the scribbler will send its 'find me' message
        /// </summary>
        private const int noCommsTimeout = 1000;

        /// <summary>
        /// After this number of milliseconds while waiting for known data to arrive, 
        /// the service will give up.
        /// <remarks>NOTE: The timer gets reset after every packet received.
        /// Messages to and from scribbler take about 70 ms.
        /// Keep in mind the base rate at which the service asks for refreshed sensor data (250 ms)</remarks>
        /// </summary>
        //private const int ReadTimeOut = 120;

        /// <summary>
        /// the scribbler's 'find me' message must contain this string
        /// </summary>
        //private const string characteristicString = "IPRE";
        private const string characteristicString = "Scribbler";

        //internal ScribblerDataPort ScribblerComInboundPort = null;


        //these are just to transfer back to the main scribbler service
        public string foundRobotName;
        public int openedComPort;

        /// <summary>
        /// Open a serial port.
        /// </summary>
        /// <param name="comPort"></param>
        /// <param name="baudRate"></param>
        /// <returns>A Ccr Port for receiving serial port data</returns>
        internal void Open(int comPort)
        {
            if (_serialPort != null)
                Close();

            //debug
#if DEBUG
            Console.WriteLine("Opening com port: " + comPort);
#endif

            _serialPort = new SerialPort("COM" + comPort.ToString(System.Globalization.NumberFormatInfo.InvariantInfo), _baudRate);
            _serialPort.Encoding = Encoding.Default;
            _serialPort.Parity = Parity.None;
            _serialPort.DataBits = 8;
            _serialPort.StopBits = StopBits.One;
            _serialPort.ReadTimeout = 500;
            _serialPort.WriteTimeout = 500;

            //try
            //{
            string name = TrySerialPort(_serialPort.PortName);

            System.Threading.Thread.Sleep(500); //give the BT device some time between closing and opening of the port
            _serialPort.Open();
            foundRobotName = name;
            openedComPort = comPort;

            //}
            //            catch (Exception ex)
            //            {
            //                Console.WriteLine("Invalid Serial Port.");

            //#if DEBUG
            //                Console.WriteLine("Open caught exception: " + ex);
            //                throw new Exception("TrySerialPort caught exception", ex);
            //#endif

            //                return false;
            //            }
        }


        /// <summary>
        /// Attempt to read our data from a serial port.
        /// </summary>
        /// <param name="spName">Serial port name</param>
        /// <returns>name of robot attached to port</returns>
        private string TrySerialPort(string spName)
        {
#if DEBUG
            Console.WriteLine("Trying Serial Port: " + spName); //DEBUG
#endif

            SerialPort p = null;
            string robotname = null;

            try
            {
                p = new SerialPort(spName, _baudRate, Parity.None, 8, StopBits.One);
                p.Handshake = Handshake.None;
                p.Encoding = Encoding.ASCII;
                p.Open();
                _serialPort = p;

#if DEBUG
                Console.WriteLine("reading once"); //DEBUG
#endif

                // Send the GETINFO string
                ScribblerResponse srp = SendCommand(new ScribblerCommand(ScribblerHelper.Commands.GET_INFO));

                // GTEMP: Send twice - some problem with dongle
                srp = SendCommand(new ScribblerCommand(ScribblerHelper.Commands.GET_INFO));

                UTF8Encoding enc = new UTF8Encoding();
                string s = enc.GetString(srp.Data);

                if (s.Length == 0)
                {
                    System.Threading.Thread.Sleep((int)(noCommsTimeout * 1.5));
#if DEBUG
                    Console.WriteLine("reading again"); //DEBUG
#endif
                    s = p.ReadExisting(); //try again
                }

                if (s.Length == 0)
                {
#if DEBUG
                    Console.WriteLine("length == 0"); //DEBUG
#endif
                    throw new ScribblerProtocolException("Could not interpret Scribbler info, probably not a Scribbler robot.");
                }

                // we are receiving data.
#if DEBUG
                Console.WriteLine("we are receiving data."); //DEBUG
                Console.WriteLine("received: \"" + s + "\"");
#endif

                int index = s.IndexOf(characteristicString);
                //not a Scribbler robot
                if (index < 0)
                {
#if DEBUG
                    Console.WriteLine("not a Scribbler robot."); //DEBUG
#endif
                    _serialPort = null;
                    throw new ScribblerProtocolException("Could not find magic string, not a Scribbler robot.");
                }

                // Sending Echo off command
                SendCommand(new ScribblerCommand(ScribblerHelper.Commands.SET_ECHO_MODE, (byte)1, (byte)1));

                // Now get robotname
                srp = SendCommand(new ScribblerCommand(ScribblerHelper.Commands.GET_NAME));
                enc = new UTF8Encoding();
                robotname = enc.GetString(srp.Data);
                if (robotname.Length == 0)
                {
#if DEBUG
                    Console.WriteLine("Cannot get name"); //DEBUG
#endif
                    robotname = "Noname";
                }

#if DEBUG
                Console.WriteLine("TrySerialPort found: " + robotname); //DEBUG
#endif
            }
            catch (Exception)
            {
                throw;
            }
            //            catch (Exception ex)
            //            {
            //                throw;
            //#if DEBUG
            //                Console.WriteLine("TrySerialPort caught exception: " + ex);
            //                //throw new Exception("TrySerialPort caught exception", ex);
            //#endif
            //            }
            finally
            {
                if (p != null && p.IsOpen)
                    p.Close();
                p.Dispose();
            }

            return robotname;
        }


        ///// <summary>
        ///// Attempt to find and open a Scribbler on any serial port.
        ///// <returns>True if a robot unit was found</returns>
        ///// </summary>
        //public bool FindRobot(string robotname)
        //{
        //    //debug
        //    //Console.WriteLine("FindRobot: " + robotname);

        //    //if there are multiple robots connected, add to list and prompt user
        //    //item0 = COM port name
        //    //item1 = robot name
        //    List<Tuple<String, String>> foundRobots = new List<Tuple<string, string>>();

        //    foreach (string spName in SerialPort.GetPortNames())
        //    {
        //        Console.WriteLine("Checking " + spName);
        //        string spName2 = FixComPortName(spName);

        //        Console.Write("Checking for robot on " + spName2 + ".  ");

        //        string tempName = TrySerialPort(spName2);

        //        if (tempName != null)
        //        {
        //            Console.Write("Found robot \"" + tempName + "\"\n");

        //            if (tempName == robotname)
        //            {
        //                return Open(int.Parse(spName2.Substring(3, spName2.Length - 3)));
        //            }
        //            else
        //            {
        //                Tuple<String, String> pair = new Tuple<string, string>(spName2, tempName);
        //                foundRobots.Add(pair);
        //            }
        //        }
        //        else
        //            Console.Write("\n");
        //    }

        //    //only one robot found. so connect
        //    if (foundRobots.Count == 1)
        //    {
        //        return Open(int.Parse(foundRobots[0].Item0.Substring(3, foundRobots[0].Item0.Length - 3)));
        //    }
        //    //many robots found. prompt user to connect
        //    else if (foundRobots.Count > 0)
        //    {
        //        Console.WriteLine("*** Found multiple robots: ***");
        //        foreach (Tuple<string, string> tup in foundRobots)
        //        {
        //            Console.WriteLine("   Robot \"" + tup.Item1 + "\" on " + tup.Item0);
        //        }


        //        bool selected = false;
        //        while (!selected)
        //        {
        //            Console.WriteLine("Which robot would you like to connect to?");
        //            Console.Write("Enter the robot's name or COM port:");

        //            string connect = Console.ReadLine();
        //            connect = connect.ToUpper();          //read robot name and standardize

        //            int connectPort = -1;
        //            int.TryParse(connect, out connectPort); //convert to int if possible

        //            //if user entered number
        //            if (connectPort > 0)
        //                return Open(connectPort);

        //            //NOTE: Item0 = COMport, Item1 = RobotName
        //            foreach (Tuple<string, string> tup in foundRobots)
        //            {
        //                int foundPort = -1;
        //                int.TryParse(tup.Item0.Substring(3, tup.Item0.Length - 3), out foundPort); //convert to int if possible

        //                //if user entered name
        //                if (connect == tup.Item1.ToUpper())
        //                    return Open(foundPort);
        //                else if (connect == tup.Item0.ToUpper()) //if user entered COMport
        //                    return Open(foundPort);
        //            }

        //        }
        //    }

        //    //no robots found
        //    return false;
        //}


        void serialPort_ErrorReceived(object sender, SerialErrorReceivedEventArgs e)
        {
            Console.WriteLine("serialPort_ErrorReceived: " + e);
            //throw new IOException();
        }


        /// <summary>
        /// DO NOT USE THIS COMMAND DIRECTLY.
        /// In Scribbler.cs, post a message to _scribblerComPort
        /// </summary>
        /// <param name="cmd"></param>
        /// <returns></returns>
        internal ScribblerResponse SendCommand(ScribblerCommand cmd)
        {
            ScribblerResponse echo = null;
            ScribblerResponse response = null;
            int outMessageSize = cmd.Data.Length + 1;
            byte[] buffer = new byte[outMessageSize];

            if (buffer != null)
            {

                int ix = 0;

                //buffer = cmd.ToByteArray();

                buffer[ix++] = cmd.CommandType;

                // Changed this so it doesn't copy entire command (Fluke commands are shorter than 8 bytes)
                int len = Math.Min(cmd.Data.Length, (outMessageSize - 1));
                if (cmd.Data != null && cmd.Data.Length > 0)
                    Array.Copy(cmd.Data, 0, buffer, 1, len);
                ix += len;
                //foreach (byte b in cmd.Data)
                //    buffer[ix++] = b;


                //fill to standard size
                while (ix < outMessageSize)
                    buffer[ix++] = 0;


#if DEBUG
                Console.Write("\nSent: ");
                foreach (byte b in buffer)
                {
                    if (b != 0)
                        Console.Write(b + " ");
                    else
                        Console.Write("` ");
                }
                Console.Write("\n");
#endif


                //try
                //{
                // When requesting a response, clear the inbound buffer 
                //Console.WriteLine(_serialPort.BytesToRead + " bytes left over");
                while (_serialPort.BytesToRead > 0)
                    _serialPort.DiscardInBuffer();

                //Console.WriteLine(((ScribblerHelper.Commands)cmd.CommandType).ToString());
                //Console.WriteLine("Command: " + cmd.Data.Length);
                //Console.WriteLine("Response: " + cmd.ResponseLength);
                //Console.WriteLine("Echo: " + cmd.HasEcho);

                _serialPort.Write(buffer, 0, ix);
                //}
                //catch
                //{
                //Console.WriteLine("Serial Port Timeout.  Lost connection with Scribbler.");
                //throw new IOException();
                //}


                if (cmd.HasEcho)
                    echo = GetEcho(buffer);

                response = GetCommandResponse(cmd);
            }
            return response;
        }



        /// <summary>
        /// Read Serial Port for echo
        /// </summary>
        /// <param name="outBuff">The outbound message to match</param>
        /// <returns>ScribblerResponse</returns>
        private ScribblerResponse GetEcho(byte[] outBuff)
        {
            byte[] inBuff = new byte[outBuff.Length];
            ScribblerResponse response = null;
            //int ixOutBuff = 0;
            //DateTime lastbytetime = DateTime.Now;
            //try
            //{

            // Read the whole echo in one chunk, with 500ms timeout
            int count = 0;
            while (_serialPort.BytesToRead < outBuff.Length)
            {
                if (++count > 50)
                    throw new TimeoutException("Timed out waiting for command echo of " + outBuff.Length + " bytes");
                Thread.Sleep(10); //spin
            }

            _serialPort.Read(inBuff, 0, outBuff.Length);
            for (int i = 0; i < outBuff.Length; i++)
                if (inBuff[i] != outBuff[i])
                    Console.WriteLine("Echo mismatch");

            //while (ixOutBuff < echoSize) // && Compare(DateTime.Now, lastbytetime) < ReadTimeOut)
            //{
            //    byte[] temp = new byte[1];
            //    _serialPort.Read(temp, 0, 1); //get 1 byte
            //    if (temp[0] == outBuff[ixOutBuff])
            //    {
            //        inBuff[ixOutBuff] = temp[0];
            //        ixOutBuff++;
            //        lastbytetime = DateTime.Now;
            //    }
            //    else
            //    {
            //        Console.WriteLine("Echo missmatch");
            //        break;
            //    }
            //}

            response = new ScribblerResponse();
            response.Data = inBuff;

#if DEBUG
                Console.Write("Echo: ");
                foreach (byte b in response.Data)
                {
                    if (b != 0)
                        Console.Write(b + " ");
                    else
                        Console.Write("` ");
                }
                Console.Write("\n");
#endif
            //}
            //catch (Exception ex)
            //{
            //    Console.WriteLine("GetCommandResponse Exception: " + ex);
            //    //throw;
            //}
            return response;
        }

        /// <summary>
        /// compares the two times and returns number of milliseconds between the two.
        /// </summary>
        /// <param name="now">the larger time</param>
        /// <param name="then"></param>
        /// <returns>delta milliseconds</returns>
        private int Compare(DateTime now, DateTime then)
        {
            return (now.Second - then.Second) * 1000 + (now.Millisecond - then.Millisecond);
        }

        /// <summary>
        /// Read Serial Port for a number of bytes and put into ScribblerResponse
        /// </summary>
        /// <param name="nBytes">number of bytes to read (includes the command type byte)</param>
        /// <returns>ScribblerResponse</returns>
        private ScribblerResponse GetCommandResponse(ScribblerCommand cmd)
        {
            int nBytes = cmd.ResponseLength;
            bool cmdEcho = cmd.HasEcho;

            //Console.WriteLine("GetCommandResponse: creating buffer");
            byte[] inBuff = new Byte[Math.Abs(nBytes)];

            //Console.WriteLine("Check 1");
            ScribblerResponse response = null;
            //Console.WriteLine("Check 2");
            int read = 0;
            bool error = false, done = false;

            // Set the default EOM character to a linefeed
            if (cmd.EndMarker1 == null)
            {
                cmd.EndMarker1 = 0x0A;
                cmd.EndMarker2 = null;
            }

            while (read < Math.Abs(nBytes) && !done)
            {
                int canread;
                int count = 0;
                while (((canread = _serialPort.BytesToRead) == 0) && count++ < 100)
                    Thread.Sleep(20); //spin 

                if (canread == 0)
                    break;

                //Console.WriteLine("Spun for " + (count*10) + " ms, got chunk of " + canread);

                if (nBytes < 0)
                {
                    //if (cmd.CommandType == (byte)ScribblerHelper.Commands.GET_JPEG_COLOR_SCAN)
                    //{
                    //    Console.WriteLine("JPEG");
                    //}
                    //Console.WriteLine("Reading variable length (buffer size " + inBuff.Length + ")");
                    for (int i = 0; i < canread; i++)
                    {
                        _serialPort.Read(inBuff, read++, 1);

                        // NOTE: This is a hack to get the header length from the first
                        // two bytes of the return from GET_JPEG_HEADER.
                        if (read == 2 &&
                            (cmd.CommandType == (byte)ScribblerHelper.Commands.GET_JPEG_COLOR_HEADER ||
                            cmd.CommandType == (byte)ScribblerHelper.Commands.GET_JPEG_GRAY_HEADER))
                        {
                            nBytes = (int)inBuff[0] + ((int)inBuff[1] << 8) + 2;
                            //Console.WriteLine("Looking for JPEG header of " + nBytes + " bytes");
                        }
                        else if (nBytes < 0) // Have to check this again because of the JPEG hack, changed nBytes
                        {
                            //Console.WriteLine("  Got " + inBuff[read - 1] + " at " + (read - 1));
                            if (cmd.EndMarker2 == null)
                            {
                                if (inBuff[read - 1] == cmd.EndMarker1)
                                    done = true;
                            }
                            else if (read >= 2)
                            {
                                if (inBuff[read - 2] == cmd.EndMarker1 && inBuff[read - 1] == cmd.EndMarker2)
                                    done = true;
                            }
                        }
                    }
                }
                else
                {
                    int needtoread = nBytes - read;
                    //Console.WriteLine("Reading fixed length of " + needtoread + ", buffer " + nBytes);
                    if (canread > needtoread)
                    {
                        _serialPort.Read(inBuff, read, needtoread);
                        read += needtoread;
                    }
                    else
                    {
                        _serialPort.Read(inBuff, read, canread);
                        read += canread;
                    }
                }
            }

            //Console.WriteLine("GetCommandResponse: " + _serialPort.BytesToRead + " left over");

            if (read < nBytes)
                throw new ScribblerProtocolException("Command response of " + read + " was too short: " + (ScribblerHelper.Commands)cmd.CommandType);

            int dataBytes = (cmdEcho ? read - 1 : read);
            response = new ScribblerResponse(Math.Max(0, dataBytes));
            response.CommandType = (cmdEcho ? inBuff[inBuff.Length - 1] : (byte)0);
            Array.Copy(inBuff, response.Data, dataBytes);

#if DEBUG
                    Console.Write("Got: ");
                    foreach (byte b in response.Data)
                    {
                        if (b != 0)
                            Console.Write(b + " ");
                        else
                            Console.Write("` ");
                    }
                    Console.Write("\n");
#endif
            return response;
        }



        /// <summary>
        /// Close the connection to a serial port.
        /// </summary>
        public void Close()
        {
            if (_serialPort != null)
            {
                if (_serialPort.IsOpen)
                    _serialPort.Close();
                _serialPort = null;
            }
        }



        /// <summary>
        /// Search serial port data for binary characters.  
        /// Unrecognized data indicates that we are 
        /// operating in binary mode or receiving data at 
        /// the wrong baud rate.
        /// </summary>
        /// <param name="data"></param>
        /// <returns></returns>
        private static bool UnrecognizedData(string data)
        {
            foreach (char c in data)
            {
                if ((c < 32 || c > 126) && (c != 0))
                {
                    Console.WriteLine("unrecognized: " + c);
                    return true;
                }
            }
            return false;
        }

        private static string FixComPortName(string name)
        {
            char[] tmp = name.ToCharArray();
            if (name[name.Length - 1] < 48 || name[name.Length - 1] > 57)
            {
                //Console.WriteLine("Fixing name: " + name + " -> " + name.Substring(0, name.Length - 1)); //DEBUG
                return name.Substring(0, name.Length - 1);
            }
            return name;
        }


    }







}
