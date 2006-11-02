//-----------------------------------------------------------------------
//  
//     
//      Ben Axelrod 08/28/2006
//
//-----------------------------------------------------------------------

using System;

using System.Text;
using System.IO;
using System.IO.Ports;
using System.Diagnostics;
using System.Globalization;
using System.Collections.Generic;
using System.Runtime.Serialization;

using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.DsspServiceBase;


namespace IPRE.ScribblerBase
{
    internal class ScribblerCom
    {
        private const int outMessageSize = 9;

        private SerialPort _serialPort = null;

        private const int _baudRate = 38400;

        private ScribblerHelper helper = new ScribblerHelper();

        /// <summary>
        /// after this number of milliseconds without seeing data, the scribbler will send its 'find me' message
        /// </summary>
        private const int noCommsTimeout = 1000;
        
        /// <summary>
        /// the scribbler's 'find me' message must contain this string
        /// </summary>
        private const string characteristicString = "IPRE";

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
        internal bool Open(int comPort)
        {
            if (_serialPort != null)
                Close();

            //debug
            //Console.WriteLine("Open: " + comPort);

            _serialPort = new SerialPort("COM" + comPort.ToString(System.Globalization.NumberFormatInfo.InvariantInfo), _baudRate);
            _serialPort.Encoding = Encoding.Default;
            _serialPort.Parity = Parity.None;
            _serialPort.DataBits = 8;
            _serialPort.StopBits = StopBits.One;
            _serialPort.WriteTimeout = 2000;
            try
            {
                string name = TrySerialPort(_serialPort.PortName);
                if (name != null)
                {
                    _serialPort.Open();
                    foundRobotName = name;
                    openedComPort = comPort;
                }
                else
                    return false;
            }
            catch
            {
                Console.WriteLine("Invalid Serial Port.");
                return false;
            }
            return true;
        }


        /// <summary>
        /// Attempt to read our data from a serial port.
        /// </summary>
        /// <param name="spName">Serial port name</param>
        /// <returns>name of robot attached to port</returns>
        private string TrySerialPort(string spName)
        {
            //DEBUG
            //Console.WriteLine("TrySerialPort: " + spName);

            SerialPort p = null;
            string robotname = null;

            try
            {
                p = new SerialPort(spName, _baudRate, Parity.None, 8, StopBits.One);
                p.Handshake = Handshake.None;
                p.Encoding = Encoding.ASCII;
                p.Open();

                string s = p.ReadExisting();

                if (s.Length == 0)
                {
                    System.Threading.Thread.Sleep((int)(noCommsTimeout * 1.5));
                    s = p.ReadExisting(); //try again
                }

                if (s.Length == 0)
                {
                    return null;
                }

                // we are receiving data.
                
                int index = s.IndexOf(characteristicString);
                int end = -1;
                if (index >= 0)
                    end = s.IndexOf("\0", index);
                bool invalidData = UnrecognizedData(s);

                //not a Scribbler robot
                if (invalidData || (index < 0) || (end <= index) )
                {
                    return null;
                }

                robotname = s.Substring(index + characteristicString.Length, end - index - characteristicString.Length);
                Console.WriteLine("TrySerialPort found: " + robotname);
            }
            catch { }
            finally
            {
                if (p != null && p.IsOpen)
                    p.Close();
                p.Dispose();
            }

            return robotname;
        }


        /// <summary>
        /// Attempt to find and open a Scribbler on any serial port.
        /// <returns>True if a robot unit was found</returns>
        /// </summary>
        public bool FindRobot(string robotname)
        {
            //debug
            //Console.WriteLine("FindRobot: " + robotname);

            //if there are multiple robots connected, add to list and prompt user
            //item0 = COM port name
            //item1 = robot name
            List<Tuple<String, String>> foundRobots = new List<Tuple<string, string>>();

            foreach (string spName in SerialPort.GetPortNames())
            {
                string spName2 = spName;

                if (spName.Contains("?"))
                {
                    spName2 = spName.Substring(0, spName.Length - 1);
                    Console.WriteLine("Fixing name: "+ spName + " -> " + spName2);
                }

                Console.Write("Checking for robot on " + spName2 + ".  ");

                string tempName = TrySerialPort(spName2);

                if (tempName != null)
                {
                    Console.Write("Found robot \"" + tempName + "\"\n");

                    if (tempName == robotname)
                    {
                        return Open(int.Parse(spName2.Substring(3, spName2.Length - 3)));
                    }
                    else
                    {
                        Tuple<String, String> pair = new Tuple<string, string>(spName2, tempName);
                        foundRobots.Add(pair);
                    }
                }
                else
                    Console.Write("\n");
            }

            //only one robot found. so connect
            if (foundRobots.Count == 1)
            {
                return Open(int.Parse(foundRobots[0].Item0.Substring(3, foundRobots[0].Item0.Length - 3)));
            }
            //many robots found. prompt user to connect
            else if (foundRobots.Count > 0)
            {
                bool selected = false;
                while (!selected)
                {
                    Console.Write("Which robot would you like to connect to:");
                    string connect = Console.ReadLine();
                    connect = connect.ToLower();

                    foreach (Tuple<string, string> tup in foundRobots)
                    {
                        if (connect == tup.Item1.ToLower())
                            return Open(int.Parse(tup.Item0.Substring(3, tup.Item0.Length - 3)));
                        try
                        {
                            int portNum = int.Parse(tup.Item0.Substring(3, tup.Item0.Length - 3));
                            return Open(portNum);
                        }
                        catch { }
                    }
                }
            }

            //no robots found
            return false;
        }

        
        void serialPort_ErrorReceived(object sender, SerialErrorReceivedEventArgs e)
        {
            Console.WriteLine("serialPort_ErrorReceived: "+e);
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
            byte[] buffer = new byte[outMessageSize];

            if (buffer != null)
            {

                int ix = 0;

                //buffer = cmd.ToByteArray();

                buffer[ix++] = cmd.CommandType;

                if (cmd.Data != null && cmd.Data.Length > 0)
                    foreach (byte b in cmd.Data)
                        buffer[ix++] = b;

                //fill to standard size
                while (ix < outMessageSize)
                    buffer[ix++] = 0;

                //DEBUG
                //Console.Write("\nSent: ");
                //foreach (byte b in buffer)
                //{
                //    if (b != 0)
                //        Console.Write(b + " ");
                //    else
                //        Console.Write("` ");
                //}
                //Console.Write("\n");
                //DEBUG

                // When requesting a response, clear the inbound buffer 
                if (_serialPort.BytesToRead > 0)
                    _serialPort.DiscardInBuffer();

                try
                {
                    _serialPort.Write(buffer, 0, ix);
                }
                catch
                {
                    Console.WriteLine("Serial Port Timeout.  Turn on Scribbler.");
                    //throw new IOException();
                }

                echo = GetEcho(buffer);
                response = GetCommandResponse(helper.ReturnSize((ScribblerHelper.Commands)cmd.CommandType));
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
            byte[] inBuff = new byte[outMessageSize];
            ScribblerResponse response = null;
            int ixOutBuff = 0;
            try
            {
                while (ixOutBuff < outMessageSize)
                {
                    byte[] temp = new byte[1];
                    _serialPort.Read(temp, 0, 1); //get 1 byte
                    if (temp[0] == outBuff[ixOutBuff])
                    {
                        inBuff[ixOutBuff] = temp[0];
                        ixOutBuff++;
                    }
                }

                response = new ScribblerResponse();
                response.Data = (byte[])inBuff.Clone();

                //DEBUG
                //Console.Write("Echo: ");
                //foreach (byte b in response.Data)
                //{
                //    if (b != 0)
                //        Console.Write(b + " ");
                //    else
                //        Console.Write("` ");
                //}
                //Console.Write("\n");
                //DEBUG
            }
            catch (Exception ex)
            {
                Console.WriteLine("GetCommandResponse Exception: " + ex);
            }
            return response;
        }


        /// <summary>
        /// Read Serial Port for a number of bytes and put into ScribblerResponse
        /// </summary>
        /// <param name="nBytes">number of bytes to read (includes the command type byte)</param>
        /// <returns>ScribblerResponse</returns>
        private ScribblerResponse GetCommandResponse(int nBytes)
        {
            byte[] inBuff = new byte[nBytes];
            ScribblerResponse response = null;
            int read = 0;
            try
            {
                while (read < nBytes)
                {
                    
                    int canread;
                    while ((canread = _serialPort.BytesToRead) == 0) ; //spin
                    int needtoread = nBytes - read;
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
                    //DEBUG
                    //foreach (byte b in inBuff)
                    //{
                    //    if (b != 0)
                    //        Console.Write(b + " ");
                    //    else
                    //        Console.Write("` ");
                    //}
                    //Console.Write("\n");
                    //DEBUG
                }

                response = new ScribblerResponse(nBytes-1);
                response.CommandType = inBuff[inBuff.Length-1];
                for (int i = 0; i < inBuff.Length - 1; i++)
                    response.Data[i] = inBuff[i];

                //DEBUG
                //Console.Write("Got: ");
                //foreach (byte b in response.Data)
                //{
                //    if (b != 0)
                //        Console.Write(b + " ");
                //    else
                //        Console.Write("` ");
                //}
                //Console.Write("\n");
                //DEBUG
            }
            catch (Exception ex)
            {
                Console.WriteLine("GetCommandResponse Exception: " + ex);
            }
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

     


    }


  




}
