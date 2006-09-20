//-----------------------------------------------------------------------
//  
//     
//      Ben Axelrod 08/28/2006
//
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.IO.Ports;
using Microsoft.Ccr.Core;
using System.IO;
using System.Diagnostics;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System.Runtime.Serialization;


namespace IPRE.ScribblerBase
{
    internal class ScribblerComm
    {
        byte[] buffer;

        SerialPort serialPort = null;

        internal ScribblerDataPort ScribblerComInboundPort = null;

        /// <summary>
        /// Open a serial port.
        /// </summary>
        /// <param name="comPort"></param>
        /// <param name="baudRate"></param>
        /// <returns>A Ccr Port for receiving serial port data</returns>
        internal ScribblerDataPort Open(int comPort, int baudRate)
        {
            if (serialPort != null)
                Close();

            if (buffer == null)
                buffer = new byte[128];

            Console.WriteLine("Opening COM port: " + comPort + " with baud rate: " + baudRate);

            serialPort = new SerialPort("COM" + comPort.ToString(System.Globalization.NumberFormatInfo.InvariantInfo), baudRate);
            serialPort.Encoding = Encoding.Default;
            serialPort.Parity = Parity.None;
            serialPort.DataBits = 8;
            serialPort.StopBits = StopBits.One;
            //serialPort.WriteTimeout = 2000;
            serialPort.DataReceived += new SerialDataReceivedEventHandler(serialPort_DataReceived);
            serialPort.ErrorReceived += new SerialErrorReceivedEventHandler(serialPort_ErrorReceived);
            
            ScribblerComInboundPort = new ScribblerDataPort();
            
            try
            {
                serialPort.Open();
            }
            catch
            {
                Console.WriteLine("Invalid Serial Port.");
                throw new IOException();
            }
            return ScribblerComInboundPort;
        }

        void serialPort_ErrorReceived(object sender, SerialErrorReceivedEventArgs e)
        {
            throw new IOException();
        }

        public bool SendCommand(ScribblerCommand cmd)
        {
            if (buffer != null && cmd.Data != null)
            {
                int ix = 0;

                //Add packet length start byte
                buffer[ix++] = (byte)(cmd.Data.Length+1);

                if (cmd.Data != null && cmd.Data.Length > 0)
                    foreach (byte b in cmd.Data)
                        buffer[ix++] = b;

                // null terminate as stop byte for scribbler
                buffer[ix] = 0;

                try
                {
                    serialPort.Write(buffer, 0, ix+1);
                }
                catch
                {
                    throw new IOException();
                }

                return true;
            }
            return false;
        }

        /// <summary>
        /// Serial Port data event handler
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        void serialPort_DataReceived(object sender, SerialDataReceivedEventArgs e)
        {
            if (e.EventType == SerialData.Chars)
            {
                try
                {
                    while (serialPort.BytesToRead >= 2)
                    {
                        int messageLength = serialPort.ReadByte();

                        while ((messageLength > 5 || messageLength == 0) && serialPort.BytesToRead > 0)
                            messageLength = serialPort.ReadByte();

                        if (messageLength >= 2)
                        {
                            byte[] data = new byte[messageLength];

                            serialPort.Read(data, 0, messageLength);

                            //debug
                            foreach (byte b in data)
                            {
                                if (b != 0)
                                    Console.Write(b + " ");
                                else
                                    Console.Write("` ");
                            }
                            Console.Write("\n");

                            if (ScribblerHelper.IsSensorResponse(data[0]))
                            {
                                SensorNotification sensorMsg = new SensorNotification(data[0], data[1]);

                                ScribblerComInboundPort.Post(sensorMsg);
                            }
                            else
                            {
                                ScribblerCommand echo = new ScribblerCommand();
                                echo.Data = data;
                                ScribblerComInboundPort.Post(echo);
                            }

                        }

                    }
                }
                catch (ArgumentException ex)
                {
                    if (ex.Message == "Offset and length were out of bounds for the array or count is greater than the number of elements from index to the end of the source collection.")
                    {
                        Exception invalidBaudRate = new IOException();
                        System.Diagnostics.Debug.WriteLine("Invalid Baud Rate");
                        ScribblerComInboundPort.Post(invalidBaudRate);
                    }
                    else
                    {
                        Console.WriteLine("Error reading from the serial port in ComBase(): {0}", ex.Message);
                        System.Diagnostics.Debug.WriteLine("Error reading from the serial port in ComBase(): {0}", ex.Message);
                        ScribblerComInboundPort.Post(ex);
                    }
                }
                catch (TimeoutException ex)
                {
                    ScribblerComInboundPort.Post(ex);
                    // Ignore timeouts for now.
                }
                catch (IOException ex)
                {
                    Console.WriteLine("Error reading from the serial port in CommBase(): {0}", ex.Message);
                    ScribblerComInboundPort.Post(ex);

                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error reading from the serial port in CommBase(): {0}", ex.Message);
                    ScribblerComInboundPort.Post(ex);
                }
            }
        }


        /// <summary>
        /// Close the connection to a serial port.
        /// </summary>
        private void Close()
        {
            if (serialPort != null)
            {
                if (serialPort.IsOpen)
                {
                    serialPort.DataReceived -= new SerialDataReceivedEventHandler(serialPort_DataReceived);
                    serialPort.Close();
                }

                serialPort = null;
                ScribblerComInboundPort = null;
            }
        }


    }


  




}
