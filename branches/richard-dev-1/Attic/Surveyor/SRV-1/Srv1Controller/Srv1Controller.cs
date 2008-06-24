using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.IO.Ports;
using System.Drawing;
using System.Timers;

namespace SharpLogic.Robotics.Surveyor.Srv1
{
    /// <summary>
    /// Provides control commands to communicate with a Surveyor SRV-1 robot.
    /// </summary>
    public class Srv1Controller : IDisposable
    {
        #region Private Fields
        private const int _responseTimeout = 500;//ms
        private const int _engineTimerInterval = 50;//ms
        private ImageResolution _currentImageResolution = ImageResolution.DefaultResolution;

        private static Srv1Controller _singletonController;
        private static string _comPort = String.Empty;
        private static SerialPort _port;

        private int _responseBufferLength;
        private byte[] _responseBuffer;
        private float _rightMotorPower;
        private float _leftMotorPower;        
        private bool _motorValueChanged;
        private Timer _engineTimer;
        #endregion

        #region Constructors
        /// <summary>
        /// Constructs a new instance of the SurveyorController class.
        /// </summary>
        private Srv1Controller()
        {
            // Initialize engine timer.
            this._engineTimer = new Timer();
            this._engineTimer.Interval = _engineTimerInterval;
            this._engineTimer.AutoReset = false;
            this._engineTimer.Elapsed += this.OnTimedRunEngine;

            // The longest response packet will be 10009 bytes, which is if an image response
            // provides a compressed image with 320x240 pixels and a data portion of 9999.
            this._responseBuffer = new byte[16384];//=2^14
            this._responseBufferLength = 0;

            // Initialize the port
            SerialPort port = Port;
        }
        #endregion

        #region Public Properties

        /// <summary>
        /// Indicates whether the connection with robot is active.
        /// </summary>
        /// <remarks>This property makes a synchronous request to the robot to test the connection.</remarks>
        public bool IsConnected
        {
            get
            {
                if (_port.IsOpen)
                {
                    // Making a request/response to ensure the connection is actually working.
                    try 
                    {
                        string version = this.FirmwareVersion;
                        return true;
                    }
                    catch (TimeoutException) { return false; }
                }
                return false;
            }
        }

        /// <summary>
        /// Gets the name of the serial port for communications.
        /// </summary>
        public static string PortName
        {
            get { return _comPort; }
            set
            {
                if (value == null) { throw new ArgumentNullException("value", "PortName cannot be null."); }
                if (_comPort != value)
                {
                    _comPort = value;
                    DisposeSerialPort();
                }
            }
        }

        private SerialPort Port
        {
            get
            {
                lock (this._lockPort)
                {
                    if (_port == null)
                    {
                        _port = new SerialPort(PortName, 115200, Parity.None, 8, StopBits.One);
                        _port.Encoding = Encoding.ASCII;
                        _port.ReadTimeout = _responseTimeout;
                        this.Connect();
                    }
                }
                return _port;
            }
        }
        private Object _lockPort = new Object();

        /// <summary>
        /// Gets or sets the number of milliseconds before a time-out occurs when a response is expected from robot.
        /// </summary>
        /// <exception cref="ArgumentOutOfRangeException">Thrown if the specified time-out value is less than zero.</exception>
        public int ResponseTimeout
        {
            get {return Port.ReadTimeout; }
            set
            {
                // The SerialPort.ReadTimeout property supports -1 as SerialPort.InfiniteTimeout, but we don't.
                if (value < 0) { throw new ArgumentOutOfRangeException("value", "The specified time-out value is less than zero."); }
                Port.ReadTimeout = value;
            }
        }

        /// <summary>
        /// Gets the version info of the firmware installed on the robot controller.
        /// </summary>
        /// <returns>A string representing the firmware version.</returns>
        /// <exception cref="ApplicationException">Thrown if the firmware version cannot be retrieved.</exception>
        public string FirmwareVersion
        {
            get
            {
                SerialResponse serialResponse = this.SendCommand(SerialCommands.ReadFirmwareVersionInfo);

                if (!(serialResponse.Data is string))
                {
                    throw new ApplicationException("Read firmware version request did not return a valid string.");
                }

                return serialResponse.Data as string;
            }
        }

        /// <summary>
        /// Gets a DateTime representation of the firmware version.
        /// This property makes a synchronous call to the FirmwareVersion property.
        /// </summary>
        /// <exception cref="ApplicationException">Thrown if the version date and time cannot be retrieved.</exception>
        public DateTime FirmwareDate
        {
            get
            {
                try
                {
                    return DateTime.ParseExact(this.FirmwareVersion, "MMM dd yyyy - HH:mm:ss", new System.Globalization.CultureInfo("en-US"));
                }
                catch (Exception ex)
                {
                    throw new ApplicationException("There was an error retrieving the version date and time.", ex);
                }
            }
        }

        public float RightMotorPower
        {
            get { return this._rightMotorPower; }
            set
            {
                if (this._rightMotorPower != value)
                {
                    this._rightMotorPower = value;
                    this.OnMotorValueChanged();
                }
            }
        }

        public float LeftMotorPower
        {
            get { return this._leftMotorPower; }
            set
            {
                if (this._leftMotorPower != value)
                {
                    this._leftMotorPower = value;
                    this.OnMotorValueChanged();
                }
            }
        }

        #endregion

        #region Public Methods
        /// <summary>
        /// Creates, connects and returns a singleton instance of the SurveyorController class.
        /// </summary>
        public static Srv1Controller Srv1Factory()
        {
            lock (_lockFactory)
            {
                if (String.IsNullOrEmpty(PortName))
                {
                    throw new Exception("You must set the PortName before calling the factory method.");
                }
                if (_singletonController == null)
                {
                    _singletonController = new Srv1Controller();
                }
            }

            return _singletonController;
        }
        private static Object _lockFactory = new Object();
        
        /// <summary>
        /// Opens the port and establishes the connection with robot.
        /// </summary>
        /// <remarks>
        /// This method allows the exceptions that may occur to bubble up from the port being used.
        /// If the underlying port is already open, the current connection is used.
        /// </remarks>
        /// <exception cref="IOException">Thrown if an error occurs when establishing the connection.</exception>
        private void Connect()
        {
            if (!_port.IsOpen)
            {
                _port.Open();
            }
            if (!this.IsConnected)
            {
                throw new IOException("There was an error establishing the connection.");
            }
        }

        /// <summary>
        /// Closes the port and terminates the connection with robot.
        /// </summary>
        private void Disconnect()
        {
            if (_port.IsOpen)
            {
                _port.Close();
            }
        }

        /// <summary>
        /// Directs the motor control.
        /// </summary>
        /// <param name="leftSpeed">Engine power for the left motor. Use a negative number to rotate backwards.</param>
        /// <param name="rightSpeed">Engine power for the right motor. Use a negative number to rotate backwards.</param>
        /// <param name="duration">Duration in scale of 10 milliseconds. Duration of 0 is infinite.</param>
        /// <returns>True if a success response is received from the robot.</returns>
        private void RunEngine(sbyte leftSpeed, sbyte rightSpeed, byte duration)
        {
            this.SendCommand(SerialCommands.MotorControl, (byte)leftSpeed, (byte)rightSpeed, duration);
        }

        /// <summary>
        /// Directs the motor control and runs indefinitely.
        /// </summary>
        /// <param name="leftSpeed">Percentage of engine power for the left motor. Use a negative number to go backwards.</param>
        /// <param name="rightSpeed">Percentage of engine power for the right motor. Use a negative number to go backwards.</param>
        /// <returns>True if a success response is received from the robot.</returns>
        private void RunEngine(sbyte leftSpeed, sbyte rightSpeed)
        {
            this.RunEngine(leftSpeed, rightSpeed, (byte)0);
        }

        /// <summary>
        /// Directs the motor control.
        /// </summary>
        /// <param name="leftSpeed">A floating point number from -1 to 1 for the left motor speed.</param>
        /// <param name="rightSpeed">A floating point number from -1 to 1 for the right motor speed.</param>
        /// <remarks>The sign in the leftSpeed and rightSpeed parameters defines the direction and the
        /// magnitude defines the engine power. If the specified number goes beyond the [-1, +1] range
        /// it will be assumed as -1 if negative or 1 if positive.</remarks>
        /// <param name="duration">Duration in milliseconds. duration of 0 is infinite.</param>
        private void RunEngine(float leftSpeed, float rightSpeed, int duration)
        {
            if (duration < 0)
            {
                throw new ArgumentException("Duration cannot be a negative number.", "duration");
            }
            // Ensure leftSpeed and rightSpeed are in the [-1, 1] range.
            leftSpeed = Math.Max(-1, Math.Min(leftSpeed, 1));
            rightSpeed = Math.Max(-1, Math.Min(rightSpeed, 1));

            sbyte leftPower = (sbyte)(leftSpeed * 0x7F);
            sbyte rightPower = (sbyte)(rightSpeed * 0x7F);

            if (duration / 10 <= 0xFF)
            {
                // Ensure 0 (infinite) is not passed if the duration is not zero.
                if (0 < duration && duration < 10) { duration = 10; }

                this.RunEngine(leftPower, rightPower, (byte)(duration / 10));
            }
            else
            {
                this.RunEngine(leftPower, rightPower);
                // Wait for the duration time to elapse...
                //TODO: This is not thread safe.
                System.Threading.Thread.Sleep(duration);

                this.Stop();
            }
        }

        private void OnMotorValueChanged()
        {
            this._motorValueChanged = true;
            this._engineTimer.Enabled = true;
        }

        private void OnTimedRunEngine(object source, ElapsedEventArgs e)
        {
            if (this._motorValueChanged)
            {
                if (this._leftMotorPower == 0 && this._rightMotorPower == 0)
                {
                    this.Stop();
                }
                else
                {
                    this.RunEngine(this._leftMotorPower, this._rightMotorPower, 0);
                }
                this._motorValueChanged = false;
            }
            this._engineTimer.Start();
        }

        /// <summary>
        /// Drift robot to the left.
        /// </summary>
        public void DriftLeft()
        {
            this.SendCommand(SerialCommands.RobotDriftLeft);
        }

        /// <summary>
        /// Drift robot to the right.
        /// </summary>
        public void DriftRight()
        {
            this.SendCommand(SerialCommands.RobotDriftRight);
        }

        /// <summary>
        /// Drive robot forward.
        /// </summary>
        public void DriveForward()
        {
            this.SendCommand(SerialCommands.RobotDriveForward);
        }

        /// <summary>
        /// Drive robot backward.
        /// </summary>
        public void DriveBackward()
        {
            this.SendCommand(SerialCommands.RobotDriveBack);
        }

        /// <summary>
        /// Drive robot forward to the left.
        /// </summary>
        public void DriveLeft()
        {
            this.SendCommand(SerialCommands.RobotDriveLeft);
        }

        /// <summary>
        /// Drive robot forward to the right.
        /// </summary>
        public void DriveRight()
        {
            this.SendCommand(SerialCommands.RobotDriveRight);
        }

        /// <summary>
        /// Drive robot backward to the left.
        /// </summary>
        public void DriveBackLeft()
        {
            this.SendCommand(SerialCommands.RobotBackLeft);
        }

        /// <summary>
        /// Drive robot backward to the right.
        /// </summary>
        public void DriveBackRight()
        {
            this.SendCommand(SerialCommands.RobotBackRight);
        }

        /// <summary>
        /// Sets all motor powers to zero.
        /// </summary>
        public void Stop()
        {
            this.SendCommand(SerialCommands.RobotStop);
        }

        /// <summary>
        /// Rotates 20 degrees to the left.
        /// </summary>
        public void RotateLeft_20Degrees()
        {
            this.SendCommand(SerialCommands.RobotRotateLeft_20deg);
        }

        /// <summary>
        /// Rotates 20 degrees to the right.
        /// </summary>
        public void RotateRight_20Degrees()
        {
            this.SendCommand(SerialCommands.RobotRotateRight_20deg);
        }

        /// <summary>
        /// Sets motor speed range to high.
        /// </summary>
        public void SetHighMotorSpeedRange()
        {
            this.SendCommand(SerialCommands.HighMotorSpeedRange);
        }

        /// <summary>
        /// Sets motor speed range to low.
        /// </summary>
        public void SetLowMotorSpeedRange()
        {
            this.SendCommand(SerialCommands.LowMotorSpeedRange);
        }

        /// <summary>
        /// Enables "FailSafe Mode" to avoid hitting obstacles (stop motors if no radio contact).
        /// </summary>
        public void EnableFailSafeMode()
        {
            this.SendCommand(SerialCommands.FailSafeModeOn);
        }

        /// <summary>
        /// Disables "FailSafe Mode". May hit obstacles.
        /// </summary>
        public void DisableFailSafeMode()
        {
            this.SendCommand(SerialCommands.FailSafeModeOff);
        }

        /// <summary>
        /// Enables "Wander Mode".
        /// </summary>
        public void EnableWanderMode()
        {
            this.SendCommand(SerialCommands.WanderModeOn);
        }

        /// <summary>
        /// Disables "Wander Mode".
        /// </summary>
        public void DisableWanderMode()
        {
            this.SendCommand(SerialCommands.WanderModeOff);
        }

        /// <summary>
        /// Enables "Wander Mode" without autonomous movement for analyzing Scan data.
        /// </summary>
        public void EnableWanderModeScanOnly()
        {
            this.SendCommand(SerialCommands.WanderModeScanOn);
        }

        /// <summary>
        /// View raw "Pixel Column Vector" data in "Wander Mode".
        /// </summary>
        /// <returns>An array of bytes representing 80 columns. Each column is assigned with a value
        /// of 0 to 64 that indicates the pixel position of the first non-floor pixel for that
        /// column (a value of 64 means there is no blockage).</returns>
        public byte[] Scan()
        {
            SerialResponse serialResponse = this.SendCommand(SerialCommands.Scan);

            if (!(serialResponse.Data is Byte[]))
            {
                throw new ApplicationException("Scan request did not return valid data.");
            }

            return serialResponse.Data as Byte[];            
        }

        /// <summary>
        /// Enables "Xmit IR Beacon" mode.
        /// </summary>
        public void EnableXmitIRBeaconMode()
        {
            this.SendCommand(SerialCommands.XmitIRBeaconModeOn);
        }

        /// <summary>
        /// Disables "Xmit IR Beacon" mode.
        /// </summary>
        public void DisableXmitIRBeaconMode()
        {
            this.SendCommand(SerialCommands.XmitIRBeaconModeOff);
        }

        /// <summary>
        /// Enables "Locate Beacon" mode.
        /// </summary>
        public void EnableLocateBeaconMode()
        {
            this.SendCommand(SerialCommands.LocateBeaconModeOn);
        }

        /// <summary>
        /// Disables "Locate Beacon" mode.
        /// </summary>
        public void DisableLocateBeaconMode()
        {
            this.SendCommand(SerialCommands.LocateBeaconModeOff);
        }

        /// <summary>
        /// Estimates the proximity on each side by emitting IR signals and counting the bounced bits. 
        /// </summary>
        /// <returns>An instance of BounceIRResponse containing the proximity info on each side of the robot.</returns>
        public BounceIRResponse GetProximity()
        {
            SerialResponse serialResponse = this.SendCommand(SerialCommands.BounceInfraRed);
            // The response will be in form "##BounceIR - aa bb cc dd\n".
            // aa bb cc dd are bounced bit counts for front, left, back, right.

            if (!(serialResponse.Data is BounceIRResponse))
            {
                throw new ApplicationException("BounceIR request did not return a valid BounceIRResponse");
            }

            return serialResponse.Data as BounceIRResponse;
        }

        /// <summary>
        /// Sets the camera capture resolution.
        /// </summary>
        /// <param name="resolution">A predefined image resolution to use when capturing images from the camera.</param>
        /// <exception cref="ArgumentException">Thrown if ImageResolution is set to a value other than one of the predefined resolutions.</exception>
        public void SetImageResolution(ImageResolutions resolution)
        {
            switch (resolution)
            {
                case ImageResolutions.Resolution80x64: this.SendCommand(SerialCommands.SetCaptureResolutionTo_80x64); break;
                case ImageResolutions.Resolution160x128: this.SendCommand(SerialCommands.SetCaptureResolutionTo_160x128); break;
                case ImageResolutions.Resolution320x240: this.SendCommand(SerialCommands.SetCaptureResolutionTo_320x240); break;
                default: throw new ArgumentException("Invalid resolution");
            }

            this._currentImageResolution = new ImageResolution(resolution);
        }

        /// <summary>
        /// Captures a compressed video frame from the camera.
        /// </summary>
        /// <returns>A Bitmap.</returns>
        public Bitmap CaptureImage()
        {
            SerialResponse serialResponse = this.SendCommand(SerialCommands.GrabCompressedVideoFrame);

            if (!(serialResponse.Data is Bitmap))
            {
                throw new ApplicationException("GrabFrame request did not return a valid Image");
            }

            return serialResponse.Data as Bitmap;
        }

        /// <summary>
        /// Captures a compressed video frame from the camera.
        /// This method will try querying the image for a number of times until a valid image is received
        /// or all the queries time out.
        /// </summary>
        /// <returns>A Bitmap.</returns>
        /// <exception cref="TimeoutException">Thrown if the capture image query times out for every try.</exception>
        public Bitmap ForceCaptureImage()
        {
            int minResponseTimeout = Math.Max(this.ResponseTimeout, this._currentImageResolution.Timeout / 2);
            int maxResponseTimeout = Math.Max(this._currentImageResolution.Timeout, minResponseTimeout);
            int numTries = 5;// should be > 1;
            int increaseTimeout = (maxResponseTimeout - minResponseTimeout) / (numTries - 1);
            this.ResponseTimeout = minResponseTimeout;

            Bitmap frame = null;
            for (int i = 0; i < numTries; i++)
            {
                try
                {
                    frame = this.CaptureImage();
                    break;
                }
                catch (TimeoutException)
                {
                    this.ResponseTimeout += increaseTimeout;
                }
            }

            // Revert ResponseTimeout to the default value.
            this.ResponseTimeout = _responseTimeout;

            if (frame == null)
            {
                throw new TimeoutException("Image capture timed out for " + numTries + " tries.");
            }

            // Set current image resolution from the captured image.
            ImageResolutions res = this._currentImageResolution.Resolution;
            switch (frame.Width)
            {
                case 80:
                    res = ImageResolutions.Resolution80x64;
                    break;
                case 160:
                    res = ImageResolutions.Resolution160x128;
                    break;
                case 320:
                    res = ImageResolutions.Resolution320x240;
                    break;
            }
            if (this._currentImageResolution.Resolution != res)
            {
                this._currentImageResolution = new ImageResolution(res);
            }

            return frame;
        }

        #endregion

        #region Private Methods

        /// <summary>
        /// Sends a command followed by command arguments to robot.
        /// </summary>
        /// <param name="command">A command to send.</param>
        /// <param name="args">An array of bytes specifying the arguments for the command.</param>
        /// <returns>An instance of SerialResponse containing the response data received from the robot.</returns>
        /// <exception cref="ApplicationException">Thrown if the serial response does not match serial command.</exception>
        private SerialResponse SendCommand(SerialCommands command, params byte[] args)
        {
            lock (this._lockCommand)
            {
                // Clear the port and response buffer.
                this._responseBufferLength = 0;
                if (Port.BytesToRead > 0)
                {
                    Port.ReadExisting();
                }

                // Send command followed by any arguments.
                Port.Write(new byte[] { (byte)command }, 0, 1);
                if (args != null && args.Length > 0) { Port.Write(args, 0, args.Length); }

                // Wait to receive a complete response or until timeout.
                SerialResponse serialResponse = this.ReadResponse();

                if (serialResponse.SerialCommand != command)
                {
                    throw new ApplicationException("Serial response did not match serial command.");
                }

                return serialResponse;
            }
        }

        private Object _lockCommand = new Object();

        /// <summary>
        /// Waits for response data to fully load in the buffer and returns the response data.
        /// </summary>
        /// <remarks>
        /// By convention all commands from the host are single byte ASCII characters,
        /// and all commands receive an acknowledgment from the robot, which is either
        /// a '#' character followed by the command, or '##' for variable length responses.
        /// </remarks>
        /// <param name="command">The command for which a response is expected.</param>
        /// <returns>An instance of SerialResponse containing the response data received from the robot.</returns>
        /// <exception cref="TimeoutException">Thrown if the response data times out before being retrieved.</exception>
        /// <exception cref="ApplicationException">Thrown if the response buffer overflows.</exception>
        private SerialResponse ReadResponse()
        {
            const int msInterval = 100;
            int waitEnd = Environment.TickCount + this.ResponseTimeout;
            this._responseBufferLength = 0;

            // Load data into response buffer.
            while (true)
            {
                if (Port.BytesToRead > 0)
                {
                    int maxBytesToRead = Math.Min(this._responseBuffer.Length - this._responseBufferLength, Port.BytesToRead);

                    for (int lcv = 0; lcv < maxBytesToRead; lcv++)
                    {
                        // Read one byte at a time.
                        this._responseBuffer[this._responseBufferLength++] = (byte)Port.ReadByte();
                    }

                    // Parse the response data received so far.
                    SerialResponse serialResponse = this.ParseSerialResponse();
                    if (serialResponse != null)
                    {
                        return serialResponse;
                    }

                    // Check the received packet won't overflow in the response buffer.
                    if (this._responseBufferLength == this._responseBuffer.Length)
                    {
                        throw new ApplicationException("Response buffer overflowed.");
                    }
                }

                if (Environment.TickCount > waitEnd)
                {
                    throw new TimeoutException("Response timed out");
                }

                System.Threading.Thread.Sleep(msInterval);
            }
        }

        /// <summary>
        /// Parses the available response data in the response buffer and returns a SerialResponse object, or returns null if the response data is incomplete.
        /// </summary>
        /// <returns>An instance of SerialResponse containing the response data received from the robot or null if the response data could not be parsed.</returns>
        private SerialResponse ParseSerialResponse()
        {
            if (this._responseBufferLength < 2)
            {
                return null;
            }

            if (this._responseBuffer[0] != '#')
            {
                throw new IOException("Invalid response received (does not begin with '#').");
            }

            switch ((SerialCommands) this._responseBuffer[1])
            {
                // Single byte response
                case SerialCommands.MotorControl:
                case SerialCommands.RobotDriftLeft: 
                case SerialCommands.RobotDriveForward:
                case SerialCommands.RobotDriftRight:
                case SerialCommands.RobotDriveRight:
                case SerialCommands.RobotStop:
                case SerialCommands.RobotDriveLeft:
                case SerialCommands.RobotBackLeft:
                case SerialCommands.RobotDriveBack:
                case SerialCommands.RobotBackRight:
                case SerialCommands.RobotRotateLeft_20deg:
                case SerialCommands.RobotRotateRight_20deg:
                case SerialCommands.HighMotorSpeedRange:
                case SerialCommands.LowMotorSpeedRange:
                case SerialCommands.SetCaptureResolutionTo_160x128:
                case SerialCommands.SetCaptureResolutionTo_320x240:
                case SerialCommands.SetCaptureResolutionTo_80x64:
                case SerialCommands.WanderModeOn:
                case SerialCommands.WanderModeOff:
                case SerialCommands.WanderModeScanOn:
                case SerialCommands.FailSafeModeOn:
                case SerialCommands.FailSafeModeOff:
                case SerialCommands.XmitIRBeaconModeOn:
                case SerialCommands.XmitIRBeaconModeOff:
                case SerialCommands.LocateBeaconModeOff:
                    return new SerialResponse((SerialCommands) this._responseBuffer[1]);

                // Variable length response
                case (SerialCommands) '#':
                    if (this._responseBufferLength < 3)
                    {
                        return null;
                    }

                    string response;
                    Regex regex;
                    Match match;

                    switch ((SerialCommands) this._responseBuffer[2])
                    {
                        case SerialCommands.Scan: // "##Scan - 0001020304..79\n"
                            int columns = 80;
                            int scanDataLength = 9 + (2 * columns) + 1;
                            if (this._responseBufferLength < scanDataLength) { return null; }

                            if (this._responseBuffer[scanDataLength - 1] == (byte)'\n')
                            {
                                string pixelColumnStr = Port.Encoding.GetString(this._responseBuffer, 9, this._responseBufferLength - 10)
                                    .Replace(" ", "0");
                                byte[] pixelColumnArray = new byte[columns];
                                for (int i = 0; i < pixelColumnArray.Length; i++)
                                {
                                    pixelColumnArray[i] = Convert.ToByte(pixelColumnStr.Substring(2*i, 2), 16);
                                }
                                return new SerialResponse(SerialCommands.Scan, pixelColumnArray);
                            }
                            return null;

                        case SerialCommands.GrabCompressedVideoFrame:
                            // Image
                            if (this._responseBufferLength < 10) { return null; }

                            response = Port.Encoding.GetString(this._responseBuffer, 0, 6);
                            regex = new Regex(@"^##IMJ(\d)$");
                            match = regex.Match(response);
                            if (!match.Success) { throw new ApplicationException("Image request did not respond correctly"); }

                            int res = int.Parse(match.Groups[1].Value);

                            int dataLength = 
                                 (this._responseBuffer[6]) + 
                                 (this._responseBuffer[7] << 8) + 
                                 (this._responseBuffer[8] << 16) +
                                 (this._responseBuffer[9] << 24);

                            if (this._responseBufferLength < (dataLength + 10)) { return null; }

                            using (MemoryStream memoryStream = new MemoryStream(this._responseBuffer, 10, this._responseBufferLength - 10))
                            {
                                return new SerialResponse(SerialCommands.GrabCompressedVideoFrame, new Bitmap(memoryStream));
                            }

                        case SerialCommands.ReadFirmwareVersionInfo:
                            if (this._responseBufferLength < 12) { return null; }

                            for (int lcv = 12; lcv < this._responseBufferLength; lcv++)
                            {
                                if (this._responseBuffer[lcv] == (byte)'\n')
                                {
                                    return new SerialResponse(SerialCommands.ReadFirmwareVersionInfo, 
                                        Port.Encoding.GetString(this._responseBuffer, 12, this._responseBufferLength - 13));
                                }
                            }
                            return null;

                        case SerialCommands.BounceInfraRed:
                            if (this._responseBufferLength < 41) { return null; }

                            response = Port.Encoding.GetString(this._responseBuffer, 0, this._responseBufferLength);

                            match = Regex.Match(response, @"^##BounceIR -\s+([\dA-F]+)\s+([\dA-F]+)\s+([\dA-F]+)\s+([\dA-F]+)\n$");

                            if (match.Groups.Count != 5) { return null; }

                            int front = Convert.ToInt32(match.Groups[1].Value, 16);
                            int left = Convert.ToInt32(match.Groups[2].Value, 16);
                            int back = Convert.ToInt32(match.Groups[3].Value, 16);
                            int right = Convert.ToInt32(match.Groups[4].Value, 16);

                            return new SerialResponse(SerialCommands.BounceInfraRed, new BounceIRResponse(front, left, back, right));

                        case SerialCommands.LocateBeaconModeOn:
                            // Enable "Locate Beacon"

                            // Can't test this yet since we don't have a beacon and it seems to not respond until it finds one.

                            return null;

                        default: throw new ApplicationException("Unknown response");
                    }
                
                default: throw new ApplicationException("Unknown response");
            }
        }

        private bool IsSingleByteCommand(SerialCommands command)
        {
            switch (command)
            {
                case SerialCommands.MotorControl:
                case SerialCommands.LocateBeaconModeOn:
                case SerialCommands.SwarmModeCommand:
                    return false;

                default:
                    return true;
            }
        }
        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            DisposeSerialPort();
            if (this._engineTimer != null)
            {
                this._engineTimer.Dispose();
                this._engineTimer = null;
            }
        }
        #endregion

        private static void DisposeSerialPort()
        {
            if (_port != null)
            {
                if (_port.IsOpen)
                {
                    _port.Close();
                }

                _port.Dispose();

                // Setting port to null to prevent multiple disposals.
                _port = null;
            }
        }
    }
}
