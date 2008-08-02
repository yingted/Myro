// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;
using Microsoft.Dss.Hosting;
using System.IO;
using System.Threading;
using System.Drawing;
using Myro.Utilities;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Ccr.Core;
using W3C.Soap;

using directory = Microsoft.Dss.Services.Directory.Proxy;
using scribbler = Myro.Services.Scribbler.ScribblerBase.Proxy;

namespace Myro
{
    /// <summary>
    /// The static Robot class provides the highest-level implementation of the
    /// Myro API.  This class is responsible for loading a robot configuration
    /// when init() is called, and starting the DSS environment, which in turn
    /// starts the robot services.  This class also is the high-level point of
    /// access for the Myro adapters, and stores a static instance of
    /// AdapterBank, which actually manages the adapters.
    /// </summary>
    public static class Robot
    {
        private static AdapterBank bank = null;
        private static AdapterToken<DriveAdapter> driveAdapter = null;
        private static AdapterToken<VectorAdapter> soundAdapter = null;
        private static AdapterToken<WebcamAdapter> webcamAdapter = null;
        private static AdapterToken<FlukeControlAdapter> controlAdapter = null;
        private static int httpPort;
        private static int dsspPort;
        private static ServiceInfoType brickService = null;

        /// <summary>
        /// This enumeration indicates the type of state change in a StateChangeEventArgs
        /// class.  The GUI uses this enumeration and event to change the connection
        /// status indicator.
        /// </summary>
        public enum RobotStateChange
        {
            NONE, CONNECTING, CONNECTING_FAILED, CONNECTING_SUCCEEDED, SHUTDOWN, SHUTDOWN_COMPLETE
        }
        /// <summary>
        /// This class contains a single member that indicates the type of state change in a StateChangeEventArgs
        /// class.  The GUI uses this enumeration and event to change the connection
        /// status indicator.
        /// </summary>
        public class RobotStateChangeEventArgs : EventArgs
        {
            public RobotStateChange StateChange;
        }
        /// <summary>
        /// This is the delegate type for a RobotStateChange handler.  The GUI
        /// uses this event to change the connection status indicator.
        /// </summary>
        /// <param name="e"></param>
        public delegate void RobotStateChangeEventHandler(RobotStateChangeEventArgs e);
        /// <summary>
        /// The GUI uses the RobotStateChange event
        /// uses this event to change the connection status indicator.
        /// </summary>
        /// <param name="e"></param>
        public static event RobotStateChangeEventHandler RobotStateChangeEvent;
        private static RobotStateChange _lastStateChange = RobotStateChange.NONE;
        /// <summary>
        /// This property makes the type of the last state change event available.
        /// </summary>
        public static RobotStateChange LastStateChange
        {
            get
            {
                return _lastStateChange;
            }
            private set
            {
                _lastStateChange = value;
                RobotStateChangeEvent.Invoke(new RobotStateChangeEventArgs() { StateChange = value });
            }
        }

        /// <summary>
        /// This property exposes the configuration for the currently-connected robot.
        /// </summary>
        public static MyroConfigFiles CurrentConfig { get; private set; }

        #region Startup and shutdown

        /// <summary>
        /// This version of Init is generally not called by the end-user.  It
        /// uses a configuation that is already found by a MyroConfigFinder class.
        /// </summary>
        /// <param name="config"></param>
        public static void Init(MyroConfigFiles config)
        {
            Init(config, null);
        }

        /// <summary>
        /// This is one of the init methods that is available in Python.  It 
        /// automatically does the right thing when given a base name such as
        /// "Scribbler", or "Create".
        /// </summary>
        /// <param name="baseName">The base name of the manifest and config file group.</param>
        public static void init(string baseName)
        {
            init(baseName, null);
        }

        /// <summary>
        /// This is one of the init methods that is available in Python.  It 
        /// automatically does the right thing when given a base name such as
        /// "Scribbler", or "Create".  It also allows you to specify a COM port,
        /// which currently only works with the Scribbler.
        /// </summary>
        /// <param name="baseName"></param>
        /// <param name="comPort"></param>
        public static void init(string baseName, string comPort)
        {
            Init(new MyroConfigFinder(Params.ConfigPath).FindFromBaseName(baseName), comPort);
        }

        /// <summary>
        /// This is another init method that is generally not called by the end-user.
        /// The purpose of the comPort argument is the same as with the string version,
        /// init(string baseName, string comPort).
        /// </summary>
        /// <param name="config"></param>
        /// <param name="comPort"></param>
        public static void Init(MyroConfigFiles config, string comPort)
        {
            bool isScribbler = config.BaseName.ToLower().Equals("scribbler");

            // If this is a Scribbler, wait for it to start
            if (isScribbler)
            {
                CurrentConfig = config;
                LastStateChange = RobotStateChange.CONNECTING;

                try
                {
                    // Start DSS if not started
                    if (bank == null)
                    {
                        startDSS(config);
                        createAdapters();
                    }
                    connectWaitForScribbler(comPort);
                }
                catch (Exception)
                {
                    LastStateChange = RobotStateChange.CONNECTING_FAILED;
                    CurrentConfig = null;
                    throw;
                }

                LastStateChange = RobotStateChange.CONNECTING_SUCCEEDED;
            }
            else
            {
                if (bank == null)
                {
                    CurrentConfig = config;
                    LastStateChange = RobotStateChange.CONNECTING;

                    try
                    {
                        startDSS(config);
                        createAdapters();
                    }
                    catch (Exception)
                    {
                        LastStateChange = RobotStateChange.CONNECTING_FAILED;
                        CurrentConfig = null;
                        throw;
                    }

                    LastStateChange = RobotStateChange.CONNECTING_SUCCEEDED;
                }
                else
                {
                    throw new MyroInitException("Myro is already initialized");
                }
            }
        }

        /// <summary>
        /// This is an internal helper method that starts the DSS environment,
        /// using the manifest file specified in the MyroConfigFiles argument.
        /// </summary>
        /// <param name="config"></param>
        private static void startDSS(MyroConfigFiles config)
        {
            string manifestFile = Path.Combine(Path.Combine(Params.ConfigPath, config.BaseName + ".manifest"), config.BaseName + ".manifest.xml");
            Robot.httpPort = config.MyroConfiguration.HttpPort;
            Robot.dsspPort = config.MyroConfiguration.DsspPort;
            FileAttributes att = File.GetAttributes(manifestFile);
            if ((att & (FileAttributes.Device | FileAttributes.Directory | FileAttributes.Offline)) != 0)
                throw new IOException("Manifest file is not a normal file");
            Console.Write("Starting DSS environment...");
            DssEnvironment.Initialize(httpPort, dsspPort, "file://" + manifestFile);
            Console.WriteLine("Done");
        }

        /// <summary>
        /// This is an internal helper method that creates the AdapterBank, and
        /// caches the pre-known adapters.
        /// </summary>
        private static void createAdapters()
        {
            bank = new AdapterBank(new List<IAdapterFactory>() {
                new Myro.Adapters.DriveAdapterFactory(),
                new Myro.Adapters.VectorAdapterFactory(),
                new Myro.Adapters.WebcamAdapterFactory(),
                new Myro.Adapters.FlukeControlAdapterFactory()
            });
            driveAdapter = bank.GetAdapterSpec<DriveAdapter>("drive");
            soundAdapter = bank.GetAdapterSpec<VectorAdapter>("tonegen");
            webcamAdapter = bank.GetAdapterSpec<WebcamAdapter>("webcam");
            controlAdapter = bank.GetAdapterSpec<FlukeControlAdapter>("flukecontrol");
        }

        /// <summary>
        /// This is a helper mehtod that connects the Scribbler on the specified
        /// COM port.  It waits for the connection to complete, and re-throws any
        /// exceptions generated by the Scribbler service.
        /// </summary>
        /// <param name="comPort"></param>
        private static void connectWaitForScribbler(string comPort)
        {
            ManualResetEvent evt = new ManualResetEvent(false);
            waitForService(new ServiceInfoType(scribbler.Contract.Identifier),
                delegate(ServiceInfoType info) { brickService = info; evt.Set(); });
            evt.WaitOne(Params.DefaultRecieveTimeout, false);
            if (brickService == null)
                throw new MyroInitException("Could not find Scribbler service");
            var scribPort = DssEnvironment.ServiceForwarder<scribbler.ScribblerOperations>(new Uri(brickService.Service));
            DispatcherQueue queue = new DispatcherQueue("init", new Dispatcher());
            try
            {
                if (comPort != null)
                {
                    int comNumber;
                    if (comPort.ToLower().StartsWith("com"))
                        comNumber = Int32.Parse(comPort.Substring(3));
                    else
                        throw new MyroInitException("COM port string must be of the format com2, com5, etc.");
                    RSUtils.ReceiveSync(queue, scribPort.Replace(new scribbler.ScribblerState()
                    {
                        ComPort = comNumber
                    }), Params.DefaultRecieveTimeout);
                }
                DssEnvironment.LogInfo("calling reconnect...");
                RSUtils.ReceiveSync(queue, scribPort.Reconnect(), Params.DefaultRecieveTimeout);
                DssEnvironment.LogInfo("reconnect returned");
            }
            catch (Exception)
            {
                throw;
            }
            finally
            {
                queue.Dispose();
            }
        }

        /// <summary>
        /// Shut down the DSS node and MSRDS services.  This method waits
        /// for everything to shut down before returning.
        /// </summary>
        public static void Shutdown()
        {
            LastStateChange = RobotStateChange.SHUTDOWN;

            try
            {
                if (bank != null)
                {
                    bank.Dispose();
                }
            }
            catch (Exception) { }

            try
            {
                DssEnvironment.Shutdown();
                DssEnvironment.WaitForShutdown();
            }
            catch (Exception) { }

            LastStateChange = RobotStateChange.SHUTDOWN_COMPLETE;
        }

        /// <summary>
        /// Helper method that waits for a service to start and runs a handler when it does.
        /// This subscribes to and queries the DSS directory.
        /// </summary>
        /// <param name="serviceInfo"></param>
        /// <param name="handler"></param>
        private static void waitForService(ServiceInfoType serviceInfo, Handler<ServiceInfoType> handler)
        {
            var dirPort = DssEnvironment.ServiceForwarder<directory.DirectoryPort>(new Uri("dssp.tcp://localhost/directory"));
            var notPort = new directory.DirectoryPort();

            // Keep this flag to only run the handler once
            bool ranHandler = false;
            Object lockObj = new Object();

            // Subscribe first, then query, to make sure we don't miss the service registering itself
            DssEnvironment.LogInfo("Subscribing to directory");
            Arbiter.Activate(DssEnvironment.TaskQueue, Arbiter.Choice(
                dirPort.Subscribe(
                    new directory.SubscribeRequest()
                    {
                        QueryRecordList = new List<ServiceInfoType>() { serviceInfo },
                        NotificationCount = 1
                    },
                    notPort,
                    typeof(directory.Insert)),
                delegate(SubscribeResponseType r)
                {
                    DssEnvironment.LogInfo("Subscribed to directory!");

                    // Run handler if the service starts and we get the subscription notification
                    DssEnvironment.LogInfo("Activating subscription receive");
                    Arbiter.Activate(DssEnvironment.TaskQueue, Arbiter.Receive<directory.Insert>(false, notPort,
                        delegate(directory.Insert ins)
                        {
                            DssEnvironment.LogInfo("Got subscription notification!");
                            lock (lockObj)
                            {
                                if (ranHandler == false)
                                {
                                    new Thread(new ThreadStart(delegate() { handler.Invoke(ins.Body.Record); })).Start();
                                    ranHandler = true;
                                }
                            }
                        }));
                },
                delegate(Fault f)
                {
                    lock (lockObj)
                    {
                        if (ranHandler == false)
                        {
                            new Thread(new ThreadStart(delegate() { handler.Invoke(null); })).Start();
                            ranHandler = true;
                        }
                    }
                    DssEnvironment.LogError("Fault received while subscribing to directory: " + Strings.FromFault(f));
                }));

            // Query directory, run handler if the service is already started
            DssEnvironment.LogInfo("Querying directory");
            Arbiter.Activate(DssEnvironment.TaskQueue, Arbiter.Choice(
                dirPort.Query(new directory.QueryRequest() { QueryRecord = serviceInfo }),
                delegate(directory.QueryResponse r)
                {
                    DssEnvironment.LogInfo("Queried directory!");
                    lock (lockObj)
                    {
                        if (ranHandler == false && r.RecordList.Length > 0)
                        {
                            new Thread(new ThreadStart(delegate() { handler.Invoke(r.RecordList[0]); })).Start();
                            ranHandler = true;
                        }
                    }
                },
                delegate(Fault f)
                {
                    //lock (lockObj)
                    //{
                    //    if (ranHandler == false)
                    //    {
                    //        new Thread(new ThreadStart(delegate() { handler.Invoke(null); })).Start();
                    //        ranHandler = true;
                    //    }
                    //}
                    DssEnvironment.LogError("Fault received while querying directory: " + Strings.FromFault(f));
                }));

        }
        #endregion

        #region Fluke commands

        public static string getName()
        { checkNull(controlAdapter); return controlAdapter.Adapter.GetName(); }

        public static void setName(string name)
        { checkNull(controlAdapter); controlAdapter.Adapter.SetName(name); }

        public static void setIRPower(byte power)
        { checkNull(controlAdapter); controlAdapter.Adapter.SetIRPower(power); }

        #endregion

        #region Drive commands
        public static void forward(double power)
        { motors(power, power); }

        public static void forward(double power, double seconds)
        { motors(power, power, seconds); }

        public static void backward(double power)
        { motors(-power, -power); }

        public static void backward(double power, double seconds)
        { motors(-power, -power, seconds); }

        public static void turnLeft(double power)
        { motors(-power, power); }

        public static void turnLeft(double power, double seconds)
        { motors(-power, power, seconds); }

        public static void turnRight(double power)
        { motors(power, -power); }

        public static void turnRight(double power, double seconds)
        { motors(power, -power, seconds); }

        public static void stop()
        { motors(0.0, 0.0); }

        private static double lastTranslate = 0.0;
        private static double lastRotate = 0.0;

        public static void translate(double amount)
        { move((lastTranslate = amount), lastRotate); }

        public static void rotate(double amount)
        { move(lastTranslate, (lastRotate = amount)); }

        public static void move(double translate, double rotate)
        { motors(translate - rotate, translate + rotate); }

        public static void motors(double leftPower, double rightPower, double seconds)
        {
            motors(leftPower, rightPower);
            if (seconds > 0)
            {
                Thread.Sleep((int)(seconds * 1000));
                stop();
            }
        }

        public static void motors(double leftPower, double rightPower)
        {
            checkNull(driveAdapter);
            driveAdapter.Adapter.SetMotors(
                Math.Max(-1.0, Math.Min(1.0, leftPower)),
                Math.Max(-1.0, Math.Min(1.0, rightPower)));
        }

        //public static void turn(string direction, double power)
        //{
        //    int dir = checkDirection(direction);
        //    if (dir == -1)
        //        turnLeft(power);
        //    else if (dir == 1)
        //        turnRight(power);
        //}

        //public static void TurnFor(string direction, double power, double seconds)
        //{
        //    int dir = checkDirection(direction);
        //    if (dir == -1)
        //        turnLeft(power, seconds);
        //    else if (dir == 1)
        //        turnRight(power, seconds);
        //}

        //private static int checkDirection(string direction)
        //{
        //    if (direction == null)
        //        throw new ArgumentException("Direction settings when turning must be either 'left' or 'right'");
        //    direction = direction.ToLower().Trim();
        //    if (direction.Equals("left"))
        //        return -1;
        //    if (direction.Equals("right"))
        //        return 1;
        //    throw new ArgumentException("Direction settings when turning must be either 'left' or 'right'");
        //}
        #endregion

        #region Sound commands
        public static object ReadSong(string fileName)
        {
            throw new NotImplementedException();
        }

        public static object MakeSong(string text)
        {
            throw new NotImplementedException();
        }

        public static void SaveSong(string text, string fileName)
        {
            throw new NotImplementedException();
        }

        public static void PlaySong(object song)
        {
            throw new NotImplementedException();
        }

        public static void PlaySong(string text)
        {
            PlaySong(MakeSong(text));
        }

        public static void beep()
        { beep(0.75); }

        public static void beep(double duration)
        { beep(duration, 800.0); }

        public static void beep(double duration, double frequency)
        { beep(duration, frequency, 0.0); }

        public static void beep(double duration, double frequency1, double frequency2)
        {
            checkNull(soundAdapter);
            soundAdapter.Adapter.Set(
                new List<int>() { 0, 1, 2 },
                new List<double>() { frequency1, frequency2, duration });
        }

        public static void setLoud(bool loud)
        {
            checkNull(soundAdapter);
            soundAdapter.Adapter.Set(3, loud ? 1.0 : 0.0);
        }
        #endregion

        #region Sensor commands

        public static double[] Get(string name)
        { checkNull(bank); return bank.GetAdapterSpec<VectorAdapter>(name.ToLower()).Adapter.GetAllElements().ToArray(); }

        public static double Get(string name, int index)
        { checkNull(bank); return bank.GetAdapterSpec<VectorAdapter>(name.ToLower()).Adapter.Get(index); }

        public static double Get(string name, string tag)
        { checkNull(bank); return bank.GetAdapterSpec<VectorAdapter>(name.ToLower()).Adapter.Get(tag.ToLower()); }

        public static string[] GetNames(string name)
        { checkNull(bank); return bank.GetAdapterSpec<VectorAdapter>(name.ToLower()).Adapter.GetState().Keys.ToArray(); }

        public static void GetPairs(string name, out string[] names, out double[] values)
        {
            checkNull(bank);
            var state = bank.GetAdapterSpec<VectorAdapter>(name).Adapter.GetState();
            values = state.Values.ToArray();
            names = state.Keys.ToArray();
        }

        public static void Set(string name, int index, double value)
        { checkNull(bank); bank.GetAdapterSpec<VectorAdapter>(name.ToLower()).Adapter.Set(index, value); }

        public static void Set(string name, string tag, double value)
        { checkNull(bank); bank.GetAdapterSpec<VectorAdapter>(name.ToLower()).Adapter.Set(tag.ToLower(), value); }

        #endregion

        #region Camera commands

        public static MyroImage TakePicture(string type)
        { return takePicture(MyroImageType.CreateFromShortName(type.ToLower())); }

        public static MyroImage takePicture(MyroImageType type)
        {
            if (webcamAdapter != null)
            {
                int width, height;
                byte[] image;
                webcamAdapter.Adapter.QueryFrame(type, out width, out height, out image);
                return new MyroImage()
                {
                    Width = width,
                    Height = height,
                    Image = image
                };
            }
            else
                throw new MyroNotInitializedException();
        }

        public static Bitmap TakeBitmap(MyroImageType type)
        {
            if (webcamAdapter != null)
            {
                var r = takePicture(type);

                Bitmap ret;
                System.Drawing.Imaging.BitmapData bitmapData;
                ret = new Bitmap(r.Width, r.Height, System.Drawing.Imaging.PixelFormat.Format24bppRgb);

                if (r.Image.Length != 3 * ret.Width * ret.Height)
                    throw new Exception("Image returned from QueryFrame in TakeBitmap is the wrong size");

                // Get bitmap data
                bitmapData = ret.LockBits(
                    new Rectangle(0, 0, ret.Width, ret.Height),
                    System.Drawing.Imaging.ImageLockMode.WriteOnly,
                    System.Drawing.Imaging.PixelFormat.Format24bppRgb);

                // Copy frame
                System.Runtime.InteropServices.Marshal.Copy(
                    r.Image, 0, bitmapData.Scan0, 3 * ret.Width * ret.Height);

                ret.UnlockBits(bitmapData);
                return ret;
            }
            else
                throw new MyroNotInitializedException();
        }

        public static void darkenCamera(byte level)
        {
            if (controlAdapter != null) controlAdapter.Adapter.DarkenCamera(level);
            else throw new MyroNotInitializedException();
        }

        public static void autoCamera()
        {
            if (controlAdapter != null) controlAdapter.Adapter.AutoCamera();
            else throw new MyroNotInitializedException();
        }

        #endregion Camera commands

        #region Exception definitions

        private static void checkNull(object adapter)
        {
            if (adapter == null)
                throw new MyroNotInitializedException();
        }

        /// <summary>
        /// This exception indicates that one of the methods in this class was
        /// called before init(...) was called.
        /// </summary>
        public class MyroNotInitializedException : Exception
        {
            public MyroNotInitializedException() : base(Strings.NotInitialized) { }
        }

        /// <summary>
        /// This exception indicates that initialization failed.
        /// </summary>
        public class MyroInitException : Exception
        {
            public MyroInitException(string message) : base(message) { }
        }

        #endregion
    }

    /// <summary>
    /// This class (temporarily) represents an image as returned by takePicture().
    /// In the future, this will be removed as takePicture() will return an image
    /// compatible with the existing Myro image processing library.
    /// </summary>
    public class MyroImage
    {
        /// <summary>
        /// The width of the image, in pixels
        /// </summary>
        public int Width;
        /// <summary>
        /// The height of the image, in pixels
        /// </summary>
        public int Height;
        /// <summary>
        /// The image data, in 24-bit RGB format
        /// </summary>
        public byte[] Image;
    }

}
