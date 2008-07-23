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

namespace Myro
{
    public static class Robot
    {
        private static AdapterBank bank = null;
        private static AdapterSpec<DriveAdapter> driveAdapter = null;
        private static AdapterSpec<VectorAdapter> soundAdapter = null;
        private static AdapterSpec<WebcamAdapter> webcamAdapter = null;
        private static AdapterSpec<FlukeControlAdapter> controlAdapter = null;
        private static int httpPort;
        private static int dsspPort;

        #region Startup and shutdown
        public static void Init(string manifestFile, int httpPort, int dsspPort)
        {
            if (bank == null)
            {
                Robot.httpPort = httpPort;
                Robot.dsspPort = dsspPort;
                FileAttributes att = File.GetAttributes(manifestFile);
                if ((att & (FileAttributes.Device | FileAttributes.Directory | FileAttributes.Offline)) != 0)
                    throw new IOException("Manifest file is not a normal file");
                Console.Write("Starting DSS environment...");
                DssEnvironment.Initialize(httpPort, dsspPort, "file://" + manifestFile);
                Console.WriteLine("Done");
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
            else
                throw new Exception("Myro is already initialized");
        }

        public static void init(string baseName)
        {
            Init(Path.Combine(Path.Combine(Params.ConfigPath, baseName + ".manifest"), baseName + ".manifest.xml"), Params.DefaultHttpPort, Params.DefaultDsspPort);
        }

        public static void shutdown()
        {
            if (bank != null)
            {
                bank.Dispose();
                DssEnvironment.Shutdown();
                DssEnvironment.WaitForShutdown();
            }
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

        public class MyroNotInitializedException : Exception
        {
            public MyroNotInitializedException() : base(Strings.NotInitialized) { }
        }

        #endregion

        //AdapterBank bank;
        //public MyroSensors Sensors { get; private set; }
        //public MyroMovement Movement { get; private set; }
        //public IMyroSound Sound { get; private set; }

        //public Robot(string manifestFile)
        //{
        //    FileAttributes att = File.GetAttributes(manifestFile);
        //    if ((att & (FileAttributes.Device | FileAttributes.Directory | FileAttributes.Offline)) != 0)
        //        throw new IOException("Manifest file is not a normal file");
        //    Console.Write("Starting DSS environment...");
        //    DssEnvironment.Initialize(50000, 50001, "file://" + manifestFile);
        //    Console.WriteLine("Done");
        //    bank = new AdapterBank(new List<IAdapterFactory>() {
        //        new Myro.Adapters.DriveAdapterFactory(),
        //        new Myro.Adapters.VectorAdapterFactory()
        //    });

        //    Sensors = new MyroSensors(bank);
        //    Movement = new MyroMovement(bank);
        //    Sound = new MyroSound(bank);
        //}

        //public void Shutdown()
        //{
        //    bank.Dispose();
        //    DssEnvironment.Shutdown();
        //    DssEnvironment.WaitForShutdown();
        //}
    }

    public class MyroImage
    {
        public int Width;
        public int Height;
        public byte[] Image;
    }

}
