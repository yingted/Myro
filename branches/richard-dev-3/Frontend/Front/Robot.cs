using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Myro.Adapters;
using Myro.API;
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
        private static int httpPort;
        private static int dsspPort;

        #region Startup and shutdown
        public static void Init(string manifestFile, int httpPort, int dsspPort)
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
                new Myro.Adapters.WebcamAdapterFactory()
            });
            driveAdapter = bank.GetAdapterSpec<DriveAdapter>("drive");
            soundAdapter = bank.GetAdapterSpec<VectorAdapter>("tonegen");
            webcamAdapter = bank.GetAdapterSpec<WebcamAdapter>("webcam");
        }

        public static void Init(string baseName)
        {
            Init(Path.Combine(Path.Combine(Params.ConfigPath, baseName + ".manifest"), baseName + ".manifest.xml"), Params.DefaultHttpPort, Params.DefaultDsspPort);
        }

        public static void Shutdown()
        {
            if (bank != null)
            {
                bank.Dispose();
                DssEnvironment.Shutdown();
                DssEnvironment.WaitForShutdown();
            }
        }
        #endregion

        #region Drive commands
        public static void Move(double translate, double rotate)
        {
            throw new NotImplementedException("Move not yet implemented");
        }

        public static void Forward(double power)
        {
            checkPowerRange(power);
            SetMotors(power, power);
        }

        public static void ForwardFor(double power, double seconds)
        {
            checkPowerRange(power);
            SetMotorsFor(power, power, seconds);
        }

        public static void Backward(double power)
        {
            checkPowerRange(power);
            SetMotors(-power, -power);
        }

        public static void BackwardFor(double power, double seconds)
        {
            checkPowerRange(power);
            SetMotorsFor(-power, -power, seconds);
        }

        public static void Turn(string direction, double power)
        {
            int dir = checkDirection(direction);
            if (dir == -1)
                TurnLeft(power);
            else if (dir == 1)
                TurnRight(power);
        }

        public static void TurnFor(string direction, double power, double seconds)
        {
            int dir = checkDirection(direction);
            if (dir == -1)
                TurnLeftFor(power, seconds);
            else if (dir == 1)
                TurnRightFor(power, seconds);
        }

        public static void TurnLeft(double power)
        {
            checkPowerRange(power);
            SetMotors(-power, power);
        }

        public static void TurnLeftFor(double power, double seconds)
        {
            checkPowerRange(power);
            SetMotorsFor(-power, power, seconds);
        }

        public static void TurnRight(double power)
        {
            checkPowerRange(power);
            SetMotors(power, -power);
        }

        public static void TurnRightFor(double power, double seconds)
        {
            checkPowerRange(power);
            SetMotorsFor(power, -power, seconds);
        }

        public static void Stop()
        {
            SetMotors(0f, 0f);
        }

        public static void SetMotorsFor(double leftPower, double rightPower, double seconds)
        {
            SetMotors(leftPower, rightPower);
            Thread.Sleep((int)(seconds * 1000));
            Stop();
        }

        public static void SetMotors(double leftPower, double rightPower)
        {
            if (driveAdapter != null)
                driveAdapter.Adapter.SetMotors(leftPower, rightPower);
            else
                throw new MyroNotInitializedException();
        }

        private static void checkPowerRange(double power)
        {
            if (power < -1.0f || power > 1.0f)
                throw new ArgumentException("Motor power settings must be in the range of -1.0 to 1.0");
        }

        private static int checkDirection(string direction)
        {
            if (direction == null)
                throw new ArgumentException("Direction settings when turning must be either 'left' or 'right'");
            direction = direction.ToLower().Trim();
            if (direction.Equals("left"))
                return -1;
            if (direction.Equals("right"))
                return 1;
            throw new ArgumentException("Direction settings when turning must be either 'left' or 'right'");
        }
        #endregion

        #region Sound commands
        public static IMyroSong ReadSong(string fileName)
        {
            throw new NotImplementedException();
        }

        public static IMyroSong MakeSong(string text)
        {
            throw new NotImplementedException();
        }

        public static void SaveSong(string text, string fileName)
        {
            throw new NotImplementedException();
        }

        public static void PlaySong(IMyroSong song)
        {
            throw new NotImplementedException();
        }

        public static void PlaySong(string text)
        {
            PlaySong(MakeSong(text));
        }

        public static void Beep(double duration, double frequency)
        {
            Beep(duration, frequency, 0.0);
        }

        public static void Beep(double duration, double frequency1, double frequency2)
        {
            if (soundAdapter != null)
                soundAdapter.Adapter.Set(
                    new List<int>() { 0, 1, 2 },
                    new List<double>() { frequency1, frequency2, duration });
            else
                throw new MyroNotInitializedException();
        }

        public static void SetLoud(bool loud)
        {
            if (soundAdapter != null)
                soundAdapter.Adapter.Set(3, loud ? 1.0 : 0.0);
            else
                throw new MyroNotInitializedException();
        }
        #endregion

        #region Sensor commands
        public static double[] Get(string name)
        {
            return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.GetAllElements().ToArray();
        }

        public static double Get(string name, int index)
        {
            return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Get(index);
        }

        public static double Get(string name, string tag)
        {
            return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Get(tag);
        }

        public static string[] GetNames(string name)
        {
            return bank.GetAdapterSpec<VectorAdapter>(name).Adapter.GetState().Keys.ToArray();
        }

        public static void GetPairs(string name, out string[] names, out double[] values)
        {
            var state = bank.GetAdapterSpec<VectorAdapter>(name).Adapter.GetState();
            values = state.Values.ToArray();
            names = state.Keys.ToArray();
        }

        public static void Set(string name, int index, double value)
        {
            bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Set(index, value);
        }

        public static void Set(string name, string tag, double value)
        {
            bank.GetAdapterSpec<VectorAdapter>(name).Adapter.Set(tag, value);
        }
        #endregion

        #region Camera commands

        public static MyroImage TakePicture(MyroImageType type)
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

        public static Bitmap TakeBitmap(MyroImageType type)
        {
            var r = TakePicture(type);
            
            Bitmap ret;
            System.Drawing.Imaging.BitmapData bitmapData;
                ret = new Bitmap(r.Width, r.Height, type.PixelFormat);
                // Get bitmap data
                bitmapData = ret.LockBits(
                    new Rectangle(0, 0, ret.Width, ret.Height),
                    System.Drawing.Imaging.ImageLockMode.WriteOnly,
                    type.PixelFormat);

            // Copy frame
            IntPtr scanline = bitmapData.Scan0;
            int frameOffset = 0;
            for(int i = 0; i<ret.Height; i++)
            {
                System.Runtime.InteropServices.Marshal.Copy(
                    r.Image, frameOffset, scanline, type.BitsPerPixel * ret.Width);
                scanline = new IntPtr(scanline.ToInt64() + (long)bitmapData.Stride);
                frameOffset += (type.BitsPerPixel * ret.Width);
            }

            ret.UnlockBits(bitmapData);
            return ret;
        }

        #endregion Camera commands

        #region Exception definitions
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
