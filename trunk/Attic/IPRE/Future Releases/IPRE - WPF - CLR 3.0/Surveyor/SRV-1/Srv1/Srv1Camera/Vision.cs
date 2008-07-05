using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;

using Microsoft.Ccr.Core;
using SharpLogic.Robotics.Surveyor.Srv1;
using srv1srv = SharpLogic.Robotics.Services.Surveyor.Srv1;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Vision
{
    public delegate void CaptureEvent(object sender, CaptureEventArgs e);

    public class CaptureEventArgs : EventArgs
    {
        private Bitmap _frame;
        public Bitmap Frame
        {
            get { return _frame; }
            set { _frame = value; }
        }

        private DateTime _sampleTime;
        public DateTime SampleTime
        {
            get { return _sampleTime; }
            set { _sampleTime = value; }
        }
    }

    public class Format
    {
        public Format() { }

        public Format(int width, int height, int minFrameRate, int maxFrameRate, uint compression)
        {
            this._width = width;
            this._height = height;
            this._minFrameRate = minFrameRate;
            this._maxFrameRate = maxFrameRate;
            this._compression = compression;
        }

        private int _width;
        public int Width
        {
            get { return _width; }
            set { _width = value; }
        }

        private int _height;
        public int Height
        {
            get { return _height; }
            set { _height = value; }
        }

        private int _minFrameRate;
        public int MinFrameRate
        {
            get { return _minFrameRate; }
            set { _minFrameRate = value; }
        }

        private int _maxFrameRate;
        public int MaxFrameRate
        {
            get { return _maxFrameRate; }
            set { _maxFrameRate = value; }
        }

        private uint _compression;
        public uint Compression
        {
            get { return _compression; }
            set { _compression = value; }
        }
    }

    public class Camera : IDisposable
    {
        public Camera()
        {
        }

        /// <summary>
        /// Surveyor camera instance.
        /// </summary>
        public static Camera Instance
        {
            get
            {
                if (_instance == null)
                {
                    _instance = new Camera(
                        new Format[]
                        {
                            new Format(80, 64, 1, 3, 0),
                            new Format(160, 128, 1, 2, 0),
                            new Format(320, 240, 1, 1, 0)
                        },
                        "Surveyor SRV-1 Camera",
                        "remote robot"
                    );
                }
                return _instance;
            }
        }
        private static Camera _instance;

        /// <summary>
        /// Surveyor scan view instance.
        /// </summary>
        public static Camera ScanView
        {
            get
            {
                Camera scanView = new Camera();
                scanView._formats = new Format[]
                {
                    new Format(80, 64, 1, 5, 0),
                    new Format(160, 128, 1, 5, 0),
                    new Format(320, 240, 1, 5, 0),
                    new Format(640, 480, 1, 5, 0)
                };
                scanView._name = "Surveyor Scan View";
                scanView._path = "radar on robot";
                return scanView;
            }
        }

        private Camera(Format[] formats, string name, string path)
        {
            this._formats = formats;
            this._name = name;
            this._path = path;
        }

        private Format[] _formats;
        public Format[] Formats
        {
            get { return _formats; }
        }

        private string _name;
        public string Name
        {
            get { return _name; }
        }

        private string _path;
        public string Path
        {
            get { return _path; }
        }

        public void Dispose()
        {
        }
    }

    public class CameraCollection : IEnumerable<Camera>
    {
        public CameraCollection()
        {
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public IEnumerator<Camera> GetEnumerator()
        {
            yield return Camera.Instance;
            yield return Camera.ScanView;
        }
    }

    public class FrameGrabber : IDisposable
    {
        private Format _format;
        private bool _capturing;
        private Camera _camera;
        private bool _scanMode;
        private static Srv1Controller _srv1;
        private DispatcherQueue _taskQueue;
        private PortSet<Bitmap, Exception> _capturePort = new PortSet<Bitmap, Exception>();

        private FrameGrabber(Camera camera)
        {
            this._camera = camera;
            this._scanMode = camera.Path.Contains("radar");
            _srv1 = srv1srv.Srv1Service.Srv1Controller;
            // Set the default resolution to 160x128
            this.Format = this.Formats[1];

            Dispatcher dispatcher = new Dispatcher(0, "FrameGrabber");
            this._taskQueue = new DispatcherQueue("FrameGrabber DispatcherQueue", dispatcher);

            this.StartBehavior();

            while (_srv1 == null) { System.Threading.Thread.Sleep(500); }
        }

        private void StartBehavior()
        {
            Arbiter.Activate(_taskQueue,
                Arbiter.Receive<Bitmap>(
                    true, _capturePort, delegate(Bitmap frame)
                    {
                        // Raise the CaptureFrame event.
                        this.OnCaptureFrame(frame);
                        // Capture next frame.
                        this.CaptureSurveyorFrame(_capturePort);
                    }
                ),
                Arbiter.Receive<Exception>(
                    true, _capturePort, delegate(Exception error)
                    {
                        // Sometimes Surveyor robot does not respond to the
                        // 'I' command and it times out. So we query again.
                        this.CaptureSurveyorFrame(_capturePort);
                    }
                )
            );
        }

        public static FrameGrabber FromCamera(Camera camera)
        {
            return new FrameGrabber(camera);
        }

        public event CaptureEvent CaptureFrame;

        private void OnCaptureFrame(Bitmap frame)
        {
            if (this.CaptureFrame != null)
            {
                CaptureEventArgs args = new CaptureEventArgs();
                args.SampleTime = DateTime.Now;
                args.Frame = frame;
                this.CaptureFrame(this, args);
            }
        }

        public void StartCapture()
        {
            if (this._capturing) { return; };

            if (this._scanMode)
            {
                _srv1.EnableWanderModeScanOnly();
            }
            this._capturing = true;            
            this.CaptureSurveyorFrame(_capturePort);
        }

        public void StopCapture()
        {
            if (this._scanMode)
            {
                _srv1.DisableWanderMode();
            }

            // Stop the timer.
            this._capturing = false;
        }

        public bool Capturing
        {
            get
            {
                return _capturing;
            }
        }

        public Format Format
        {
            get
            {
                return this._format;
            }
            set
            {
                if (!this._scanMode)
                {
                    switch (value.Width)
                    {
                        case 80:  //80x64
                            _srv1.SetImageResolution(ImageResolutions.Resolution80x64);
                            break;
                        case 160: //160x128
                            _srv1.SetImageResolution(ImageResolutions.Resolution160x128);
                            break;
                        case 320: //320x240
                            _srv1.SetImageResolution(ImageResolutions.Resolution320x240);
                            break;
                    }
                }
                this._format = value;
            }
        }

        public Format[] Formats
        {
            get
            {
                return _camera.Formats;
            }
        }

        public void Dispose()
        {
            if (this._taskQueue != null)
            {
                if (this._taskQueue.Dispatcher != null)
                {
                    // This will also dispose of the DispatcherQueue.
                    this._taskQueue.Dispatcher.Dispose();
                }
                else
                {
                    this._taskQueue.Dispose();
                }
                this._taskQueue = null;
            }
        }

        private void CaptureSurveyorFrame(PortSet<Bitmap, Exception> capturePort)
        {
            if (!this._capturing) { return; };

            try
            {
                Bitmap frame = null;
                if (!this._scanMode)
                {
                    using (Image image = _srv1.ForceCaptureImage())
                    {
                        // To avoid GDI+ generic exception we're not using the capture image directly.
                        frame = new Bitmap(image);
                    }
                }
                else
                {
                    frame = this.CreateScanFrame();
                }
                // Check if the camera service is still active as it might have
                // been deactivated by another service running in another thread.
                if (this._capturing)
                {
                    capturePort.Post(frame);
                }
            }
            catch (TimeoutException ex)
            {
                if (this._capturing)
                {
                    capturePort.Post(ex);
                }
            }
        }

        /// <summary>
        /// Creates an Image from the Surveyor scan data.
        /// </summary>
        private Bitmap CreateScanFrame()
        {
            Color floorColor = Color.DarkGreen;
            Color blobColor = Color.LightGreen;
            Color vectorColor = Color.Maroon;

            int width = this.Format.Width;
            int height = this.Format.Height;

            // Get pixel column vector data (80x64).
            byte[] pcv = _srv1.Scan();

            //// For test: Fill with sample data: linear
            //for (int n = 0; n < 80; n++)
            //{
            //    pcv[n] = (byte)(64 * n / 80);
            //}

            //// For test: Fill with sample data: Sin curve
            //for (int n = 0; n < 80; n++)
            //{
            //    pcv[n] = (byte)(32f * (Math.Sin(n * 4.5 * Math.PI / 180)+1));
            //}

            Bitmap scanGraph = new Bitmap(width, height);
            Pen blobPen = new Pen(blobColor);
            Brush vectorBrush = new Pen(vectorColor).Brush;
            using (Graphics graphics = Graphics.FromImage(scanGraph))
            {
                float blockWidth = width / 80f;
                float blockHeight = height / 64f;
                graphics.Clear(floorColor);
                for (int i = 0; i < pcv.Length; i++)
                {
                    // Draw the obstacle area
                    Rectangle rect = new Rectangle(
                        (int)(i * blockWidth),
                        0,
                        (int)blockWidth,
                        (int)(height - ((pcv[i]/* - 2*/) * blockHeight))
                    );
                    graphics.DrawRectangle(blobPen, rect);
                    // Draw the PCV dots.
                    graphics.FillRectangle(vectorBrush, rect.Left, rect.Bottom, (int)blockWidth+1, (int)blockHeight);
                }
            }
            return scanGraph;
        }
    }

}
