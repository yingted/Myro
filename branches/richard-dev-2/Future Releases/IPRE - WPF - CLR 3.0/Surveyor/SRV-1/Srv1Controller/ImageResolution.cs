using System;
using System.Collections.Generic;
using System.Text;

namespace SharpLogic.Robotics.Surveyor.Srv1
{
    internal class ImageResolution
    {
        private static readonly ImageResolution _defaultResolution;
        private static readonly int _defaultResTimeout;

        static ImageResolution()
        {
            _defaultResolution = new ImageResolution(ImageResolutions.Resolution160x128);
            _defaultResTimeout = 1200;//ms
        }

        public ImageResolution(ImageResolutions resolution)
            : this(resolution, 0, 0)
        {
            switch (resolution)
            {
                case ImageResolutions.Resolution80x64:
                    this.Set(80, 64);
                    break;
                case ImageResolutions.Resolution160x128:
                    this.Set(160, 128);
                    break;
                case ImageResolutions.Resolution320x240:
                    this.Set(320, 240);
                    break;
            }
        }

        private ImageResolution(ImageResolutions resolution, int width, int height)
        {
            this._resolution = resolution;
            this._width = width;
            this._height = height;
        }

        private void Set(int width, int height)
        {
            this._width = width;
            this._height = height;
        }

        public static ImageResolution DefaultResolution
        {
            get { return ImageResolution._defaultResolution; }
        }

        public ImageResolutions Resolution
        {
            get { return _resolution; }
        }
        ImageResolutions _resolution;

        public int Width
        {
            get { return _width; }
        }
        private int _width;

        public int Height
        {
            get { return _height; }
        }
        private int _height;

        /// <summary>
        /// Returns the timeout for this image resolution in milliseconds
        /// proportional to the default resolution's timeout.
        /// </summary>
        public int Timeout
        {
            get
            {
                ImageResolution baseRes = _defaultResolution;
                return (_defaultResTimeout * this.Width * this.Height / (baseRes.Width * baseRes.Height));
            }
        }
    }
}
