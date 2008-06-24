//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1CameraState.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Drawing;

using Microsoft.Dss.Core.Attributes;
using vision = SharpLogic.Robotics.Services.Surveyor.Srv1.Vision;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Camera
{
    
    [DataContract]
    public class Srv1CameraState
    {
        private List<CameraInstance> _cameras = new List<CameraInstance>();
        [DataMember(IsRequired = true)]
        public List<CameraInstance> Cameras
        {
            get { return _cameras; }
            set { _cameras = value; }
        }

        private CameraInstance _selected;
        [DataMember]
        public CameraInstance Selected
        {
            get { return _selected; }
            set { _selected = value; }
        }


        [DataMember]
        public Size Size
        {
            get
            {
                if (_image != null)
                {
                    return _image.Size;
                }
                return Size.Empty;
            }
            set { return; }
        }

        private DateTime _timeStamp;

        [DataMember]
        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }

        string _captureFile;
        [DataMember]
        public string CaptureFile
        {
            get { return _captureFile; }
            set { _captureFile = value; }
        }

        double _quality;
        [DataMember]
        public double Quality
        {
            get { return _quality; }
            set { _quality = value; }
        }

        private Bitmap _image;

        public Bitmap Image
        {
            get { return _image; }
            set { _image = value; }
        }
    }

    [DataContract]
    public class CameraInstance
    {
        private string _friendlyName;
        [DataMember]
        public string FriendlyName
        {
            get { return _friendlyName; }
            set { _friendlyName = value; }
        }

        private string _devicePath;
        [DataMember]
        public string DevicePath
        {
            get { return _devicePath; }
            set { _devicePath = value; }
        }

        private vision.FrameGrabber _frameGrabber;

        public vision.FrameGrabber FrameGrabber
        {
            get { return _frameGrabber; }
            set { _frameGrabber = value; }
        }

        private List<Format> _supportedFormats;
        [DataMember]
        public List<Format> SupportedFormats
        {
            get { return _supportedFormats; }
            set { _supportedFormats = value; }
        }

        private Format _format;
        [DataMember]
        public Format Format
        {
            get { return _format; }
            set { _format = value; }
        }
    }

    [DataContract]
    public class Format
    {
        public Format() { }

        public Format(vision.Format format)
        {
            _width = format.Width;
            _height = format.Height;
            _minFramesPerSecond = format.MinFrameRate;
            _maxFramesPerSecond = format.MaxFrameRate;

            _compression = string.Empty;
            uint convert = format.Compression;
            while (convert > 0)
            {
                _compression += (char)(convert & 0xFF);
                convert >>= 8;
            }
        }

        private int _width;
        [DataMember]
        public int Width
        {
            get { return _width; }
            set { _width = value; }
        }

        private int _height;
        [DataMember]
        public int Height
        {
            get { return _height; }
            set { _height = value; }
        }

        private int _minFramesPerSecond;
        [DataMember]
        public int MinFramesPerSecond
        {
            get { return _minFramesPerSecond; }
            set { _minFramesPerSecond = value; }
        }

        private int _maxFramesPerSecond;
        [DataMember]
        public int MaxFramesPerSecond
        {
            get { return _maxFramesPerSecond; }
            set { _maxFramesPerSecond = value; }
        }

        private string _compression;
        [DataMember]
        public string Compression
        {
            get { return _compression; }
            set { _compression = value; }
        }

        public static explicit operator vision.Format(Format format)
        {
            uint fourcc = 0;

            if (!string.IsNullOrEmpty(format.Compression))
            {
                string comp = format.Compression;
                int length = Math.Min(4,comp.Length);

                for (int i = length - 1; i >= 0; i--)
                {
                    fourcc = (uint)((fourcc << 8) + ((int)comp[i] & 0xFF));
                }
            }

            return new vision.Format(
                format.Width,
                format.Height,
                format.MinFramesPerSecond,
                format.MaxFramesPerSecond,
                fourcc
            );
        }
    }

    [DataContract]
    public class QueryFrameRequest
    {
        [DataMember]
        public Guid Format;
        [DataMember]
        public Size Size;
    }

    [DataContract]
    public class QueryFrameResponse
    {
        [DataMember]
        public Guid Format;
        [DataMember]
        public Size Size;
        [DataMember]
        public byte[] Frame;
        [DataMember]
        public DateTime TimeStamp;
    }

    [DataContract]
    public class UpdateDeviceRequest
    {
        private CameraInstance _selected = new CameraInstance();
        [DataMember(IsRequired = true)]
        public CameraInstance Selected
        {
            get { return _selected; }
            set { _selected = value; }
        }
	
    }

    [DataContract]
    public class UpdateFrameRequest
    {
        [DataMember]
        public DateTime TimeStamp;
    }
}
