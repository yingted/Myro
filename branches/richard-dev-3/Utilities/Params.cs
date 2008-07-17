// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;
using System.Drawing;

namespace Myro.Utilities
{
    public class Params
    {
        static Params()
        {
            string bindir = Path.GetDirectoryName(Assembly.GetAssembly(typeof(Params)).Location);
            if (Path.GetFileName(bindir).Equals("bin"))
            {
                BinPath = bindir;
                ConfigPath = Path.Combine(Path.GetDirectoryName(bindir), "config");
            }
            else
            {
                throw new Exception("Myro does not seen to have been installed in the MSRDS bin directory");
            }
        }

        public static int DefaultRecieveTimeout = 30000;
        public static string ConfigPath = null; // "C:\\Microsoft Robotics Dev Studio 2008\\config";
        public static string BinPath = null; //"C:\\Microsoft Robotics Dev Studio 2008\\bin";
        //public static string PythonPath = "C:\\IronPython-1.1.1\\Lib";
        public static int DefaultHttpPort = 50000;
        public static int DefaultDsspPort = 50001;
    }

    public class MyroImageType
    {
        public string ShortName;
        public string FriendlyName;
        public Guid Guid;
        public int Width;
        public int Height;

        public MyroImageType()
        {
        }

        public static MyroImageType CreateFromGuid(Guid guid)
        {
            var ret = KnownImageTypes.Find(it => it.Guid.Equals(guid));
            if (ret == default(MyroImageType))
                throw new ArgumentException("Guid does not represent a known Myro image type.  Try the Guids in Myro.Utilities.Params");
            return ret;
        }

        public static MyroImageType CreateFromShortName(string shortName)
        {
            var ret = KnownImageTypes.Find(it => it.ShortName.Equals(shortName, StringComparison.CurrentCultureIgnoreCase));
            if (ret == default(MyroImageType))
                throw new ArgumentException("Guid does not represent a known Myro image type.  Try the Guids in Myro.Utilities.Params");
            return ret;
        }

        public static MyroImageType Color = new MyroImageType()
        {
            ShortName = "color",
            FriendlyName = "Uncompressed Color",
            Guid = new Guid("8FC7EC69-B0AE-42ab-B3D4-CF167B924D95"),
            Width = 256,
            Height = 192
        };
        public static MyroImageType Gray = new MyroImageType()
        {
            ShortName = "gray",
            FriendlyName = "Uncompressed Grayscale",
            Guid = new Guid("DBFCB483-0DF4-4424-AD65-9B87F96A1732"),
            Width = 256,
            Height = 192
        };
        public static MyroImageType JpegColor = new MyroImageType()
        {
            ShortName = "jpeg",
            FriendlyName = "JPEG Color",
            Guid = new Guid("49BCCEED-49C3-4757-8EFB-2CEAC62C36A4"),
            Width = 256,
            Height = 192
        };
        public static MyroImageType JpegColorFast = new MyroImageType()
        {
            ShortName = "jpeg-fast",
            FriendlyName = "JPEG Color Fast Mode",
            Guid = new Guid("56794F80-5A6C-49f7-9F92-2EDCDA99F763"),
            Width = 256,
            Height = 192
        };
        public static MyroImageType JpegGray = new MyroImageType()
        {
            ShortName = "grayjpeg",
            FriendlyName = "JPEG Grayscale",
            Guid = new Guid("DCF50C62-3854-4ac8-AD3F-69C111FA8053"),
            Width = 256,
            Height = 192
        };
        public static MyroImageType JpegGrayFast = new MyroImageType()
        {
            ShortName = "grayjpeg-fast",
            FriendlyName = "JPEG Grayscale Fast Mode",
            Guid = new Guid("7C9B7917-8301-4873-B3AB-05A6DD8A52A5"),
            Width = 256,
            Height = 192
        };

        /// <summary>
        /// Known image types
        /// </summary>
        public static List<MyroImageType> KnownImageTypes = new List<MyroImageType>()
        {
            Color,
            Gray,
            JpegColor,
            JpegColorFast,
            JpegGray,
            JpegGrayFast
        };
    }

}
