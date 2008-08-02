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
    /// <summary>
    /// This is the global Myro parameters class.  Any parameters should be stored here.
    /// This class also computes some parameters at runtime, such as the config and bin paths.
    /// </summary>
    public class Params
    {
        static Params()
        {
            string bindir = Path.GetDirectoryName(Assembly.GetAssembly(typeof(Params)).Location);
            if (Path.GetFileName(bindir).Equals("bin"))
            {
                RootPath = Path.GetDirectoryName(bindir);
                BinPath = bindir;
                ConfigPath = Path.Combine(RootPath, "config");
            }
            //else
            //{
            //    throw new Exception("Myro does not seem to be located correctly");
            //}
        }

        /// <summary>
        /// The default timeout for RSUtils.RecieveSync
        /// </summary>
        public static int DefaultRecieveTimeout = 30000;
        /// <summary>
        /// The path to the Myro installation root (computed automatically)
        /// </summary>
        public static string RootPath = null;
        /// <summary>
        /// The path to the Myro config directory (computed automatically)
        /// </summary>
        public static string ConfigPath = null; // "C:\\Microsoft Robotics Dev Studio 2008\\config";
        /// <summary>
        /// The path to the Myro bin directory (computed automatically)
        /// </summary>
        public static string BinPath = null; //"C:\\Microsoft Robotics Dev Studio 2008\\bin";
        /// <summary>
        /// The default Http port for DSS.
        /// </summary>
        public static int DefaultHttpPort = 50000;
        /// <summary>
        /// The default Dssp port for DSS.
        /// </summary>
        public static int DefaultDsspPort = 50001;
    }

    /// <summary>
    /// The class that is used internally to represent the image types supported
    /// by a webcam service.  This is currently set up for the Fluke.
    /// </summary>
    public class MyroImageType
    {
        /// <summary>
        /// The name as specified in takePicture(...)
        /// </summary>
        public string ShortName;
        /// <summary>
        /// The description that shows up in the GUI.
        /// </summary>
        public string FriendlyName;
        /// <summary>
        /// The image type GUID to pass to the underlying web cam service.
        /// </summary>
        public Guid Guid;
        /// <summary>
        /// The width of the image (pre-known image sizes are currently assumed)
        /// </summary>
        public int Width;
        /// <summary>
        /// The height of the image (pre-known image sizes are currently assumed)
        /// </summary>
        public int Height;

        public MyroImageType()
        {
        }

        /// <summary>
        /// Return the class instance associated with the GUID.  This GUID must be in the
        /// KnownImageTypes property.
        /// </summary>
        /// <param name="guid"></param>
        /// <returns></returns>
        public static MyroImageType CreateFromGuid(Guid guid)
        {
            var ret = KnownImageTypes.Find(it => it.Guid.Equals(guid));
            if (ret == default(MyroImageType))
                throw new ArgumentException("Guid does not represent a known Myro image type.  Try the Guids in Myro.Utilities.Params");
            return ret;
        }

        /// <summary>
        /// Return the class instance associated with the short name.  This short name must be in the
        /// KnownImageTypes property.
        /// </summary>
        /// <param name="guid"></param>
        /// <returns></returns>
        public static MyroImageType CreateFromShortName(string shortName)
        {
            var ret = KnownImageTypes.Find(it => it.ShortName.Equals(shortName, StringComparison.CurrentCultureIgnoreCase));
            if (ret == default(MyroImageType))
                throw new ArgumentException("Guid does not represent a known Myro image type.  Try the Guids in Myro.Utilities.Params");
            return ret;
        }

        /// <summary>
        /// Fluke full color image
        /// </summary>
        public static MyroImageType Color = new MyroImageType()
        {
            ShortName = "color",
            FriendlyName = "Uncompressed Color",
            Guid = new Guid("8FC7EC69-B0AE-42ab-B3D4-CF167B924D95"),
            Width = 256,
            Height = 192
        };
        /// <summary>
        /// Fluke grayscale image.
        /// </summary>
        public static MyroImageType Gray = new MyroImageType()
        {
            ShortName = "gray",
            FriendlyName = "Uncompressed Grayscale",
            Guid = new Guid("DBFCB483-0DF4-4424-AD65-9B87F96A1732"),
            Width = 256,
            Height = 192
        };
        /// <summary>
        /// Fluke JPEG image.
        /// </summary>
        public static MyroImageType JpegColor = new MyroImageType()
        {
            ShortName = "jpeg",
            FriendlyName = "JPEG Color",
            Guid = new Guid("49BCCEED-49C3-4757-8EFB-2CEAC62C36A4"),
            Width = 256,
            Height = 192
        };
        /// <summary>
        /// Fluke fast JPEG image.
        /// </summary>
        public static MyroImageType JpegColorFast = new MyroImageType()
        {
            ShortName = "jpeg-fast",
            FriendlyName = "JPEG Color Fast Mode",
            Guid = new Guid("56794F80-5A6C-49f7-9F92-2EDCDA99F763"),
            Width = 256,
            Height = 192
        };
        /// <summary>
        /// Fluke grayscale JPEG.
        /// </summary>
        public static MyroImageType JpegGray = new MyroImageType()
        {
            ShortName = "grayjpeg",
            FriendlyName = "JPEG Grayscale",
            Guid = new Guid("DCF50C62-3854-4ac8-AD3F-69C111FA8053"),
            Width = 256,
            Height = 192
        };
        /// <summary>
        /// Fluke Fast grayscale JPEG.
        /// </summary>
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
