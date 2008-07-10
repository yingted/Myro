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

        public static int DefaultRecieveTimeout = 20000;
        public static string ConfigPath = null; // "C:\\Microsoft Robotics Dev Studio 2008\\config";
        public static string BinPath = null; //"C:\\Microsoft Robotics Dev Studio 2008\\bin";
        //public static string PythonPath = "C:\\IronPython-1.1.1\\Lib";
        public static int DefaultHttpPort = 50000;
        public static int DefaultDsspPort = 50001;

        public static readonly Guid Image_Color = new Guid("8FC7EC69-B0AE-42ab-B3D4-CF167B924D95");
        public static readonly Guid Image_Gray = new Guid("DBFCB483-0DF4-4424-AD65-9B87F96A1732");
    }

    public class MyroImageType
    {
        public string ShortName;
        public string FriendlyName;
        public Guid Guid;
        public int BitsPerPixel;
        public int Width;
        public int Height;
        public System.Drawing.Imaging.PixelFormat PixelFormat;

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
            Guid = Params.Image_Color,
            BitsPerPixel = 3,
            Width = 256,
            Height = 192,
            PixelFormat = System.Drawing.Imaging.PixelFormat.Format24bppRgb
        };
        public static MyroImageType Gray = new MyroImageType()
        {
            ShortName = "gray",
            FriendlyName = "Uncompressed Grayscale",
            Guid = Params.Image_Gray,
            BitsPerPixel = 1,
            Width = 128,
            Height = 96,
            PixelFormat = System.Drawing.Imaging.PixelFormat.Format16bppGrayScale
        };

        /// <summary>
        /// Known image types
        /// </summary>
        public static List<MyroImageType> KnownImageTypes = new List<MyroImageType>()
        {
            Color,
            Gray
        };
    }

}
