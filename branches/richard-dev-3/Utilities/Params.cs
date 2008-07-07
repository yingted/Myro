using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;

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
    }
}
