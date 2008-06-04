using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using Microsoft.Dss.ServiceModel.Dssp;

namespace Myro.Adapters
{
    public class AdapterSpec
    {
        public AdapterTypeEnum Type { get; private set; }
        public string Name { get; private set; }
        private ReaderWriterLock adapterLock = new ReaderWriterLock();
        private IAdapter<Object> adapter;
        public IAdapter<Object> Adapter
        {
            get
            {
                adapterLock.AcquireReaderLock(-1);
                IAdapter<Object> ret = adapter;
                adapterLock.ReleaseReaderLock();
                return ret;
            }
            set
            {
                adapterLock.AcquireWriterLock(-1);
                adapter = value;
                adapterLock.ReleaseWriterLock();
            }
        }
        public ServiceInfoType ServiceConfig { get; private set; }
        public AdapterSpec(AdapterTypeEnum type, string name, ServiceInfoType serviceConfig)
        {
            Type = type;
            Name = name;
            Adapter = null;
            ServiceConfig = serviceConfig;
        }
    }

    public class NoSuchAdapter : Exception
    {
        public NoSuchAdapter(string reason)
            : base(reason)
        {
        }
    }

    public enum AdapterTypeEnum
    {
        Vector,
        Drive,
        Image,
        Speech
    }

    public class AdapterFactory
    {
        //private static AdapterTypeEnum[] adapterTypes;
        private static string[] adapterTypeNames = 
        {
            "Vector",
            "Drive",
            "Image",
            "Speech"
        };

        private static Dictionary<AdapterTypeEnum, string> byType;
        private static Dictionary<string, AdapterTypeEnum> byName;

        //public static AdapterTypeEnum[] AdapterTypes { get; private set; }
        public static string[] AdapterTypeNames { get; private set; }

        static AdapterFactory()
        {
            byType = new Dictionary<AdapterTypeEnum, string>();
            byName = new Dictionary<string, AdapterTypeEnum>();
            for (int i = 0; i < adapterTypeNames.Length; i++)
            {
                byType.Add((AdapterTypeEnum)i, adapterTypeNames[i]);
                byName.Add(adapterTypeNames[i], (AdapterTypeEnum)i);
            }
        }

        public static string GetTypeName(AdapterTypeEnum type)
        {
            string ret = byType[type];
            if (ret != null)
                return ret;
            else
                throw new NoSuchAdapter("Adapter type invalid");
        }

        public static AdapterTypeEnum GetType(string name)
        {
            AdapterTypeEnum ret = byName[name];
            if (ret != null)
                return ret;
            else
                throw new NoSuchAdapter("Adapter name invalid");
        }

        public static void CreateAdapterIfNeeded(AdapterSpec adapterSpec, ServiceInfoType serviceRecord)
        {
            Monitor.Enter(adapterSpec);
            try
            {
                if (adapterSpec.Adapter == null)
                {
                    switch (adapterSpec.Type)
                    {
                        case AdapterTypeEnum.Vector:
                            adapterSpec.Adapter = (IAdapter<Object>)(new Adapters.VectorAdapter(serviceRecord));
                            break;
                        case AdapterTypeEnum.Drive:
                            adapterSpec.Adapter = (IAdapter<Object>)(new Adapters.DriveAdapter(serviceRecord));
                            break;
                        default:
                            throw new Exception("Adapter type " + GetTypeName(adapterSpec.Type) + " not yet supported");
                    }
                }
                else
                {
                    //Console.WriteLine("*** ERROR *** Adapter for this AdapterSpec already has been created");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("*** ERROR *** " + e);
            }
            finally
            {
                //Console.WriteLine("Unlocking");
                Monitor.Exit(adapterSpec);
            }
        }
    }
}
