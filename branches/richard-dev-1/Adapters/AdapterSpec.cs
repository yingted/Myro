using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using Microsoft.Dss.ServiceModel.Dssp;
using Myro.Utilities;
using Microsoft.Dss.Hosting;

namespace Myro.Adapters
{
    /// <summary>
    /// This class holds an adapter, and information about the adapter,
    /// including the adapter/service type (Vector, Drive, etc), the Myro
    /// name, and the MSRDS service the adapter connects to.
    /// 
    /// The static members of this class can convert between string adapter
    /// type names, and runtime adapter types.
    /// 
    /// This class is constructed with the adapter unattached to the MSRDS
    /// service.  To attach it, call AttachAdapterIfNeeded(), with the service
    /// record of the service to attach to.  This service record may be
    /// different than the one stored in the ServiceConfig property, because
    /// the ServiceConfig property comes from the config file and specifies
    /// only a partial service URI, whereas the service record passed to this
    /// method should contain a full service URI, such as one returned by a
    /// directory query.
    /// 
    /// To use the adapter, call one of the Get*Adapter methods, depending
    /// on the adapter/service type, stored in the Type member.  These methods
    /// will check that the adapter is in fact of the type you need, and
    /// return an adapter of the correct type.  If, for example, you know that
    /// an AdapterSpec class contains a drive adapter, you can call
    /// GetDriveAdapter() to return the drive adapter.
    /// </summary>
    public class AdapterSpec
    {
        /// <summary>
        /// The runtime type of the adapter.
        /// </summary>
        public AdapterTypeEnum Type { get; private set; }

        /// <summary>
        /// The Myro name of the adapter.
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// The DSS service record, as constructed from the config file.  This
        /// is only a "filter" to find the right service in the DSS directory,
        /// and may contain a relative URI.
        /// </summary>
        public ServiceInfoType ServiceConfig { get; private set; }

        private ReaderWriterLock adapterLock = new ReaderWriterLock();
        private IAdapter adapterObject;
        private IAdapter Adapter
        {
            get
            {
                adapterLock.AcquireReaderLock(-1);
                IAdapter ret = adapterObject;
                adapterLock.ReleaseReaderLock();
                return ret;
            }
            set
            {
                adapterLock.AcquireWriterLock(-1);
                adapterObject = value;
                adapterLock.ReleaseWriterLock();
            }
        }

        /// <summary>
        /// Constructor, with the runtime adapter type, the Myro name, and the
        /// service record from the config file.
        /// </summary>
        /// <param name="type">The runtime adapter type</param>
        /// <param name="name">The Myro name</param>
        /// <param name="serviceConfig">The service record from the config file</param>
        public AdapterSpec(AdapterTypeEnum type, string name, ServiceInfoType serviceConfig)
        {
            Type = type;
            Name = name;
            Adapter = null;
            ServiceConfig = serviceConfig;
        }

        public AdapterSpec(string name)
        {
            ServiceInfoType serviceInfoResponse = RSUtils.RecieveSync(
                RSUtils.FindCompatibleContract(DssEnvironment.TaskQueue, new Uri("dssp.tcp://localhost/" + name), AdapterSpec.KnownContracts));
            Type = byContract[serviceInfoResponse.Contract];
            Name = name;
            Adapter = null;
            ServiceConfig = serviceInfoResponse;
            AttachAdapterIfNeeded(ServiceConfig);
        }

        ///// <summary>
        ///// Alternate constructor taking a string adapter type name, the Myro 
        ///// name, and the service record from the config file.
        ///// </summary>
        ///// <param name="type">The string adapter type name</param>
        ///// <param name="name">The Myro name</param>
        ///// <param name="serviceConfig">The service record from the config file</param>
        //public AdapterSpec(string type, string name, ServiceInfoType serviceConfig)
        //    : this(GetType(type), name, serviceConfig)
        //{
        //}

        /// <summary>
        /// Create an unattachable AdapterSpec.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="name"></param>
        public AdapterSpec(AdapterTypeEnum type, string name)
            : this(type, name, new ServiceInfoType("nocontract"))
        {
        }


        /// <summary>
        /// This methods attaches the adapter to an MSRDS service if it is not
        /// already attached.  The serviceRecord argument may contain a record
        /// different than the one stored in the ServiceConfig property, because
        /// the ServiceConfig property comes from the config file and specifies
        /// only a partial service URI, whereas the service record passed to this
        /// method should contain a full service URI, such as one returned by a
        /// directory query.
        /// </summary>
        /// <param name="serviceRecord">This should contain a full URI to an MSRDS service, such as from a directory query.</param>
        /// <returns>Whether the adapter was actually attached.  False if it was already attached.</returns>
        public bool AttachAdapterIfNeeded(ServiceInfoType serviceRecord)
        {
            Monitor.Enter(this);
            try
            {
                if (Adapter == null)
                {
                    switch (Type)
                    {
                        case AdapterTypeEnum.Vector:
                            Adapter = (IAdapter)(new Adapters.VectorAdapter(serviceRecord));
                            break;
                        case AdapterTypeEnum.Drive:
                            Adapter = (IAdapter)(new Adapters.DriveAdapter(serviceRecord));
                            break;
                        default:
                            throw new Exception("Adapter type " + Type.ToString() + " not yet supported");
                    }
                    Console.WriteLine("Attached to " + serviceRecord.Service + " as \"" + Name + "\"");
                    return true;
                }
                else
                {
                    //Console.WriteLine("*** ERROR *** Adapter for this AdapterSpec already has been created");
                    return false;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("*** ERROR *** " + e);
                return (Adapter != null);
            }
            finally
            {
                //Console.WriteLine("Unlocking");
                Monitor.Exit(this);
            }
        }

        /// <summary>
        /// Tests whether this adapter is attached.
        /// </summary>
        /// <returns></returns>
        public bool isAttached()
        {
            return Adapter != null;
        }

        /// <summary>
        /// Returns the Vector adapter contained in this AdapterSpec, but only
        /// if the adapter is in fact a Vector type.  If it is not a
        /// VectorAdapter, an AdapterOperation exception will be thrown.  This is
        /// essentially a "safe cast".
        /// </summary>
        /// <returns></returns>
        public VectorAdapter GetVectorAdapter()
        {
            if (Type == AdapterTypeEnum.Vector)
                return (VectorAdapter)adapterIfAttached();
            else
                throw new AdapterOperationException("Tried to safe cast to a VectorAdapter");
        }

        /// <summary>
        /// Returns the Drive adapter contained in this AdapterSpec, but only
        /// if the adapter is in fact a Drive type.  If it is not a
        /// DriveAdapter, an InvalidCastException will be thrown.  This is
        /// essentially a "safe cast".
        /// </summary>
        /// <returns></returns>
        public DriveAdapter GetDriveAdapter()
        {
            if (Type == AdapterTypeEnum.Drive)
                return (DriveAdapter)adapterIfAttached();
            else
                throw new AdapterOperationException("Tried to safe cast to a DriveAdapter");
        }

        /// <summary>
        /// Returns the Image adapter contained in this AdapterSpec, but only
        /// if the adapter is in fact a Image type.  If it is not a
        /// ImageAdapter, an InvalidCastException will be thrown.  This is
        /// essentially a "safe cast".
        /// </summary>
        /// <returns></returns>
        public ImageAdapter GetImageAdapter()
        {
            if (Type == AdapterTypeEnum.Image)
                return (ImageAdapter)adapterIfAttached();
            else
                throw new AdapterOperationException("Tried to safe cast to an ImageAdapter");
        }

        ///// <summary>
        ///// Returns the Speech adapter contained in this AdapterSpec, but only
        ///// if the adapter is in fact a Speech type.  If it is not a
        ///// SpeechAdapter, an InvalidCastException will be thrown.  This is
        ///// essentially a "safe cast".
        ///// </summary>
        ///// <returns></returns>
        //public SpeechAdapter GetSpeechAdapter()
        //{
        //    if (Type == AdapterTypeEnum.Speech)
        //        return (SpeechAdapter)adapterIfAttached();
        //    else
        //        throw new AdapterOperationException("Tried to safe cast to a SpeechAdapter");
        //}

        private IAdapter adapterIfAttached()
        {
            // Only access Adapter once because it is read/write lock-protected.
            IAdapter adapter = Adapter;
            if (adapter == null)
                throw new UnattachedAdapter();
            else
                return adapter;
        }

        //private static string[] adapterTypeNames = 
        //{
        //    "Vector",
        //    "Drive",
        //    "Image",
        //};
        ///// <summary>
        ///// The list of string adapter type names
        ///// </summary>
        //public static string[] AdapterTypeNames { get { return adapterTypeNames; } }

        private static string[] knownContracts = 
        {
            Myro.Services.Generic.Vector.Proxy.Contract.Identifier,
            Microsoft.Robotics.Services.Drive.Proxy.Contract.Identifier,
            Microsoft.Robotics.Services.WebCam.Proxy.Contract.Identifier,
        };
        public static string[] KnownContracts { get { return knownContracts; } }

        //private static Dictionary<AdapterTypeEnum, string> byType = new Dictionary<AdapterTypeEnum, string>();
        //private static Dictionary<string, AdapterTypeEnum> byName = new Dictionary<string, AdapterTypeEnum>();
        private static Dictionary<string, AdapterTypeEnum> byContract = new Dictionary<string,AdapterTypeEnum>();

        //public static AdapterTypeEnum[] AdapterTypes { get; private set; }

        static AdapterSpec()
        {
            for (int i = 0; i < knownContracts.Length; i++)
            {
                //byType.Add((AdapterTypeEnum)i, adapterTypeNames[i]);
                //byName.Add(adapterTypeNames[i], (AdapterTypeEnum)i);
                byContract.Add(knownContracts[i], (AdapterTypeEnum)i);
            }
        }

        ///// <summary>
        ///// Convert a runtime adapter type to a string adapter type name.
        ///// </summary>
        ///// <param name="type">The runtime adapter type</param>
        ///// <returns>The string adapter type name</returns>
        //public static string GetTypeName(AdapterTypeEnum type)
        //{
        //    string ret = byType[type];
        //    if (ret != null)
        //        return ret;
        //    else
        //        throw new NoSuchAdapter("Adapter type invalid");
        //}

        ///// <summary>
        ///// Convert a string adapter type name to a runtime adapter type.
        ///// </summary>
        ///// <param name="name">The string adapter type name</param>
        ///// <returns>The runtime adapter type</returns>
        //public static AdapterTypeEnum GetType(string name)
        //{
        //    try
        //    {
        //        return byName[name];
        //    }
        //    catch (KeyNotFoundException)
        //    {
        //        throw new NoSuchAdapter("Adapter name invalid");
        //    }
        //}

    }

    /// <summary>
    /// This exception is thrown when a string description of an adapter type,
    /// such as "Vector" or "Drive", does not match any actual adapter type.
    /// This exception would be thrown, for instance, if one called
    /// AdapterFactory.GetType("Flowers").  In the Myro framework, this could
    /// occur if an invalid adapter type is present in the config file.
    /// </summary>
    public class NoSuchAdapter : Exception
    {
        public NoSuchAdapter(string reason)
            : base(reason)
        {
        }
    }

    public class UnattachedAdapter : Exception
    {
        public UnattachedAdapter()
            : base()
        {
        }
    }

    /// <summary>
    /// This enum contains the runtime adapter types.  Each one is associated
    /// with a string name for the adapter type (which are stored in
    /// AdapterFactory.adapterTypeNames).  See AdapterFactory.GetTypeName()
    /// and AdapterFactory.GetType().
    /// </summary>
    public enum AdapterTypeEnum
    {
        Vector,
        Drive,
        Image
    }

}
