using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using Microsoft.Dss.ServiceModel.Dssp;
using Myro.Utilities;
using Microsoft.Dss.Hosting;
using Microsoft.Ccr.Core;
using W3C.Soap;

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
    public class AdapterSpec<T> : AdapterSpecN where T : IAdapter
    {
        ///// <summary>
        ///// The runtime type of the adapter.
        ///// </summary>
        //public AdapterTypeEnum Type { get; private set; }

        /// <summary>
        /// The Myro name of the adapter.
        /// </summary>
        public string Name { get; private set; }

        private List<IAdapterFactory> adapterFactories;

        ///// <summary>
        ///// The DSS service record, as constructed from the config file.  This
        ///// is only a "filter" to find the right service in the DSS directory,
        ///// and may contain a relative URI.
        ///// </summary>
        //public ServiceInfoType ServiceConfig { get; private set; }

        private ReaderWriterLock adapterLock = new ReaderWriterLock();
        private T adapterObject = default(T);
        /// <summary>
        /// This is the actual adapter encapsulated by this AdapterSpec.  If
        /// the adapter is not yet created, this class will try to create it
        /// using each of the adapter factories in AdapterFactories.  If no
        /// adapter supports the service, this property will throw a
        /// NoAdapterFoundException.  If there is a fault or error while 
        /// connecting to the service, this property will throw a 
        /// ServiceNotAvailableException.  It can also throw an
        /// InvalidCastException.
        /// </summary>
        public T Adapter
        {
            get
            {
                T ret;
                adapterLock.AcquireReaderLock(-1);
                if (adapterObject == null)
                {
                    adapterLock.UpgradeToWriterLock(-1);
                    try
                    {
                        adapterObject = tryCreateAdapter();
                        ret = adapterObject;
                    }
                    catch (NoAdapterFoundException e)
                    {
                        throw e;
                    }
                    catch (Exception e)
                    {
                        throw new ServiceNotAvailableException(e);
                    }
                    finally
                    {
                        adapterLock.ReleaseWriterLock();
                    }
                }
                else
                {
                    ret = adapterObject;
                    adapterLock.ReleaseReaderLock();
                }
                return ret;
            }
            //set
            //{
            //    adapterLock.AcquireWriterLock(-1);
            //    adapterObject = value;
            //    adapterLock.ReleaseWriterLock();
            //}
        }


        /// <summary>
        /// Returns the adapter object, if it is attached.  If not attached, it returns null.
        /// </summary>
        /// <returns></returns>
        public IAdapter AdapterIfAttached
        {
            get
            {
                adapterLock.AcquireReaderLock(-1);
                try
                {
                    if (adapterObject != null)
                        return adapterObject;
                    else
                        return null;
                }
                finally
                {
                    adapterLock.ReleaseReaderLock();
                }
            }
        }

        ///// <summary>
        ///// Constructor, with the runtime adapter type, the Myro name, and the
        ///// service record from the config file.
        ///// </summary>
        ///// <param name="type">The runtime adapter type</param>
        ///// <param name="name">The Myro name</param>
        ///// <param name="serviceConfig">The service record from the config file</param>
        //public AdapterSpec(AdapterTypeEnum type, string name, ServiceInfoType serviceConfig)
        //{
        //    Type = type;
        //    Name = name;
        //    Adapter = null;
        //    ServiceConfig = serviceConfig;
        //}

        public AdapterSpec(string name, List<IAdapterFactory> adapterFactories)
        {
            Name = name;
            this.adapterFactories = adapterFactories;
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

        ///// <summary>
        ///// Create an unattachable AdapterSpec.
        ///// </summary>
        ///// <param name="type"></param>
        ///// <param name="name"></param>
        //public AdapterSpec(AdapterTypeEnum type, string name)
        //    : this(type, name, new ServiceInfoType("nocontract"))
        //{
        //}


        private T tryCreateAdapter()
        {
            lock (this)
            {
                try
                {
                    // Query the service initially
                    PortSet<LookupResponse, Fault> responsePort = new PortSet<LookupResponse, Fault>();
                    DssEnvironment.ServiceForwarderUnknownType(new Uri("dssp.tcp://localhost/" + Name)).PostUnknownType(
                        new DsspDefaultLookup() { Body = new LookupRequestType(), ResponsePort = responsePort });
                    ServiceInfoType responseRecord = RSUtils.ReceiveSync(responsePort, Myro.Utilities.Params.DefaultRecieveTimeout);

                    // Try to find a working contract for each adapter
                    foreach (var factory in adapterFactories)
                    {
                        try
                        {
                            ServiceInfoType serviceRecord = RSUtils.FindCompatibleContract(responseRecord, factory.SupportedContracts);
                            T ret = (T)factory.Create(serviceRecord);
                            Console.WriteLine("Attached to " + serviceRecord.Service + " as \"" + Name + "\"");
                            return ret;
                        }
                        catch (NoContractFoundException) { }
                    }

                    // If we haven't returned already in the loop, we didn't find
                    // an adapter that works.
                    throw new NoAdapterFoundException(responseRecord);
                }
                catch (NoAdapterFoundException e)
                {
                    throw e;
                }
                catch (Exception e)
                {
                    DssEnvironment.LogError("Error querying or attaching to " + "dssp.tcp://localhost/" + Name + ": " + e.ToString());
                    throw e;
                }
            }
        }

        ///// <summary>
        ///// Tests whether this adapter is attached.
        ///// </summary>
        ///// <returns></returns>
        //public bool isAttached()
        //{
        //    return Adapter != null;
        //}

        ///// <summary>
        ///// Returns the Vector adapter contained in this AdapterSpec, but only
        ///// if the adapter is in fact a Vector type.  If it is not a
        ///// VectorAdapter, an AdapterOperation exception will be thrown.  This is
        ///// essentially a "safe cast".
        ///// </summary>
        ///// <returns></returns>
        //public VectorAdapter GetVectorAdapter()
        //{
        //    if (Type == AdapterTypeEnum.Vector)
        //        return (VectorAdapter)adapterIfAttached();
        //    else
        //        throw new AdapterOperationException("Tried to safe cast to a VectorAdapter");
        //}

        ///// <summary>
        ///// Returns the Drive adapter contained in this AdapterSpec, but only
        ///// if the adapter is in fact a Drive type.  If it is not a
        ///// DriveAdapter, an InvalidCastException will be thrown.  This is
        ///// essentially a "safe cast".
        ///// </summary>
        ///// <returns></returns>
        //public DriveAdapter GetDriveAdapter()
        //{
        //    if (Type == AdapterTypeEnum.Drive)
        //        return (DriveAdapter)adapterIfAttached();
        //    else
        //        throw new AdapterOperationException("Tried to safe cast to a DriveAdapter");
        //}

        ///// <summary>
        ///// Returns the Image adapter contained in this AdapterSpec, but only
        ///// if the adapter is in fact a Image type.  If it is not a
        ///// ImageAdapter, an InvalidCastException will be thrown.  This is
        ///// essentially a "safe cast".
        ///// </summary>
        ///// <returns></returns>
        //public ImageAdapter GetImageAdapter()
        //{
        //    if (Type == AdapterTypeEnum.Image)
        //        return (ImageAdapter)adapterIfAttached();
        //    else
        //        throw new AdapterOperationException("Tried to safe cast to an ImageAdapter");
        //}

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

        //private IAdapter adapterIfAttached()
        //{
        //    // Only access Adapter once because it is read/write lock-protected.
        //    IAdapter adapter = Adapter;
        //    if (adapter == null)
        //        throw new UnattachedAdapter();
        //    else
        //        return adapter;
        //}

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

        //private static string[] knownContracts = 
        //{
        //    Myro.Services.Generic.Vector.Proxy.Contract.Identifier,
        //    Microsoft.Robotics.Services.Drive.Proxy.Contract.Identifier,
        //    Microsoft.Robotics.Services.WebCam.Proxy.Contract.Identifier,
        //};
        //public static string[] KnownContracts { get { return knownContracts; } }

        //private static Dictionary<AdapterTypeEnum, string> byType = new Dictionary<AdapterTypeEnum, string>();
        //private static Dictionary<string, AdapterTypeEnum> byName = new Dictionary<string, AdapterTypeEnum>();
        //private static Dictionary<string, AdapterTypeEnum> byContract = new Dictionary<string, AdapterTypeEnum>();

        //public static AdapterTypeEnum[] AdapterTypes { get; private set; }

        //static AdapterSpec()
        //{
        //    for (int i = 0; i < knownContracts.Length; i++)
        //    {
        //        //byType.Add((AdapterTypeEnum)i, adapterTypeNames[i]);
        //        //byName.Add(adapterTypeNames[i], (AdapterTypeEnum)i);
        //        byContract.Add(knownContracts[i], (AdapterTypeEnum)i);
        //    }
        //}

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
    /// This exception is thrown when Myro cannot find an adapter that works
    /// with the service it is trying to connect to.  The adapters tried are
    /// stored in the adapterFactories list.
    /// </summary>
    public class NoAdapterFoundException : Exception
    {
        public NoAdapterFoundException(ServiceInfoType record)
            : base("Could not find an adapter for " + record.Service + ", with contract " + record.Contract)
        {
        }
    }

    public class ServiceNotAvailableException : Exception
    {
        public ServiceNotAvailableException(Exception innerException)
            : base("Could not connect to service", innerException)
        { }
    }

    //public class UnattachedAdapter : Exception
    //{
    //    public UnattachedAdapter()
    //        : base()
    //    {
    //    }
    //}

    ///// <summary>
    ///// This enum contains the runtime adapter types.  Each one is associated
    ///// with a string name for the adapter type (which are stored in
    ///// AdapterFactory.adapterTypeNames).  See AdapterFactory.GetTypeName()
    ///// and AdapterFactory.GetType().
    ///// </summary>
    //public enum AdapterTypeEnum
    //{
    //    Vector,
    //    Drive,
    //    Image
    //}

}
