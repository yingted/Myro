// Copyright (c) Microsoft Corporation.  All rights reserved.

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
    public class AdapterToken<T> : AdapterTokenN where T : IAdapter
    {
        /// <summary>
        /// The Myro "short name" of the adapter.
        /// </summary>
        public string Name { get; private set; }

        private List<IAdapterFactory> adapterFactories;

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
        /// 
        /// You should not cache the adapter.  Instead, always use this property
        /// to access it.  This class caches the adapter internally, so accessing
        /// this property is fast.
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


        /// <summary>
        /// AdapterBank calls this constructor.  This never fails.  Accessing the Adapter
        /// property will fail if the adapter cannot be created.
        /// </summary>
        /// <param name="name"></param>
        /// <param name="adapterFactories"></param>
        internal AdapterToken(string name, List<IAdapterFactory> adapterFactories)
        {
            Name = name;
            this.adapterFactories = adapterFactories;
        }

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
                catch (NoAdapterFoundException)
                {
                    throw;
                }
                catch (Exception e)
                {
                    DssEnvironment.LogError("Error querying or attaching to " + "dssp.tcp://localhost/" + Name + ": " + e.ToString());
                    throw;
                }
            }
        }
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

    /// <summary>
    /// This exception is thrown when connecting to the DSS service for an adapter fails.
    /// </summary>
    public class ServiceNotAvailableException : Exception
    {
        public ServiceNotAvailableException(Exception innerException)
            : base("Could not connect to service", innerException)
        { }
    }
}
