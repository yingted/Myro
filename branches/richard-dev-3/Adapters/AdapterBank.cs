// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.XPath;
using System.Xml;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Hosting;
using W3C.Soap;
using System.IO;
using Microsoft.Ccr.Core;
using System.Threading;
using System.Xml.Serialization;
using Microsoft.Dss.Core.Manifest;
using dirProxy = Microsoft.Dss.Services.Directory.Proxy;
using Myro.Utilities;
using manif = Microsoft.Dss.Services.ManifestLoader.Proxy;

namespace Myro.Adapters
{

    /// <summary>
    /// This class is the main interface between the higher-level Myro API
    /// and the service adapters.
    /// 
    /// It allows AdapterSpec's to be retrieved by name.
    /// </summary>
    public class AdapterBank
    {
        private Dictionary<string, AdapterTokenN> adapterNames = new Dictionary<string, AdapterTokenN>();
        
        /// <summary>
        /// This is the list of adapter factories that will be tried in order
        /// when connecting to a new service.  The Myro.Robot object will set
        /// up several default adapters, but you may add new adapter factories
        /// to this list at runtime if you create your own.
        /// </summary>
        public List<IAdapterFactory> AdapterFactories { get; private set; }

        /// <summary>
        /// Call the constructor with the initial list of adapter factories.
        /// You can add additional factories (for custom adapters) by appending
        /// to the AdapterFactories property at runtime.
        /// 
        /// You must call Dispose() on exiting to clean up.
        /// </summary>
        /// <param name="adapterFactories"></param>
        public AdapterBank(List<IAdapterFactory> adapterFactories)
        {
            this.AdapterFactories = adapterFactories;
        }

        /// <summary>
        /// You must call this method on exiting.  It disposes each
        /// adapter.
        /// </summary>
        public void Dispose()
        {
            foreach (var a in adapterNames.Values)
            {
                IAdapter adapter = a.AdapterIfAttached;
                if (adapter != null)
                    adapter.Dispose();
            }
        }

        /// <summary>
        /// Return the AdapterSpec associated with the name.  This call will never fail.
        /// Attempting to use the Adapter will fail if it cannot connect to the service.
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        public AdapterToken<T> GetAdapterSpec<T>(string name) where T:IAdapter
        {
            lock (this)
            {
                try
                {
                    return (AdapterToken<T>)adapterNames[name];
                }
                catch (KeyNotFoundException)
                {
                    AdapterToken<T> adapterSpec = new AdapterToken<T>(name, AdapterFactories);
                    adapterNames.Add(name, (AdapterTokenN)adapterSpec);
                    return adapterSpec;
                }
            }
        }

        //    public void WaitForAdapters(TimeSpan timeout)
        //    {
        //        if (adaptersReady.WaitOne(timeout) == false)
        //            throw new TimeoutException("Timed out waiting for adapters to attach");
        //        //var dirPort = DssEnvironment.ServiceForwarder<dirProxy.DirectoryPort>(new Uri("dssp.tcp://localhost/directory"));
        //        //var resp = RSUtils.RecieveSync(dirPort.Query(new dirProxy.QueryRequest()
        //        //{
        //        //    QueryRecord = new ServiceInfoType(Myro.Services.Generic.Vector.Proxy.Contract.Identifier)
        //        //}));
        //        //Console.WriteLine("Length " + resp.RecordList.Length);
        //    }

        //    private void readConfig(string configFile)
        //    {
        //        XPathNavigator nav = new XPathDocument(configFile).CreateNavigator();

        //        //// Add plain services to list
        //        //XPathNodeIterator startservices = nav.Select("//StartService");
        //        //foreach (XPathNavigator node in startservices)
        //        //    services.Add(new ServiceInfoType(getChild(node, "Contract"), getChild(node, "Service")));

        //        // Add manifests to list
        //        XPathNodeIterator manifestNodes = nav.Select("//Manifest");
        //        foreach (XPathNavigator node in manifestNodes)
        //            manifests.Add(node.GetAttribute("file", ""));

        //        //// Add adapters to list, and to dictionary
        //        //XPathNodeIterator adapters = nav.Select("//Adapter");
        //        //Console.WriteLine(adapters.Count + " adapters in config");
        //        //foreach (XPathNavigator node in adapters)
        //        //{
        //        //    // Get the relevant info for this adapter

        //        //    // Attributes
        //        //    string sType = node.GetAttribute("type", "");
        //        //    AdapterTypeEnum type = AdapterSpec.GetType(sType);
        //        //    string name = node.GetAttribute("name", "");

        //        //    // Children
        //        //    string contract = getChild(node, "Contract", true);
        //        //    string service = getChild(node, "Service");
        //        //    //NameTable names = new NameTable();
        //        //    //XmlNamespaceManager nsman = new XmlNamespaceManager(new NameTable());
        //        //    //nsman.AddNamespace("mani", "http://schemas.microsoft.com/xw/2004/10/manifest.html");
        //        //    //XPathNavigator srNav = node.SelectSingleNode(XPathExpression.Compile("./mani:ServiceRecordType", nsman));
        //        //    //if (srNav != null)
        //        //    //{
        //        //    //    XmlReader reader = srNav.ReadSubtree();
        //        //    //    XmlSerializer ser = new XmlSerializer(typeof(ServiceRecordType));
        //        //    //    ServiceRecordType record = (ServiceRecordType)ser.Deserialize(reader);
        //        //    //}

        //        //    // Add elements to the service array and dictionary
        //        //    if (adapterNames.ContainsKey(name))
        //        //        throw new ConfigException("Duplicate adapter name \"" + name + "\"");
        //        //    else
        //        //    {
        //        //        // Add a null adapter for now.  An adapter will be created when the service is actually started
        //        //        ServiceInfoType serviceInfo = new ServiceInfoType(contract, service);
        //        //        AdapterSpec adapterInfo = new AdapterSpec(type, name, serviceInfo);
        //        //        adapterNames.Add(name, adapterInfo);
        //        //        //adapterServices.Add(service, adapterInfo);
        //        //        //services.Add(serviceInfo);
        //        //    }
        //        //}
        //    }

        //    private void subscribeDirectory()
        //    {
        //        var dirPort = DssEnvironment.ServiceForwarder<dirProxy.DirectoryPort>(new Uri("http://localhost/directory"));
        //        var resPort = new dirProxy.DirectoryPort();
        //        Fault error = null;
        //        EventWaitHandle signal = new EventWaitHandle(false, EventResetMode.ManualReset);
        //        Arbiter.Activate(DssEnvironment.TaskQueue,
        //            Arbiter.Choice<SubscribeResponseType, Fault>(
        //                dirPort.Subscribe(resPort, null),
        //                delegate(SubscribeResponseType success)
        //                {
        //                    Console.WriteLine("AdapterBank subscribed to directory service");
        //                    signal.Set();
        //                },
        //                delegate(Fault failure)
        //                {
        //                    error = failure;
        //                    signal.Set();
        //                }));
        //        signal.WaitOne();
        //        if (error != null)
        //            throw new Exception("Could not subscribe to directory service: " + error.Reason);
        //        Arbiter.Activate(DssEnvironment.TaskQueue,
        //            Arbiter.Receive<dirProxy.Insert>(true, resPort, directoryInsertHandler));
        //        updateAdapters();
        //    }

        //    private void directoryInsertHandler(dirProxy.Insert insert)
        //    {
        //        Console.WriteLine("Directory insert for " + insert.Body.Record.Service);
        //        foreach (var partner in insert.Body.Record.PartnerList)
        //        {
        //            if (partner.Name.Name == "PrimaryContractService")
        //                Console.WriteLine("Service " + insert.Body.Record.Service + " has primary contract service " + partner.Service);
        //            if (partner.Name.Name == "AlternateContractService")
        //                Console.WriteLine("Service " + insert.Body.Record.Service + " has alternate contract service " + partner.Service);
        //        }
        //        //Console.WriteLine("AdapterBank got directory insert: " + insert.Body.Record.Service);
        //        updateAdapters();
        //    }

        //    private void updateAdapters()
        //    {
        //        var dirPort = DssEnvironment.ServiceForwarder<dirProxy.DirectoryPort>(new Uri("http://localhost/directory"));
        //        Arbiter.Activate(DssEnvironment.TaskQueue,
        //            Arbiter.Choice<dirProxy.GetResponse, Fault>(
        //                dirPort.Get(),
        //                delegate(dirProxy.GetResponse success)
        //                {
        //                    // See if each service matches a known one from the config file, if so, make an adapter
        //                    //Console.WriteLine("Checking " + success.RecordList.Length + " services");
        //                    foreach (var rec in success.RecordList)
        //                    {
        //                        //Uri foundUri = new Uri(rec.Service);
        //                        AdapterSpec adapterSpec = null;
        //                        //Console.WriteLine("Checking " + rec.Service);
        //                        foreach (var adapter in adapterNames.Values)
        //                        {
        //                            //Console.WriteLine("Comparing " + rec.Service + " to " + adapter.ServiceInfo.Service);
        //                            //Uri testUri = new Uri(adapter.ServiceInfo.Service);
        //                            //if (Uri.Compare(foundUri, testUri, UriComponents.Path, UriFormat.UriEscaped, StringComparison.InvariantCultureIgnoreCase) == 0)
        //                            if (rec.Contract.ToLower().CompareTo(adapter.ServiceConfig.Contract.ToLower()) == 0)
        //                            //rec.Service.EndsWith(adapter.ServiceConfig.Service))
        //                            {
        //                                //Console.WriteLine("* Assigned * " + rec.Service + " to " + adapter.ServiceInfo.Service);
        //                                if (adapterSpec == null)
        //                                    adapterSpec = adapter;
        //                                else
        //                                    Console.WriteLine("WARNING: duplicate service: " + rec.Service + " (already had one for " + adapter.ServiceConfig.Service + ")");
        //                            }
        //                        }
        //                        if (adapterSpec != null)
        //                        {
        //                            adapterSpec.tryAttachAdapter(rec);
        //                        }
        //                    }
        //                    checkAllAttached();
        //                },
        //                delegate(Fault fault)
        //                {
        //                    Console.WriteLine("AdapterBank: Fault querying directory: " + fault.Reason);
        //                }));
        //    }

        //    public void startServices()
        //    {
        //        //string manifestLoader = null;
        //        //ManualResetEvent signal = new ManualResetEvent(false);
        //        //Arbiter.Activate(DssEnvironment.TaskQueue,
        //        //    Arbiter.Choice<ServiceInfoType,Fault>(
        //        //        DssEnvironment.DirectoryQuery(manif.Contract.Identifier),
        //        //        delegate(ServiceInfoType record) {
        //        //            manifestLoader = record.Service;
        //        //            signal.Set();
        //        //        },
        //        //        delegate(Fault failure)
        //        //        {
        //        //            Console.WriteLine("*** Fault finding manifest loader *** " + failure.Reason[0].Value);
        //        //            signal.Set();
        //        //        }));
        //        //signal.WaitOne();
        //        //if(manifestLoader != null) {
        //        //    manif.ManifestLoaderPort mPort = DssEnvironment.ServiceForwarder<manif.ManifestLoaderPort>(new Uri(manifestLoader));
        //        //    manif.InsertRequest req = new manif.InsertRequest();
        //        //    req.Manifest = Microsoft.Dss.Services.ManifestLoaderClient.ValidateManifest
        //        //    req.Manifest.C
        //        //    mPort.Insert(
        //        //foreach(var manifest in manifests)
        //        //    mPort.Post(

        //        // Build a list of the "StartService"s and the "Adapter" services
        //        List<ServiceInfoType> allServices = new List<ServiceInfoType>(services);
        //        foreach (var adapter in adapterNames.Values)
        //            if (adapter.ServiceConfig.Contract != null)
        //                allServices.Add(adapter.ServiceConfig);

        //        // Start them all
        //        foreach (var service in allServices)
        //            if (service.Contract != null)
        //            {
        //                var thisService = service;
        //                Arbiter.Activate(DssEnvironment.TaskQueue,
        //                    Arbiter.Choice<CreateResponse, Fault>(
        //                    DssEnvironment.CreateService(service),
        //                    delegate(CreateResponse success)
        //                    {
        //                        Console.WriteLine("* Created * " + thisService.Service);
        //                    },
        //                    delegate(Fault failure)
        //                    {
        //                        Console.WriteLine("*** FAULT *** creating " + thisService.Service);
        //                        foreach (var reason in failure.Reason)
        //                            Console.WriteLine("*** " + reason.Value);
        //                    }));
        //            }
        //    }

        //    private void checkAllAttached()
        //    {
        //        bool ret = true;
        //        foreach (var adapter in adapterNames.Values)
        //            if (adapter.isAttached() == false)
        //                ret = false;
        //        if (ret == true)
        //            adaptersReady.Set();
        //    }

        //    private static string getChild(XPathNavigator node, string name, bool missingok)
        //    {
        //        string ret;
        //        XPathNodeIterator children = node.SelectChildren(name, "");
        //        if (children.MoveNext())
        //        {
        //            ret = children.Current.Value;
        //            if (children.MoveNext())
        //                throw new ConfigException("Duplicate <" + name + "> at " + node.Name);
        //            else
        //                return ret;
        //        }
        //        else
        //        {
        //            if (missingok)
        //                return null;
        //            else
        //                throw new ConfigException("Missing <" + name + "> at " + node.Name);
        //        }
        //    }

        //    private static string getChild(XPathNavigator node, string name)
        //    {
        //        return getChild(node, name, false);
        //    }
    }

}
