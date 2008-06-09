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
using dirProxy = Microsoft.Dss.Services.Directory.Proxy;
using Myro.Utilities;
using manif = Microsoft.Dss.Services.ManifestLoader.Proxy;

namespace Myro.Adapters
{
    #region Exception Definitions
    /// <summary>
    /// This exception is thrown when there is a problem with the content of
    /// the config file.  The exception will print a line containing the 
    /// reason for the exception.
    /// </summary>
    public class ConfigException : Exception
    {
        public ConfigException(string reason)
            : base(reason)
        {
            Console.WriteLine("*** Config Exception *** " + reason);
        }
    }

    /// <summary>
    /// This exception is thrown if an unknown adapter is requested from the
    /// AdapterBank.
    /// </summary>
    public class UnknownAdapterNameException : Exception
    {
        public UnknownAdapterNameException(string name)
            : base("Unknown adapter name: \"" + name + "\"")
        {
        }
    }
    #endregion

    /// <summary>
    /// This class is the main interface between the higher-level Myro API
    /// and the service adapters.
    /// 
    /// This class deals with managing the "bank" of adapters at runtime.  It
    /// parses the config file, creating AdapterSpec's for each Adapter
    /// section, and plain ServiceInfoType's for each StartService section.
    /// 
    /// It subscribes to the DSS directory, watching for services to start
    /// that match those from the config file, attaching adapters as soon as
    /// they do start.  It can also automatically start the services from the
    /// config file.
    /// 
    /// It also allows AdapterSpec's to be retrieved by name.
    /// </summary>
    public class AdapterBank
    {
        protected List<ServiceInfoType> services = new List<ServiceInfoType>();
        protected List<string> manifests = new List<string>();
        protected Dictionary<string, AdapterSpec> adapterNames = new Dictionary<string, AdapterSpec>();
        //protected Dictionary<string, AdapterSpec> adapterServices;
        ManualResetEvent adaptersReady = new ManualResetEvent(false);

        public AdapterBank(string configFile, bool autoStartServices)
        {
            readConfig(configFile);
            Console.WriteLine("Initializing DSS environment...");
            if (manifests.Count > 0)
            {
                IEnumerable<string> manifestFullPaths =
                    from m in manifests
                    select "file://" + Path.GetFullPath(m);
                DssEnvironment.Initialize(50000, 50001, manifestFullPaths.ToArray());
            }
            else
                DssEnvironment.Initialize(50000, 50001);
            Console.WriteLine("DSS environment initialized");
            subscribeDirectory();
            //if (autoStartServices)
            //    startServices();
        }

        /// <summary>
        /// Return the AdapterSpec associated with the name, as specified in
        /// the config file.  Throws UnknownAdapterNameException.
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        public AdapterSpec GetAdapterSpec(string name)
        {
            try
            {
                return adapterNames[name];
            }
            catch (KeyNotFoundException e)
            {
                throw new UnknownAdapterNameException(name);
            }
        }

        public void WaitForAdapters(TimeSpan timeout)
        {
            if (adaptersReady.WaitOne(timeout) == false)
                throw new TimeoutException("Timed out waiting for adapters to attach");
        }

        protected void readConfig(string configFile)
        {
            XPathNavigator nav = new XPathDocument(configFile).CreateNavigator();

            // Add plain services to list
            XPathNodeIterator startservices = nav.Select("//StartService");
            foreach (XPathNavigator node in startservices)
                services.Add(new ServiceInfoType(getChild(node, "Contract"), getChild(node, "Service")));

            // Add manifests to list
            XPathNodeIterator manifestNodes = nav.Select("//Manifest");
            foreach (XPathNavigator node in manifestNodes)
                manifests.Add(node.GetAttribute("file", ""));

            // Add adapters to list, and to dictionary
            XPathNodeIterator adapters = nav.Select("//Adapter");
            Console.WriteLine(adapters.Count + " adapters in config");
            foreach (XPathNavigator node in adapters)
            {
                // Get the relevant info for this adapter

                // Attributes
                string sType = node.GetAttribute("type", "");
                AdapterTypeEnum type = AdapterSpec.GetType(sType);
                string name = node.GetAttribute("name", "");

                // Children
                string contract = getChild(node, "Contract", true);
                string service = getChild(node, "Service");

                // Add elements to the service array and dictionary
                if (adapterNames.ContainsKey(name))
                    throw new ConfigException("Duplicate adapter name \"" + name + "\"");
                else
                {
                    // Add a null adapter for now.  An adapter will be created when the service is actually started
                    ServiceInfoType serviceInfo = new ServiceInfoType(contract, service);
                    AdapterSpec adapterInfo = new AdapterSpec(type, name, serviceInfo);
                    adapterNames.Add(name, adapterInfo);
                    //adapterServices.Add(service, adapterInfo);
                    //services.Add(serviceInfo);
                }
            }
        }

        protected void subscribeDirectory()
        {
            var dirPort = DssEnvironment.ServiceForwarder<dirProxy.DirectoryPort>(new Uri("http://localhost:50000/directory"));
            var resPort = new dirProxy.DirectoryPort();
            Fault error = null;
            EventWaitHandle signal = new EventWaitHandle(false, EventResetMode.ManualReset);
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice<SubscribeResponseType, Fault>(
                    dirPort.Subscribe(resPort, null),
                    delegate(SubscribeResponseType success)
                    {
                        Console.WriteLine("AdapterBank subscribed to directory service");
                        signal.Set();
                    },
                    delegate(Fault failure)
                    {
                        error = failure;
                        signal.Set();
                    }));
            signal.WaitOne();
            if (error != null)
                throw new Exception("Could not subscribe to directory service: " + error.Reason);
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<dirProxy.Insert>(true, resPort, directoryInsertHandler));
            updateAdapters();
        }

        protected void directoryInsertHandler(dirProxy.Insert insert)
        {
            //Console.WriteLine("AdapterBank got directory insert: " + insert.Body.Record.Service);
            updateAdapters();
        }

        protected void updateAdapters()
        {
            var dirPort = DssEnvironment.ServiceForwarder<dirProxy.DirectoryPort>(new Uri("http://localhost:50000/directory"));
            //Console.WriteLine("Querying directory");
            //dirProxy.QueryRequest request = new dirProxy.QueryRequest();
            //request.QueryRecord = new ServiceInfoType();
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice<dirProxy.GetResponse, Fault>(
                    dirPort.Get(),
                    delegate(dirProxy.GetResponse success)
                    {
                        // See if each service matches a known one from the config file, if so, make an adapter
                        //Console.WriteLine("Checking " + success.RecordList.Length + " services");
                        foreach (var rec in success.RecordList)
                        {
                            //Uri foundUri = new Uri(rec.Service);
                            AdapterSpec adapterSpec = null;
                            //Console.WriteLine("Checking " + rec.Service);
                            foreach (var adapter in adapterNames.Values)
                            {
                                //Console.WriteLine("Comparing " + rec.Service + " to " + adapter.ServiceInfo.Service);
                                //Uri testUri = new Uri(adapter.ServiceInfo.Service);
                                //if (Uri.Compare(foundUri, testUri, UriComponents.Path, UriFormat.UriEscaped, StringComparison.InvariantCultureIgnoreCase) == 0)
                                if (rec.Contract.ToLower().CompareTo(adapter.ServiceConfig.Contract.ToLower()) == 0)
                                    //rec.Service.EndsWith(adapter.ServiceConfig.Service))
                                {
                                    //Console.WriteLine("* Assigned * " + rec.Service + " to " + adapter.ServiceInfo.Service);
                                    if (adapterSpec == null)
                                        adapterSpec = adapter;
                                    else
                                        Console.WriteLine("WARNING: duplicate service: " + rec.Service + " (already had one for " + adapter.ServiceConfig.Service + ")");
                                }
                            }
                            if (adapterSpec != null)
                            {
                                adapterSpec.AttachAdapterIfNeeded(rec);
                            }
                        }
                        checkAllAttached();
                    },
                    delegate(Fault fault)
                    {
                        Console.WriteLine("AdapterBank: Fault querying directory: " + fault.Reason);
                    }));
        }

        public void startServices()
        {
            //string manifestLoader = null;
            //ManualResetEvent signal = new ManualResetEvent(false);
            //Arbiter.Activate(DssEnvironment.TaskQueue,
            //    Arbiter.Choice<ServiceInfoType,Fault>(
            //        DssEnvironment.DirectoryQuery(manif.Contract.Identifier),
            //        delegate(ServiceInfoType record) {
            //            manifestLoader = record.Service;
            //            signal.Set();
            //        },
            //        delegate(Fault failure)
            //        {
            //            Console.WriteLine("*** Fault finding manifest loader *** " + failure.Reason[0].Value);
            //            signal.Set();
            //        }));
            //signal.WaitOne();
            //if(manifestLoader != null) {
            //    manif.ManifestLoaderPort mPort = DssEnvironment.ServiceForwarder<manif.ManifestLoaderPort>(new Uri(manifestLoader));
            //    manif.InsertRequest req = new manif.InsertRequest();
            //    req.Manifest = Microsoft.Dss.Services.ManifestLoaderClient.ValidateManifest
            //    req.Manifest.C
            //    mPort.Insert(
            //foreach(var manifest in manifests)
            //    mPort.Post(

            // Build a list of the "StartService"s and the "Adapter" services
            List<ServiceInfoType> allServices = new List<ServiceInfoType>(services);
            foreach (var adapter in adapterNames.Values)
                if (adapter.ServiceConfig.Contract != null)
                    allServices.Add(adapter.ServiceConfig);

            // Start them all
            foreach (var service in allServices)
                if (service.Contract != null)
                {
                    var thisService = service;
                    Arbiter.Activate(DssEnvironment.TaskQueue,
                        Arbiter.Choice<CreateResponse, Fault>(
                        DssEnvironment.CreateService(service),
                        delegate(CreateResponse success)
                        {
                            Console.WriteLine("* Created * " + thisService.Service);
                        },
                        delegate(Fault failure)
                        {
                            Console.WriteLine("*** FAULT *** creating " + thisService.Service);
                            foreach (var reason in failure.Reason)
                                Console.WriteLine("*** " + reason.Value);
                        }));
                }
        }

        private void checkAllAttached()
        {
            bool ret = true;
            foreach (var adapter in adapterNames.Values)
                if (adapter.isAttached() == false)
                    ret = false;
            if (ret == true)
                adaptersReady.Set();
        }

        private static string getChild(XPathNavigator node, string name, bool missingok)
        {
            string ret;
            XPathNodeIterator children = node.SelectChildren(name, "");
            if (children.MoveNext())
            {
                ret = children.Current.Value;
                if (children.MoveNext())
                    throw new ConfigException("Duplicate <" + name + "> at " + node.Name);
                else
                    return ret;
            }
            else
            {
                if (missingok)
                    return null;
                else
                    throw new ConfigException("Missing <" + name + "> at " + node.Name);
            }
        }

        private static string getChild(XPathNavigator node, string name)
        {
            return getChild(node, name, false);
        }
    }
}
