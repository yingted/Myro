using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.XPath;
using System.Xml;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Hosting;
using W3C.Soap;
using Microsoft.Ccr.Core;
using System.Threading;
using dirProxy = Microsoft.Dss.Services.Directory.Proxy;

namespace Myro.Adapters
{
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
        public UnknownAdapterNameException(string name) : base("Unknown adapter name: \"" + name + "\"")
        {
        }
    }


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
        protected List<ServiceInfoType> services;
        protected Dictionary<string, AdapterSpec> adapterNames;
        //protected Dictionary<string, AdapterSpec> adapterServices;

        public AdapterBank(string configFile, bool autoStartServices)
        {
            readConfig(configFile);
            subscribeDirectory();
            if (autoStartServices)
                startServices();
        }

        /// <summary>
        /// Return the AdapterSpec associated with the name, as specified in
        /// the config file.
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

        protected void readConfig(string configFile)
        {
            services = new List<ServiceInfoType>();
            //adapterTypes = new Dictionary<AdapterTypes,ArrayList>();
            adapterNames = new Dictionary<string, AdapterSpec>();

            XPathNavigator nav = new XPathDocument(configFile).CreateNavigator();

            // Add plain services to list
            XPathNodeIterator startservices = nav.Select("//StartService");
            foreach (XPathNavigator node in startservices)
                services.Add(new ServiceInfoType(getChild(node, "Contract"), getChild(node, "Service")));

            // Add adapters to list, and to dictionary
            XPathNodeIterator adapters = nav.Select("//Adapter");
            Console.WriteLine(adapters.Count + " adapters in config");
            foreach (XPathNavigator node in adapters)
            {
                // Get the relevant info for this adapter

                // Attributes
                string sType = node.GetAttribute("type", "");
                AdapterTypeEnum type = AdapterFactory.GetType(sType);
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
            Arbiter.ExecuteToCompletion(DssEnvironment.TaskQueue,
                Arbiter.Choice<SubscribeResponseType, Fault>(
                    dirPort.Subscribe(resPort, null),
                    delegate(SubscribeResponseType success)
                    {
                        Console.WriteLine("AdapterBank subscribed to directory service");
                    },
                    delegate(Fault failure)
                    {
                        throw new Exception("Could not subscribe to directory service: " + failure.Reason);
                    }));
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<dirProxy.Insert>(true, resPort, directoryInsertHandler));
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
            Arbiter.ExecuteToCompletion(DssEnvironment.TaskQueue,
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
                                if (rec.Service.EndsWith(adapter.ServiceConfig.Service))
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
                                AdapterFactory.CreateAdapterIfNeeded(adapterSpec, rec);
                            }
                        }
                    },
                    delegate(Fault fault)
                    {
                        Console.WriteLine("AdapterBank: Fault querying directory: " + fault.Reason);
                    }));
        }

        private void startServices()
        {
            // Build a list of the "StartService"s and the "Adapter" services
            List<ServiceInfoType> allServices = new List<ServiceInfoType>(services);
            foreach (var adapter in adapterNames.Values)
                if (adapter.ServiceConfig.Contract != null)
                    allServices.Add(adapter.ServiceConfig);

            // Start them all
            foreach (var service in allServices)
                if (service.Contract != null)
                    Arbiter.Activate(DssEnvironment.TaskQueue,
                        Arbiter.Choice<CreateResponse, Fault>(
                        DssEnvironment.CreateService(service),
                        delegate(CreateResponse success)
                        {
                            Console.WriteLine("* Created * " + success.Service);
                        },
                        delegate(Fault failure)
                        {
                            Console.WriteLine("*** FAULT *** creating " + service.Service + ": " + failure.Reason);
                        }));

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
