using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Threading;
using W3C.Soap;

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Hosting;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System.Xml;
using System.Xml.XPath;
using System.Windows.Forms;
using System.IO;

using Myro.Adapters;
using IPREFoundationClasses.RobotGUI;

using drive = Microsoft.Robotics.Services.Drive.Proxy;
using directory = Microsoft.Dss.Services.Directory;
using analog = Microsoft.Robotics.Services.AnalogSensor.Proxy;
using contact = Microsoft.Robotics.Services.ContactSensor.Proxy;
using analogArray = Microsoft.Robotics.Services.AnalogSensorArray.Proxy;
using tonegen = IPREGenericContracts.ToneGenerator.Proxy;
using ledarray = IPREGenericContracts.LEDarray.Proxy;
using sonar = Microsoft.Robotics.Services.Sonar.Proxy;
using tts = Microsoft.Robotics.Technologies.Speech.TextToSpeech.Proxy;

#if false
namespace IPREFoundationClasses
{

    /// <summary>
    /// The Base Brain class. Every Robot should inherit from this class.
    /// </summary>
    public partial class RobotBrain
    {
        /// <summary>
        /// Adapter Objects indexed by AdapterTypes
        /// </summary>
        protected Dictionary<AdapterTypes, ArrayList> AdaptersDictionary;               // ArrayList = List<IAdapters>

        /// <summary>
        /// Sensor Objects indexed by SensorTypes
        /// </summary>
        protected Dictionary<SensorTypes, ArrayList> SensorsDictionary;                 // ArrayList = List<ArrayEntry>
        
        protected string manifestFile, configFile;
        protected int httpPort, soapPort, verbosity;

        /// <summary>
        /// The control GUI
        /// </summary>
        protected static Form1 robotgui;

        /// <summary>
        /// Logging Utility 
        /// </summary>
        protected static Logger logger;

        /// <summary>
        /// The Generic Contract List ONLY for Services with alternate Generic Contracts
        /// </summary>
        private string[] genericContractList = {
            drive.Contract.Identifier,
            analog.Contract.Identifier,
            contact.Contract.Identifier,
            analogArray.Contract.Identifier,
            tonegen.Contract.Identifier,
            ledarray.Contract.Identifier,
            sonar.Contract.Identifier
        };

        private string[] utilityContractList = {
            tts.Contract.Identifier
        };

        /// <summary>
        /// Overloaded Constructor
        /// </summary>
        /// <param name="configFile">Configuration File for the Robot</param>
        public RobotBrain(string configFile)
        {
            this.configFile = configFile;
            Initialize();
        }

        /// <summary>
        /// Destructor
        /// </summary>
        ~RobotBrain()
        {
            DssEnvironment.Shutdown();
        }

        /// <summary>
        /// Intialization Routine. Called by constructor.
        /// </summary>
        protected virtual void Initialize()
        {
            XPathDocument xpdoc = new XPathDocument(configFile);
            XPathNavigator xpnav = xpdoc.CreateNavigator();
            manifestFile = xpnav.SelectSingleNode("//ManifestFile/URI").Value;
            httpPort = int.Parse(xpnav.SelectSingleNode("//DssEnvironment/HttpPort").Value);
            soapPort = int.Parse(xpnav.SelectSingleNode("//DssEnvironment/SoapPort").Value);
            verbosity = int.Parse(xpnav.SelectSingleNode("//Verbosity/Level").Value);

            // Initialize Dss manifest
            String manifestFileFull = Path.Combine(Path.GetDirectoryName(Path.GetFullPath(configFile)), manifestFile);
            Console.WriteLine("Starting DSS environment with manifest: " + manifestFileFull);
            DssEnvironment.Initialize(httpPort, soapPort, manifestFileFull);
            //Console.WriteLine("Done");
            
            // Wait for 30 seconds for the DssEnvironment to completely load all services
            // TODO: utilize config file to remove the sleep
            Thread.Sleep(10000);
        }

        /// <summary>
        /// Searches service directory for a service with the specified contract. On success binds it to an Adapter object and inserts it in the 
        /// adapters lookup dictionary.
        /// </summary>
        /// <param name="contract">The contract of the service</param>
        /// <returns>Nothing</returns>
        protected virtual IEnumerator<ITask> DirectorySearch(string contract)
        {
            directory.Query dquery = new directory.Query(new directory.QueryRequestType(
                new Microsoft.Dss.ServiceModel.Dssp.ServiceInfoType(contract)));

            //Uri direcURI = DssEnvironment.FindService(directory.Contract.Identifier);
            //TODO: remove the line below once the above line works
            Uri direcURI = new Uri("http://localhost:" + httpPort + "/directory");

            directory.DirectoryPort dport = DssEnvironment.ServiceForwarder<directory.DirectoryPort>(direcURI);
            dport.Post(dquery);

            yield return Arbiter.Choice(
                dquery.ResponsePort,
                delegate(directory.QueryResponseType success)
                {
                    switch (contract)
                    {
                        case drive.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    list.Add(new DriveAdapter(success.RecordList[i].Service));
                                }
                                AdaptersDictionary.Add(AdapterTypes.DriveAdapter, list);
                                break;
                            }
                            #endregion
                        case analog.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    list.Add(new AnalogSensorAdapter(success.RecordList[i].Service));
                                }
                                AdaptersDictionary.Add(AdapterTypes.AnalogSensorAdapter, list);
                                break;
                            }
                            #endregion
                        case contact.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    list.Add(new ContactSensorArrayAdapter(success.RecordList[i].Service));
                                }
                                AdaptersDictionary.Add(AdapterTypes.ContactSensorArrayAdapter, list);
                                break;
                            }
                            #endregion
                        case analogArray.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    list.Add(new AnalogSensorArrayAdapter(success.RecordList[i].Service));
                                }
                                AdaptersDictionary.Add(AdapterTypes.AnalogSensorArrayAdapter, list);
                                break;
                            }
                            #endregion
                        case tonegen.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    list.Add(new ToneGeneratorAdapter(success.RecordList[i].Service));
                                }
                                AdaptersDictionary.Add(AdapterTypes.ToneGeneratorAdapter, list);
                                break;
                            }
                            #endregion
                        case ledarray.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    list.Add(new LEDArrayAdapter(success.RecordList[i].Service));
                                }
                                AdaptersDictionary.Add(AdapterTypes.LEDArrayAdapter, list);
                                break;
                            }
                            #endregion
                        case sonar.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    list.Add(new SonarAdapter(success.RecordList[i].Service));
                                }
                                AdaptersDictionary.Add(AdapterTypes.SonarAdapter, list);
                                break;
                            }
                            #endregion
                        case tts.Contract.Identifier:
                            #region Code
                            {
                                ArrayList list = new ArrayList();
                                for (int i = 0; i < success.RecordList.Length; i++)
                                {
                                    if (verbosity >= 2)
                                        Console.WriteLine(contract + ":" + success.RecordList[i].Service);
                                    TTSAdapter ttsadap = new TTSAdapter(success.RecordList[i].Service);
                                    cacheTTS = ttsadap;
                                    list.Add(ttsadap);
                                }
                                AdaptersDictionary.Add(AdapterTypes.TextToSpeech, list);
                                break;
                            }
                            #endregion
                    }
                },
                delegate(Fault fault) { }
            );

            yield break;
        }

        /// <summary>
        /// Calls DirectorySearch function for every generic contract known.
        /// </summary>
        protected virtual void Bind()
        {
            AdaptersDictionary = new Dictionary<AdapterTypes, ArrayList>();
            foreach (string contract in genericContractList)
            {
                Arbiter.ExecuteToCompletion(DssEnvironment.TaskQueue, new IterativeTask<string>(contract,DirectorySearch));
            }

            foreach (string contract in utilityContractList)
            {
                Arbiter.ExecuteToCompletion(DssEnvironment.TaskQueue, new IterativeTask<string>(contract, DirectorySearch));
            }
        }

        /// <summary>
        /// Parses configuration file. Inserts the sensor objects in their correct place in the sensors dictionary.
        /// </summary>
        protected virtual void ParseConfig()
        {
            SensorsDictionary = new Dictionary<SensorTypes, ArrayList>();

            XPathDocument xpdoc = new XPathDocument(configFile);
            XPathNavigator xpnav = xpdoc.CreateNavigator();
            XPathNodeIterator xpIter = xpnav.Select("//Sensor");
            while (xpIter.MoveNext())
            {
                string typeString = xpIter.Current.GetAttribute("type", xpnav.NamespaceURI).ToLower();
                string classString = xpIter.Current.GetAttribute("class", xpnav.NamespaceURI).ToLower();
                string container = null, service = null;
                int startIndex = -1, endIndex = -1, finalOrder = -1;

                XPathNodeIterator xpchildIter = xpIter.Current.SelectChildren(XPathNodeType.Element);
                while (xpchildIter.MoveNext())
                {
                    switch (xpchildIter.Current.Name.ToLower())
                    {
                        case "container":
                            container = xpchildIter.Current.Value.ToLower();
                            break;
                        case "service":
                            service = xpchildIter.Current.Value;
                            break;
                        case "startindex":
                            startIndex = int.Parse(xpchildIter.Current.Value);
                            break;
                        case "endindex":
                            endIndex = int.Parse(xpchildIter.Current.Value);
                            break;
                        case "finalorder":
                            finalOrder = int.Parse(xpchildIter.Current.Value);
                            break;
                    }
                }

                if (service == null || container == null || finalOrder == -1 || typeString == null || classString == null)
                    throw new Exception("Malformed Config XML file");

                AdapterTypes atype = AdapterTypes.OtherAdapter;
                SensorTypes stype = SensorTypes.OtherSensor;

                switch (typeString)
                {
                    case "analog":
                        if (String.Equals(container, "element"))
                            atype = AdapterTypes.AnalogSensorAdapter;
                        else if (String.Equals(container, "array"))
                            atype = AdapterTypes.AnalogSensorArrayAdapter;
                        break;
                    case "contact":
                        if (String.Equals(container, "array"))
                            atype = AdapterTypes.ContactSensorArrayAdapter;
                        break;
                    case "drive":
                        if (String.Equals(container, "element"))
                            atype = AdapterTypes.DriveAdapter;
                        break;
                    case "sonar":
                        if (String.Equals(container, "element"))
                            atype = AdapterTypes.SonarAdapter;
                        break;
                    case "sound":
                        if (String.Equals(container, "element"))
                            atype = AdapterTypes.ToneGeneratorAdapter;
                        break;
                    case "ledarray":
                        if (String.Equals(container, "element"))
                            atype = AdapterTypes.LEDArrayAdapter;
                        break;
                }

                switch (classString)
                {
                    case "infra-red":
                        stype = SensorTypes.IRSensor;
                        break;
                    case "line":
                        stype = SensorTypes.LineSensor;
                        break;
                    case "light":
                        stype = SensorTypes.LightSensor;
                        break;
                    case "bumper":
                        stype = SensorTypes.ContactSensorArray;
                        break;
                    case "stall":
                        stype = SensorTypes.StallSensor;
                        break;
                    case "differential":
                        stype = SensorTypes.DiffDrive;
                        break;
                    case "tone":
                        stype = SensorTypes.ToneGenerator;
                        break;
                    case "led":
                        stype = SensorTypes.LEDArray;
                        break;
                    case "ultrasonic":
                        stype = SensorTypes.UltraSonicSonar;
                        break;
                    case "sound":
                        stype = SensorTypes.SoundSensor;
                        break;
                }

                ArrayList adapterList = new ArrayList();

                if (!AdaptersDictionary.TryGetValue(atype, out adapterList))
                    throw new Exception("No such adapter found");

                ArrayList alist = new ArrayList();
                int index = adapterList.IndexOf(service);

                if (String.Equals(container, "element"))
                {
                    ListEntry le = new ListEntry(0, (IAdapter)adapterList[index]);
                    alist.Add(le);
                }
                else if (String.Equals(container, "array"))
                {
                    for (int i = startIndex; i <= endIndex; i++)
                    {
                        ListEntry le = new ListEntry(i, (IAdapter)adapterList[index]);
                        alist.Add(le);
                    }
                }
                AddSensorsToList(stype, alist, finalOrder);
            }
        }

        /// <summary>
        /// Helper function for ParseConfig. Inserts an array of sensor objects in their correct place in sensors dictionary.  
        /// </summary>
        /// <param name="stype">The type of sensors</param>
        /// <param name="slist">The list of sensors objects to be added</param>
        /// <param name="finalOrder">The order of these sensors objects in the overall associated sensor list</param>
        protected virtual void AddSensorsToList(SensorTypes stype, ArrayList slist, int finalOrder)
        {
            ArrayList DTlist = new ArrayList();
            if (!SensorsDictionary.TryGetValue(stype, out DTlist))
            {
                DTlist = new ArrayList(4);
                SensorsDictionary.Add(stype, DTlist);
            }

            if (DTlist.Capacity < (finalOrder + slist.Count))
                DTlist.Capacity = ((finalOrder + slist.Count) / 4 + 1) * 4;     // increase capacity in steps of 4

            DTlist.InsertRange(finalOrder, slist);

            if (stype == SensorTypes.DiffDrive)
                cacheDrive = (DriveAdapter)((ListEntry)DTlist[0]).Adapter;
            else if (stype == SensorTypes.ToneGenerator)
                cacheTone = (ToneGeneratorAdapter)((ListEntry)DTlist[0]).Adapter;
            else if (stype == SensorTypes.LEDArray)
                cacheLED = (LEDArrayAdapter)((ListEntry)DTlist[0]).Adapter;
        }

        public class ListEntry
        {
            int beginOffset;
            IAdapter adapter;

            public int BeginOffset
            {
                get { return beginOffset; }
                set { beginOffset = value; }
            }

            public IAdapter Adapter
            {
                get { return adapter; }
                set { adapter = value; }
            }

            public ListEntry(int off, IAdapter adap)
            {
                beginOffset = off;
                adapter = adap;
            }
        }

        public class NoInstanceException : Exception
        {
        }

        public class InvalidValueException : Exception
        {
            public InvalidValueException(string str) : base(str)
            {
            }
        }

    }
}
#endif