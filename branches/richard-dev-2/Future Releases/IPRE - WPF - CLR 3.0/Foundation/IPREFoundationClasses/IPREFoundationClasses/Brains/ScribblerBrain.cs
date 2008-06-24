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

using sled = IPRE.ScribblerLEDArray.Proxy;
using directory = Microsoft.Dss.Services.Directory;

using IPREFoundationClasses.RobotGUI;

namespace IPREFoundationClasses
{
    public class ScribblerBrain : RobotBrain
    {
        public ScribblerBrain(string configFile) : base(configFile)
        {
            ScribInit();
        }

        void ScribInit()
        {
            Bind();
            Thread.Sleep(10000);
            ParseConfig();
        }

        private ScribblerLEDArrayAdapter sledadapter = null;

        /// <summary>
        /// Overrides RobotBrain Bind function to implement addition LEDAdapter functions.
        /// </summary>
        protected override void Bind()
        {
            base.Bind();

            //Bind to scribbler specific sensors

            //Specialized LED array
            directory.Query dquery = new directory.Query(new directory.QueryRequestType(
                        new Microsoft.Dss.ServiceModel.Dssp.ServiceInfoType("http://www.roboteducation.org/scribblerledarray.html")));
            //Uri direcURI = DssEnvironment.FindService(directory.Contract.Identifier);
            //TODO: remove the line below once the above line works
            Uri direcURI = new Uri("http://localhost:" + httpPort + "/directory");
            directory.DirectoryPort dport = DssEnvironment.ServiceForwarder<directory.DirectoryPort>(direcURI);
            dport.Post(dquery);

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice(dquery.ResponsePort,
                    delegate(directory.QueryResponseType success)
                    {
                        sledadapter = new ScribblerLEDArrayAdapter(success.RecordList[0].Service);
                    },
                    delegate(Fault fault) { }
                )
            );
        }

        public override void setLED(string position, int state)
        {
            sledadapter.setLED(position, state);
        }

        /// <summary>
        /// Scribbler Specialized LED Adapter
        /// </summary>
        public class ScribblerLEDArrayAdapter : MyroInterfaces.IMyroLED
        {
            protected string serviceUri;
            protected sled.ScribblerLEDArrayOperations ledPort;

            public ScribblerLEDArrayAdapter(string serviceUri)
            {
                this.serviceUri = serviceUri;
                initialize();
            }

            protected virtual void initialize()
            {
                // Initialize the port and subscribe to the service
                ledPort = DssEnvironment.ServiceForwarder<sled.ScribblerLEDArrayOperations>(new Uri(serviceUri));
            }

            /// <summary>
            /// Service URI
            /// </summary>
            public string ServiceUri
            {
                get { return serviceUri; }
            }

            public void setBinary(uint number)
            {
                throw new NotImplementedException();
            }

            public void setLED(string position, string state)
            {
                throw new NotImplementedException();
            }

            public void setLED(string position, int state)
            {
                int which;
                switch (position.ToLower())
                {
                    case "left":
                        which = 0;
                        break;
                    case "center":
                        which = 1;
                        break;
                    case "right":
                        which = 2;
                        break;
                    default:
                        throw new IndexOutOfRangeException();
                }

                sled.SetSingleRequest request = new sled.SetSingleRequest(which, state != 0);
                sled.SetSingle msg = new sled.SetSingle(request);
                ledPort.Post(msg);
            }
        }
    }
}
