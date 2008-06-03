using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Hosting;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System.Threading;

using contact = Microsoft.Robotics.Services.ContactSensor.Proxy;

namespace IPREFoundationClasses.Adapters
{
    public class ContactSensorArrayAdapter : IAdapter
    {
        protected string serviceUri;
        protected contact.ContactSensorArrayOperations sensorPort;

        public ContactSensorArrayAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port
            sensorPort = DssEnvironment.ServiceForwarder<contact.ContactSensorArrayOperations>(new Uri(serviceUri));
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.ContactSensorArrayAdapter; }
        }

        public bool get(int index)
        {
            bool value = false;
            bool flag = false;

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<contact.ContactSensorArrayState>(false, sensorPort.Get(),
                delegate(contact.ContactSensorArrayState state)
                {
                    value = state.Sensors[index].Pressed;
                    flag = true;
                }
            ));

            while (!flag) ;
            return value;
        }

        public bool[] get()
        {
            bool[] values = null;
            bool flag = false;

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<contact.ContactSensorArrayState>(false, sensorPort.Get(),
                delegate(contact.ContactSensorArrayState state)
                {
                    values = new bool[state.Sensors.Count];
                    for (int i = 0; i < state.Sensors.Count; i++)
                        values[i] = state.Sensors[i].Pressed;
                    flag = true;
                }
            ));

            while (!flag) ;
            return values;
        }

        public override bool Equals(Object obj)
        {
            if (obj is String)
            {
                string truncUri = serviceUri.Substring(serviceUri.IndexOf('/', serviceUri.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.serviceUri, ((ContactSensorArrayAdapter)obj).serviceUri);
        }
    }

}
