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

using analog = Microsoft.Robotics.Services.AnalogSensor.Proxy;

namespace IPREFoundationClasses.Adapters
{
    public class AnalogSensorAdapter : MyroInterfaces.IMyroAnalogSensor, IAdapter
    {
        protected string serviceUri;
        protected analog.AnalogSensorOperations sensorPort;

        public AnalogSensorAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port
            sensorPort = DssEnvironment.ServiceForwarder<analog.AnalogSensorOperations>(new Uri(serviceUri));
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.AnalogSensorAdapter; }
        }

        public float get()
        {
            bool flag = false;
            float retvalue = 0.0f;

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<analog.AnalogSensorState>(false, sensorPort.Get(),
                delegate(analog.AnalogSensorState state)
                {
                    retvalue = (float)state.RawMeasurement;
                    flag = true;
                }
            ));

            while (!flag) ;
            return retvalue;
        }

        public override bool Equals(Object obj)
        {
            if (obj is String)
            {
                string truncUri = serviceUri.Substring(serviceUri.IndexOf('/', serviceUri.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.serviceUri, ((AnalogSensorAdapter)obj).serviceUri);
        }
    }
}
