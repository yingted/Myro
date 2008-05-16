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

using analogArray = Microsoft.Robotics.Services.AnalogSensorArray.Proxy;

namespace IPREFoundationClasses.Adapters
{
    public class AnalogSensorArrayAdapter : IAdapter
    {
        protected string serviceUri;
        protected analogArray.AnalogSensorOperations sensorPort;

        public AnalogSensorArrayAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port
            sensorPort = DssEnvironment.ServiceForwarder<analogArray.AnalogSensorOperations>(new Uri(serviceUri));
            Console.WriteLine("Analog sensor array adapter: " + serviceUri);
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.AnalogSensorArrayAdapter; }
        }

        public float[] get()
        {
            float[] values = null;
            bool flag = false;
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<analogArray.AnalogSensorArrayState>(false, sensorPort.Get(),
                delegate(analogArray.AnalogSensorArrayState state)
                {
                    values = new float[state.Sensors.Count];
                    for (int i = 0; i < state.Sensors.Count; i++)
                        values[i] = (float)state.Sensors[i].RawMeasurement;
                    flag = true;
                }
            ));

            while (!flag) ;
            return values;
        }

        public float get(int index)
        {
            float value = -1f;
            bool flag = false;

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<analogArray.AnalogSensorArrayState>(false, sensorPort.Get(),
                delegate(analogArray.AnalogSensorArrayState state)
                {
                    value = (float)state.Sensors[index].RawMeasurement;
                    flag = true;
                }
            ));

            while (!flag) ;
            return value;
        }

        public override bool Equals(Object obj)
        {
            if (obj is String)
            {
                string truncUri = serviceUri.Substring(serviceUri.IndexOf('/', serviceUri.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.serviceUri, ((AnalogSensorArrayAdapter)obj).serviceUri);
        }
    }

}
