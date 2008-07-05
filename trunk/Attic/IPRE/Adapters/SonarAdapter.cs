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

using sonar = Microsoft.Robotics.Services.Sonar.Proxy;

#if false
namespace Myro.Adapters
{
    public class SonarAdapter : MyroInterfaces.IMyroAnalogSensor, IAdapter
    {
        protected string serviceUri;
        protected sonar.SonarOperations sensorPort;

        public SonarAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port
            sensorPort = DssEnvironment.ServiceForwarder<sonar.SonarOperations>(new Uri(serviceUri));
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.SonarAdapter; }
        }

        public float get()
        {
            bool flag = false;
            float retvalue = 0.0f;

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<sonar.SonarState>(false, sensorPort.Get(),
                delegate(sonar.SonarState state)
                {
                    retvalue = (float)state.DistanceMeasurement;
                    flag = true;
                }
            ));

            while (!flag) ;
            return retvalue;
        }

        public double[] getMultiple()
        {
            bool flag = false;
            double[] retvalue = null;

            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<sonar.SonarState>(false, sensorPort.Get(),
                delegate(sonar.SonarState state)
                {
                    retvalue = state.DistanceMeasurements;
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
                return String.Equals(this.serviceUri, ((SonarAdapter)obj).serviceUri);
        }
    }

}
#endif