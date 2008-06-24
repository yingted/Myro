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

using ledarray = IPREGenericContracts.LEDarray.Proxy;

namespace IPREFoundationClasses.Adapters
{
    public class LEDArrayAdapter : MyroInterfaces.IMyroLED, IAdapter
    {
        protected string serviceUri;
        protected ledarray.LedarrayOperations ledPort;

        public LEDArrayAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port and subscribe to the service
            ledPort = DssEnvironment.ServiceForwarder<ledarray.LedarrayOperations>(new Uri(serviceUri));
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.LEDArrayAdapter; }
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

            ledarray.SetSingleRequest request = new ledarray.SetSingleRequest(which, state != 0);
            ledarray.SetSingle msg = new ledarray.SetSingle(request);
            ledPort.Post(msg);
        }

        public override bool Equals(Object obj)
        {
            if (obj is String)
            {
                string truncUri = serviceUri.Substring(serviceUri.IndexOf('/', serviceUri.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.serviceUri, ((LEDArrayAdapter)obj).serviceUri);
        }
    }
}
