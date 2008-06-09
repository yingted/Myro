using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;

namespace Myro.Adapters
{
    public class SpeechAdapter : IAdapter
    {
        public ServiceInfoType ServiceInfo { get; private set; }

        public string Get()
        {
            throw new NotImplementedException();
        }

        public void Set(string state)
        {
            throw new NotImplementedException();
        }
    }
}
