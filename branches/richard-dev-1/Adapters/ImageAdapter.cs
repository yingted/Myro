using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;

namespace Myro.Adapters
{
    public class ImageAdapter : IAdapter
    {
        public ServiceInfoType ServiceInfo { get; private set; }

        public System.Drawing.Image Get()
        {
            throw new NotImplementedException();
        }

        public void Set(System.Drawing.Image state)
        {
            throw new NotImplementedException();
        }
    }
}
