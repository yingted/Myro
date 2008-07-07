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

        public void Dispose()
        {
        }

    }
}
