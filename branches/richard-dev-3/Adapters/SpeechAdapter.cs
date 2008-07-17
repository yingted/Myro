// Copyright (c) Microsoft Corporation.  All rights reserved.

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

        public void Dispose()
        {
        }
    }
}
