// Copyright (c) Microsoft Corporation.  All rights reserved.

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using Microsoft.Dss.Core.DsspHttp;
using System.ComponentModel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Myro.Services.Generic.Vector
{
    [DisplayName("Vector Service")]
    [Description("The Vector Service")]
    [Contract(Contract.Identifier)]
    [ServicePort(AllowMultipleInstances = false)]
    class VectorServiceImpl : VectorServiceBase
    {
        public VectorServiceImpl(DsspServiceCreationPort port) : base(port) { }

        [ServicePort("/vector", AllowMultipleInstances=false)]
        VectorOperations _operationsPort = new VectorOperations();
        protected override VectorOperations OperationsPort { get { return _operationsPort; } }
    }
}
