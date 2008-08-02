// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;

namespace Myro.Adapters
{
    /// <summary>
    /// The speech adapter is not currently implemented.
    /// See adapter documentation in the Myro 3 developer manual.
    /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
    /// </summary>
    public class SpeechAdapter : IAdapter
    {
        /// <summary>
        /// The speech adapter is not currently implemented.
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        /// <param name="serviceRecord"></param>
        public ServiceInfoType ServiceInfo { get; private set; }

        /// <summary>
        /// The speech adapter is not currently implemented.
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        public void Dispose()
        {
        }
    }
}
