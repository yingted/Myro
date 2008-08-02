// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Myro.Adapters
{
    /// <summary>
    /// This is an internally-used type to deal with the adapter type
    /// argument.
    /// </summary>
    interface AdapterTokenN
    {
        string Name { get; }
        IAdapter AdapterIfAttached { get; }
    }
}
