// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using Myro;

namespace Myro.Adapters
{
    /// <summary>
    /// Every adapter class must implement this IAdapter interface
    /// </summary>
    public interface IAdapter
    {
        /// <summary>
        /// Returns the ServiceInfoType (containing the service URI, among other things)
        /// </summary>
        ServiceInfoType ServiceInfo { get; }

        void Dispose();
    }

    /// <summary>
    /// Every adapter must come with an adapter factory to create it.  The
    /// AdapterBank is created with a list of adapter factories, which it
    /// uses to hook up adapters to services.  You can add custom adapter
    /// factories to the AdapterBank by adding them to the AdapterFactories
    /// property.
    /// </summary>
    public interface IAdapterFactory
    {
        /// <summary>
        /// This should contain the list of contracts to which this adapter
        /// can connect.
        /// </summary>
        List<string> SupportedContracts { get; }

        /// <summary>
        /// This method should create the adapter from the given service 
        /// record.  The contract of the service record will be one of the 
        /// contracts in the SupportedContracts property.
        /// </summary>
        /// <param name="service"></param>
        /// <returns></returns>
        IAdapter Create(ServiceInfoType service);
    }
}
