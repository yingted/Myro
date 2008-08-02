// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Hosting;
using Microsoft.Ccr.Core;
using W3C.Soap;
using vector = Myro.Services.Generic.Vector.Proxy;
using System.Threading;
using Myro.Utilities;

namespace Myro.Adapters
{
    /// <summary>
    /// See documentation for IAdapterFactory in IAdapter.cs
    /// </summary>
    public class VectorAdapterFactory : IAdapterFactory
    {
        #region IAdapterFactory Members

        private List<string> supportedContracts = new List<string>() { vector.Contract.Identifier };
        /// <summary>
        /// See documentation for IAdapterFactory in IAdapter.cs
        /// </summary>
        public List<string> SupportedContracts
        {
            get { return supportedContracts; }
        }

        /// <summary>
        /// See documentation for IAdapterFactory in IAdapter.cs
        /// </summary>
        public IAdapter Create(ServiceInfoType service)
        {
            return new VectorAdapter(service);
        }

        #endregion
    }

    /// <summary>
    /// This class provides access to a Vector service.
    /// See adapter documentation in the Myro 3 developer manual.
    /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
    /// </summary>
    public class VectorAdapter : IAdapter
    {
        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        public ServiceInfoType ServiceInfo { get; private set; }

        vector.VectorOperations opPort;

        DispatcherQueue taskQueue = new DispatcherQueue("VectorAdapter", new Dispatcher(1, "VectorAdapter"));

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        public VectorAdapter(ServiceInfoType serviceRecord)
        {
            ServiceInfo = serviceRecord;
            opPort = DssEnvironment.ServiceForwarder<vector.VectorOperations>(new Uri(serviceRecord.Service));
            //if (opPort == null)
            //    throw new AdapterCreationException("Service forwarder port was null");
        }

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        public void Dispose()
        {
            taskQueue.Dispose();
        }

        /// <summary>
        /// Retrieve the entire vector state.  State members may be null.
        /// </summary>
        /// <returns></returns>
        public vector.VectorState GetState()
        {
            return RSUtils.ReceiveSync<vector.VectorState>(taskQueue, opPort.Get(), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Retrieve a single element from the state vector, with full safety
        /// checks.  Throws AdapterArgumentException and
        /// AdapterOperationException.
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        public double Get(int index)
        {
            var resp = RSUtils.ReceiveSync<vector.GetElementResponseType>(taskQueue, opPort.GetByIndex(index), Myro.Utilities.Params.DefaultRecieveTimeout);
            return resp.Value;
        }

        /// <summary>
        /// Retrieve a single element from the state vetor, by name, with full
        /// safety checks.  Throws AdapterArgumentException and
        /// AdapterOperationException.
        /// </summary>
        /// <param name="tag"></param>
        /// <returns></returns>
        public double Get(string key)
        {
            var resp = RSUtils.ReceiveSync<vector.GetElementResponseType>(taskQueue, opPort.GetByKey(key), Myro.Utilities.Params.DefaultRecieveTimeout);
            return resp.Value;
        }

        /// <summary>
        /// Retrieve all vector elements at once.
        /// </summary>
        /// <returns></returns>
        public List<double> GetAllElements()
        {
            return RSUtils.ReceiveSync<vector.GetAllElementsResponseType>(taskQueue, opPort.GetAllElements(), Myro.Utilities.Params.DefaultRecieveTimeout).Values;
        }

        /// <summary>
        /// Set a single element in the vector by index.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="value"></param>
        public void Set(int index, double value)
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(taskQueue, opPort.SetByIndex(new List<int>() { index }, new List<double>() { value }, DateTime.Now), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Set a group of elements in the vector by index.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="value"></param>
        public void Set(List<int> indices, List<double> values)
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(taskQueue, opPort.SetByIndex(indices, values, DateTime.Now), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Set a single element in the vector by name.
        /// </summary>
        /// <param name="tag"></param>
        /// <param name="value"></param>
        public void Set(string key, double value)
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(taskQueue, opPort.SetByKey(new List<string>() { key }, new List<double>() { value }, DateTime.Now), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Set a group of elements in the vector by name.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="value"></param>
        public void Set(List<string> keys, List<double> values)
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(taskQueue, opPort.SetByKey(keys, values, DateTime.Now), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Set all elements in the vector.
        /// </summary>
        /// <param name="values"></param>
        public void SetAllElements(List<double> values)
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(taskQueue, opPort.SetAllElements(values, DateTime.Now), Myro.Utilities.Params.DefaultRecieveTimeout);
        }
    }
}
