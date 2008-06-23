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
    public class VectorAdapterFactory : IAdapterFactory
    {
        #region IAdapterFactory Members

        private List<string> supportedContracts = new List<string>() { vector.Contract.Identifier };
        public List<string> SupportedContracts
        {
            get { return supportedContracts; }
        }

        public IAdapter Create(ServiceInfoType service)
        {
            return new VectorAdapter(service);
        }

        #endregion
    }

    /// <summary>
    /// This class provides access to a Vector service.  It also allows lookup
    /// of values by tag, by caching a local dictionary of tags and values.
    /// If the TagTimestamp member of the VectorState becomes more recent than
    /// the cached tag-value dictionary, the dictionary is rebuilt.
    /// TODO: Implement setting by tag
    /// </summary>
    public class VectorAdapter : IAdapter
    {
        public ServiceInfoType ServiceInfo { get; private set; }

        vector.VectorOperations opPort;

        public VectorAdapter(ServiceInfoType serviceRecord)
        {
            ServiceInfo = serviceRecord;
            opPort = DssEnvironment.ServiceForwarder<vector.VectorOperations>(new Uri(serviceRecord.Service));
            //if (opPort == null)
            //    throw new AdapterCreationException("Service forwarder port was null");
        }

        /// <summary>
        /// Retrieve the entire vector state.  State members may be null.
        /// </summary>
        /// <returns></returns>
        public vector.VectorState GetState()
        {
            return RSUtils.ReceiveSync<vector.VectorState>(opPort.Get(), Myro.Utilities.Params.defaultRecieveTimeout);
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
            //try
            //{
                var resp = RSUtils.ReceiveSync<vector.GetElementResponseType>(opPort.GetByIndex(index), Myro.Utilities.Params.defaultRecieveTimeout);
                return resp.Value;
            //}
            //catch (ArgumentOutOfRangeException)
            //{
            //    throw new AdapterArgumentException(Strings.IndexOutOfBounds(index));
            //}
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
            //try
            //{
                var resp = RSUtils.ReceiveSync<vector.GetElementResponseType>(opPort.GetByKey(key), Myro.Utilities.Params.defaultRecieveTimeout);
                return resp.Value;
            //}
            //catch (KeyNotFoundException)
            //{
            //    throw new AdapterArgumentException(Strings.KeyNotFound(key));
            //}
        }

        /// <summary>
        /// Retrieve all vector elements at once.
        /// </summary>
        /// <returns></returns>
        public List<double> GetAllElements()
        {
            return RSUtils.ReceiveSync<vector.GetAllElementsResponseType>(opPort.GetAllElements(), Myro.Utilities.Params.defaultRecieveTimeout).Values;
        }

        /// <summary>
        /// Set a single element in the vector by index.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="value"></param>
        public void Set(int index, double value)
        {
            //try
            //{
                RSUtils.ReceiveSync<DefaultUpdateResponseType>(opPort.SetByIndex(new List<int>() { index }, new List<double>() { value }, DateTime.Now), Myro.Utilities.Params.defaultRecieveTimeout);
            //}
            //catch (ArgumentOutOfRangeException)
            //{
            //    throw new AdapterArgumentException(Strings.IndexOutOfBounds(index));
            //}
        }

        /// <summary>
        /// Set a group of elements in the vector by index.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="value"></param>
        public void Set(List<int> indices, List<double> values)
        {
            //try
            //{
                RSUtils.ReceiveSync<DefaultUpdateResponseType>(opPort.SetByIndex(indices, values, DateTime.Now), Myro.Utilities.Params.defaultRecieveTimeout);
            //}
            //catch (ArgumentOutOfRangeException e)
            //{
            //    throw new AdapterArgumentException(
            //        e.ActualValue is Int32 ?
            //        Strings.IndexOutOfBounds((Int32)e.ActualValue) :
            //        Strings.IndexOutOfBounds());
            //}
        }

        /// <summary>
        /// Set a single element in the vector by name.
        /// </summary>
        /// <param name="tag"></param>
        /// <param name="value"></param>
        public void Set(string key, double value)
        {
            //try
            //{
                RSUtils.ReceiveSync<DefaultUpdateResponseType>(opPort.SetByKey(new List<string>() { key }, new List<double>() { value }, DateTime.Now), Myro.Utilities.Params.defaultRecieveTimeout);
            //}
            //catch (KeyNotFoundException)
            //{
            //    throw new AdapterArgumentException(Strings.KeyNotFound(key));
            //}
        }

        /// <summary>
        /// Set a group of elements in the vector by name.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="value"></param>
        public void Set(List<string> keys, List<double> values)
        {
            //try
            //{
                RSUtils.ReceiveSync<DefaultUpdateResponseType>(opPort.SetByKey(keys, values, DateTime.Now), Myro.Utilities.Params.defaultRecieveTimeout);
            //}
            //catch (KeyNotFoundException)
            //{
            //    throw new AdapterArgumentException(Strings.KeyNotFound());
            //}
        }


        /// <summary>
        /// Set all elements in the vector.
        /// </summary>
        /// <param name="values"></param>
        public void SetAllElements(List<double> values)
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(opPort.SetAllElements(values, DateTime.Now), Myro.Utilities.Params.defaultRecieveTimeout);
        }
    }
}
