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
            if (opPort == null)
                throw new AdapterCreationException("Service forwarder port was null");
        }

        /// <summary>
        /// Retrieve the entire vector state.  State members may be null.
        /// </summary>
        /// <returns></returns>
        public vector.VectorState GetState()
        {
            return RSUtils.RecieveSync<vector.VectorState>(opPort.Get());
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
            try
            {
                var resp = RSUtils.RecieveSync<vector.GetElementResponseType>(opPort.GetByIndex(index));
                return resp.Value;
            }
            catch (ArgumentOutOfRangeException e)
            {
                throw new AdapterArgumentException(Strings.IndexOutOfBounds(index));
            }
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
            try
            {
                var resp = RSUtils.RecieveSync<vector.GetElementResponseType>(opPort.GetByKey(key));
                return resp.Value;
            }
            catch (ArgumentOutOfRangeException e)
            {
                throw new AdapterArgumentException(Strings.KeyNotFound(key));
            }
        }

        /// <summary>
        /// Set a single element in the vector by index.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="value"></param>
        public void Set(int index, double value)
        {
            try
            {
                RSUtils.RecieveSync<DefaultUpdateResponseType>(opPort.SetByIndex(index, value, DateTime.Now));
            }
            catch (KeyNotFoundException e)
            {
                throw new AdapterArgumentException(Strings.IndexOutOfBounds(index));
            }

        }

        /// <summary>
        /// Set a single element in the vector by name.
        /// </summary>
        /// <param name="tag"></param>
        /// <param name="value"></param>
        public void Set(string key, double value)
        {
            try
            {
                RSUtils.RecieveSync<DefaultUpdateResponseType>(opPort.SetByKey(key, value, DateTime.Now));
            }
            catch (KeyNotFoundException e)
            {
                throw new AdapterArgumentException(Strings.KeyNotFound(key));
            }
        }

        /// <summary>
        /// Set all elements in the vector.
        /// </summary>
        /// <param name="values"></param>
        public void SetAll(IList<double> values)
        {
            RSUtils.RecieveSync<DefaultUpdateResponseType>(opPort.SetAll(values, DateTime.Now));
        }
    }
}
