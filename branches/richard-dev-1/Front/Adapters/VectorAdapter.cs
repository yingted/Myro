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

        Dictionary<string, int> indexCache = null;
        DateTime indexCacheTime = DateTime.Now;

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
            vector.VectorState ret = null;
            Fault error = null;
            Object monitor = new Object();
            Signal signal = new Signal();
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice<vector.VectorState, Fault>(
                    opPort.Get(),
                    delegate(vector.VectorState state)
                    {
                        ret = state;
                        signal.Raise();
                    },
                    delegate(Fault failure)
                    {
                        error = failure;
                        signal.Raise();
                    }));
            signal.Wait();
            if (error != null)
                throw new AdapterOperationException(error);
            else
                return ret;
        }

        /// <summary>
        /// Return only the values from the vector state.
        /// </summary>
        /// <returns></returns>
        public double[] Get()
        {
            vector.VectorState state = GetState();
            return state.Values.ToArray();
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
            vector.VectorState state = GetState();
            if (state.Values != null)
            {
                if (index < 0 || index >= state.Values.Count)
                    throw new AdapterArgumentException(Strings.IndexOutOfBounds(index, state.Values.Count));
                else
                    return state.Values[index];
            }
            else
                throw new AdapterOperationException(Strings.NotReady);
        }

        /// <summary>
        /// Retrieve a single element from the state vetor, by name, with full
        /// safety checks.  Throws AdapterArgumentException and
        /// AdapterOperationException.
        /// </summary>
        /// <param name="tag"></param>
        /// <returns></returns>
        public double Get(string tag)
        {
            vector.VectorState state = GetState();
            CheckIndexCache(state);
            int index;
            try
            {
                index = indexCache[tag];
            }
            catch (KeyNotFoundException e)
            {
                throw new AdapterArgumentException(Strings.TagNotFound(tag));
            }
            if (index > state.Values.Count)
                throw new AdapterOperationException(Strings.VectorTooShort);
            return state.Values[index];
        }

        public void Set(vector.VectorState state)
        {
            throw new NotImplementedException();
        }

        private void CheckIndexCache(vector.VectorState state)
        {
            if (indexCache == null || state.TagTimestamp.CompareTo(indexCacheTime) > 0)
            {
                indexCache = new Dictionary<string, int>(state.Tags.Count);
                int max = state.Tags.Count > state.Values.Count ? state.Values.Count : state.Tags.Count;
                for (int i = 0; i < max; i++)
                    indexCache.Add(state.Tags[i], i);
            }
        }
    }
}
