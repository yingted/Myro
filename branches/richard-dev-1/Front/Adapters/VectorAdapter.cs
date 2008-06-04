using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Hosting;
using Microsoft.Ccr.Core;
using W3C.Soap;
using vector = Myro.Services.Generic.Vector.Proxy;

namespace Myro.Adapters
{
    /// <summary>
    /// This class provides access to a Vector service.  It also allows lookup
    /// of values by tag, by caching a local dictionary of tags and values.
    /// If the TagTimestamp member of the VectorState becomes more recent than
    /// the cached tag-value dictionary, the dictionary is rebuilt.
    /// TODO: Implement setting by tag
    /// </summary>
    class VectorAdapter : IAdapter<vector.VectorState>
    {
        public ServiceInfoType ServiceInfo { get; private set; }

        vector.VectorOperations opPort;

        Dictionary<string, int> indexCache = null;
        DateTime indexCacheTime = null;

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
        public vector.VectorState get()
        {
            vector.VectorState ret = null;
            Arbiter.ExecuteToCompletion(DssEnvironment.TaskQueue,
                Arbiter.Choice<vector.VectorState, Fault>(
                    opPort.Get(),
                    delegate(vector.VectorState state)
                    {
                        ret = state;
                    },
                    delegate(Fault failure)
                    {
                        throw new AdapterOperationException(failure);
                    }));
            return ret;
        }

        /// <summary>
        /// Retrieve a single element from the state vector, with full safety
        /// checks.  Throws AdapterArgumentException and
        /// AdapterOperationException.
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        public double get(int index)
        {
            vector.VectorState state = get();
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
        public double get(string tag)
        {
            vector.VectorState state = get();
            if (state.Values != null)
            {
                checkIndexCache(state);
                int index;
                try
                {
                    index = indexCache[tag];
                }
                catch (KeyNotFoundException e)
                {
                    throw new AdapterArgumentException(Strings.TagNotFound);
                }
                if(index > state.Values.Count)
                    throw new AdapterOperationException(Strings.VectorTooShort);
                return state.Values[index];
            }
            else
                throw new AdapterOperationException(Strings.NotReady);
        }

        public void set(vector.VectorState state)
        {
            throw new NotImplementedException();
        }

        private void checkIndexCache(vector.VectorState state)
        {
            if (indexCacheTime == null || state.TagTimestamp.CompareTo(indexCacheTime) > 0)
            {
                if (state.Tags != null && state.Values != null)
                {
                    indexCache = new Dictionary<string, int>(state.Tags.Count);
                    int max = state.Tags.Count > state.Values.Count ? state.Values.Count : state.Tags.Count;
                    for (int i = 0; i < max; i++)
                        indexCache.Add(state.Tags[i], i);
                }
                else
                    indexCache = new Dictionary<string, int>();
            }
        }
    }
}
