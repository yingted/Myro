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
    class VectorAdapter : IAdapter<vector.VectorState>
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

        public void set(vector.VectorState state)
        {
            throw new NotImplementedException();
        }
    }
}
