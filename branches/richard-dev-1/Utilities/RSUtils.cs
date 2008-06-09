using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.Core;
using Microsoft.Dss.Hosting;
using W3C.Soap;
using Microsoft.Ccr.Core;
using System.Threading;

namespace Myro.Utilities
{
    public static class RSUtils
    {
        /// <summary>
        /// Synchronous receive, using DssEnvironment.TaskQueue.
        /// Waits for a 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="port"></param>
        /// <returns></returns>
        public static T RecieveSync<T>(PortSet<T, Fault> port)
        {
            T ret = default(T);
            Fault error = null;
            
            ManualResetEvent signal = new ManualResetEvent(false);
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice<T, Fault>(
                    port,
                    delegate(T state)
                    {
                        ret = state;
                        signal.Set();
                    },
                    delegate(Fault failure)
                    {
                        error = failure;
                        signal.Set();
                    }));
            signal.WaitOne();

            ExceptionOfFault(error);
            if (error != null)
                throw ExceptionOfFault(error);
            else
                return ret;
        }

        /// <summary>
        /// Generates an exception from a Fault.  If the Fault encapsulates an
        /// exception (Fault.Detail.Any[0] is Exception), as do Faults created
        /// by FaultOfException, this method returns the exception.  Otherwise,
        /// it returns a FaultReceivedException, which encapsulates the Fault.
        /// </summary>
        /// <param name="fault"></param>
        /// <returns></returns>
        public static Exception ExceptionOfFault(Fault fault)
        {
            if (fault.Detail != null && fault.Detail.Any != null &&
                    fault.Detail.Any.Length > 0 && (fault.Detail.Any[0] is Exception))
                return (Exception)fault.Detail.Any[0];
            else
                return new FaultReceivedException(fault);
        }

        /// <summary>
        /// Generates a Fault that encapsulates an exception, by making
        /// Fault.Detail.Any[0] = exception.  The original exception can by
        /// retreived by calling ExceptionOfFault.
        /// </summary>
        /// <param name="exception"></param>
        /// <returns></returns>
        public static Fault FaultOfException(Exception exception)
        {
            return new Fault() { Detail = new Detail() { Any = new object[] { exception } } };
        }
    }

    public class FaultReceivedException : Exception
    {
        public Fault Fault { get; private set; }
        public FaultReceivedException(Fault fault)
        {
            Fault = fault;
        }
        public string ToString()
        {
            return Strings.FromFault(Fault);
        }
    }
}
