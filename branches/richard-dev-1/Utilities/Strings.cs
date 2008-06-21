using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Myro.Utilities
{
    public static class Strings
    {
        public static string FaultReceived = "Fault received.";
        public static string IndexOutOfBounds(int requested, int max) { return "Index \"" + requested + "\" is out of bounds.  Indices must be >= 0.  The maximum index in this case " + max + "."; }
        public static string IndexOutOfBounds(int requested) { return "Index \"" + requested + "\" is out of bounds"; }
        public static string IndexOutOfBounds() { return "Index is out of bounds"; }
        public static string NotReady = "The sensor or actuator is not ready yet.";
        public static string VectorTooShort = "The vector from this service is shorter than expected, this probably indicates a bug in the service.";
        public static string KeyNotFound(string key) { return "The element name \"" + key + "\" was not found in this vector"; }
        public static string KeyNotFound() { return "The element name was not found"; }
        public static string FromFault(W3C.Soap.Fault fault)
        {
            string msg = "Fault received:";
            if (fault.Reason != null)
                foreach (var r in fault.Reason)
                    msg += r.Value;
            return msg;
        }

        public static string Light = "light";
        public static string IR = "ir";
        public static string Line = "line";
        public static string Stall = "stall";
        public static string Name = "name";
    }
}
