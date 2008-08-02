// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Myro.Utilities
{
    /// <summary>
    /// This class contains all of the messages that could be shown to the user, as a facility for internationalization.
    /// It also contains methods for computing messages, based on arguments, or from Exceptions or Faults.
    /// </summary>
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
        public static string NotInitialized = "Myro is not initialized.";
        public static string SavePrompt(string file) { return "Do you want to save the file '" + file + "'?"; }
        public static string SaveConfigPrompt = "Do you want to save the changes to this robot configuration?";
        public static string BaseNameNotFound = "Myro does not know about this robot type.";
        public static string TrySpecifyCOMPort = "Could not connect to the Scribbler on the current COM port.  You can try specifying the COM port, by typing init('Scribbler', 'comXX'), where XX is your COM port number.  If Myro can then connect, it will remember your COM port from now on.";
        public static string FromFault(W3C.Soap.Fault fault)
        {
            string msg = "Fault received:";
            if (fault.Reason != null)
                foreach (var r in fault.Reason)
                    msg += r.Value;
            return msg;
        }
        /// <summary>
        /// Returns the Message parameter of the exception, if it has one.  If it does not
        /// have a Message, or it is of zero length, this returns Exception.ToString().
        /// </summary>
        /// <param name="e"></param>
        /// <returns></returns>
        public static string FromExceptionMessage(Exception e)
        {
            if (e.Message != null && e.Message.Length > 0)
                return e.Message;
            else
                return e.ToString();
        }
        /// <summary>
        /// Build a detailed exception message, recursively including inner exceptions up to a recursion limit of 6.
        /// This also will print the exception message, source, and stack trace for each exception.
        /// </summary>
        /// <param name="e"></param>
        /// <returns></returns>
        public static string FromException(Exception e)
        {
            return buildExceptionMessage("", e, 6);
        }
        private static string buildExceptionMessage(string curMessage, Exception e, int remainingLevels)
        {
            if (e != null)
            {
                var builder = new StringBuilder(curMessage);
                builder.Append("Exception of type ");
                builder.Append(e.GetType().FullName);
                if (e.Message != null && e.Message.Length > 0)
                {
                    builder.Append(": ");
                    builder.Append("\"" + e.Message + "\".");
                }
                else
                    builder.Append(".  No message available.");
                builder.AppendLine();
                if (e.Source != null)
                {
                    builder.Append("Source: ");
                    builder.Append(e.Source.ToString());
                    builder.AppendLine();
                }
                if (e.StackTrace != null && e.StackTrace.Length > 0)
                    builder.Append("Stacktrace:\n" + e.StackTrace);
                else
                    builder.Append("No stacktrace available.");
                builder.AppendLine();
                builder.AppendLine();
                if (remainingLevels >= 0 && e.InnerException != null)
                {
                    builder.AppendLine("Inner exception:");
                    return buildExceptionMessage(builder.ToString(), e.InnerException, remainingLevels - 1);
                }
                else
                    return builder.ToString();
            }
            else
                return curMessage;
        }

        public static string Light = "light";
        public static string IR = "ir";
        public static string Line = "line";
        public static string Stall = "stall";
        public static string Name = "name";
    }
}
