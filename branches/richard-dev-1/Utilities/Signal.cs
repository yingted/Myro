using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace Myro.Utilities
{
    /// <summary>
    /// Acts like a waitable, thread-safe, flag.  A thread can call Wait(), in
    /// which case it will block until another thread "raises" the signal by
    /// calling Raise().  If a thread calls Cancel(), all waiting threads will
    /// instead receive a SignalCancelledException.  Any threads calling Wait()
    /// after Raise() is called will return immediately, and any calling Wait()
    /// after Cancel() is called will immediately receive a SignalCancelledException.
    /// This class is designed to be completely safe, i.e.
    /// Wait() will return immediately if the flag is already raised, and you
    /// should not have to worry about Wait getting stuck when it should
    /// return (I hope).  Multiple threads can call Wait(), Raise(), and
    /// Cancel().
    /// </summary>
    public class Signal
    {
        bool raised = false;
        bool cancelled = false;
        List<Object> signals = new List<Object>();

        public Signal()
        {
        }

        /// <summary>
        /// Wait for the signal to be raised.  Returns true if it was raised, 
        /// or false if waiting timed out.  Throws SignalCancelledException
        /// if Cancel() was called.  Note that a negative timeout will result
        /// in waiting indefinitely.
        /// </summary>
        /// <param name="timeout"></param>
        /// <returns></returns>
        public bool Wait(TimeSpan timeout)
        {
            Object signal = new Object();
            // This lock will prevent the signal from being changed while we
            // are setting up this call's monitor.
            lock (this)
            {
                if (raised)
                    return true;
                else
                {
                    if (cancelled)
                        throw new SignalCancelledException();

                    // Lock this thread's monitor (this also prevents the flag
                    // from being changed)
                    Monitor.Enter(signal);
                    try
                    {
                        signals.Add(signal);
                    }
                    catch (Exception e)
                    {
                        Monitor.Exit(signal);
                        throw e;
                    }
                }
            }

            try
            {
                // Now that this thread's monitor has been added and locked before
                // any pulses could be sent, or the flag changed, wait for a pulse.
                DateTime finish = DateTime.Now.Add(timeout);
                TimeSpan remaining = timeout;
                bool stay = true;
                while (stay)
                {
                    stay = false;
                    try
                    {
                        if (timeout.Ticks < 0)
                            Monitor.Wait(signal);
                        else
                            Monitor.Wait(signal, remaining);
                    }
                    catch (ThreadInterruptedException e)
                    {
                        remaining = finish.Subtract(DateTime.Now);
                        if (timeout.Ticks < 0 || remaining.Ticks > 0)
                            stay = true;
                    }
                }

                // At this point, we have this call's monitor, so we can check the
                // raised flag safely, and remove this call's monitor from the
                // list.  It is ok if another thread tries to lock this call's
                // monitor after it is removed from the list, because it will lock
                // immediately.
                signals.Remove(signal);
                if (raised == true)
                    return true;
                else
                {
                    if (cancelled == true)
                        throw new SignalCancelledException();
                    else
                        return false;
                }
            }
            catch (Exception e)
            {
                throw e;
            }
            finally
            {
                Monitor.Exit(signal);
            }
        }

        /// <summary>
        /// Same as Wait(TimeSpan), but waits indefinitely.
        /// </summary>
        /// <returns></returns>
        public bool Wait()
        {
            return this.Wait(TimeSpan.FromTicks(-1));
        }

        public void Raise()
        {
            lock (this)
            {
                foreach (Object signal in signals)
                    Monitor.Enter(signal);
                raised = true;
                foreach (Object signal in signals)
                {
                    Monitor.PulseAll(signal);
                    Monitor.Exit(signal);
                }
            }
        }

        public void Cancel()
        {
            lock (this)
            {
                foreach (Object signal in signals)
                    Monitor.Enter(signal);
                cancelled = true;
                foreach (Object signal in signals)
                {
                    Monitor.PulseAll(signal);
                    Monitor.Exit(signal);
                }
            }
        }

    }

    /// <summary>
    /// This exception is thrown by all calls to Wait() if Cancel() is called.
    /// </summary>
    public class SignalCancelledException : Exception
    {
        public SignalCancelledException()
            : base()
        {
        }
    }
}
