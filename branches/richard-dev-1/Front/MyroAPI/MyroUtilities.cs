using System;
using System.Collections.Generic;
using System.Text;

namespace MyroInterfaces
{
    public class MyroUtilities : IMyroUtility
    {
        private static Random rand = new Random();

        /// <summary>
        /// Returns the date and time object that represents the time known by the
        /// system at the time of the call.
        /// </summary>
        public DateTime CurrentTime
        {
            get { return DateTime.Now; }
        }

        /// <summary>
        /// Returns a random number from an evenly distributed set of numbers from 0.0 to 1.0
        /// </summary>
        public float RandomNumber()
        {
            return (float)rand.NextDouble();
        }

        /// <summary>
        /// Randomly chooses the side of a coin, heads or tails.
        /// </summary>
        /// <returns>The string 'heads' or 'tails'</returns>
        public string FlipCoin()
        {
            return FlipCoinBool() ? "heads" : "tails";
        }

        /// <summary>
        /// Randomly chooses from the results true or false with a 50% chance of either choice.
        /// </summary>
        public Boolean FlipCoinBool()
        {
            return RandomNumber() < 0.5f;
        }

        /// <summary>
        /// Puts the current thread to sleep for a supplied time interval.
        /// </summary>
        /// <param name="seconds">The number of seconds to wait</param>
        public void Wait(int seconds)
        {
            if (seconds < 0)
                throw new ArgumentException("The number of seconds Myro has to wait needs to be at least zero.");
            if (seconds > Int32.MaxValue / 1000)
                throw new ArgumentException(String.Format("At {0} seconds, you are trying to make Myro wait too long", seconds));
            System.Threading.Thread.Sleep(seconds * 1000);
        }
    }
}
