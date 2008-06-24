using System;
using System.Collections.Generic;
using System.Text;

namespace ScribblerControl
{
    class ScribblerControl : ScribblerBrain
    {
        static void Main(string[] args)
        {
            ScribblerControl robot = new ScribblerControl();
            Console.WriteLine("Press <Enter> to begin your program");
            Console.ReadLine();
            robot.Program1();
            Console.WriteLine("Program completed running");
        }

        public void Program1()
        {
            Console.WriteLine("Drawing Square");

            for (int i = 0; i < 4; i++)
            {
                ForwardFor(1f, 2f);
                TurnFor("left", 1f, 0.5f);
            }

        }
    }
}
