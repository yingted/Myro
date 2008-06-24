using System;
using System.Collections.Generic;
using System.Text;

namespace MyProgram
{
    partial class Program
    {
        public static void ScribPlaySong()
        {
            PlaySong(@"C:\Microsoft Robotics Studio (1.5)\samples\IPRE\Foundation\IPREFoundationClasses\IPREFoundationClasses\jingle.mau");
        }

        public static void DrawSquare()
        {
            SetMotorsFor(1f, 1f, 1f);
            TurnRightFor(1f, .25f);
            SetMotorsFor(1f, 1f, 1f);
            TurnRightFor(1f, .25f);
            SetMotorsFor(1f, 1f, 1f);
            TurnRightFor(1f, .25f);
            SetMotorsFor(1f, 1f, 1f);
            TurnRightFor(1f, .25f);
        }

        public static void DrawSquareLoop()
        {
            for (int i = 0; i < 4; i++)
            {
                SetMotorsFor(1f, 1f, 2f);
                TurnRightFor(1f, .5f);
            }
        }

        public static void NxtAvoidCollision()
        {
            float distance;
            while (true)
            {
                SetMotors(.5f, .5f);
                distance = getSonar(0);
                if (distance < 30 && distance > 0)
                {
                    Stop();
                    SetMotorsFor(-.5f, -.5f, .5f);
                    TurnRightFor(.5f, .5f);
                }
            }
        }

        public static void work()
        {
            //ScribPlaySong();
            //DrawSquare();
            //DrawSquareLoop();
            NxtAvoidCollision();
        }

    }
}
