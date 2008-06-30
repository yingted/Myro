using System;
using System.Collections.Generic;
using System.Text;

namespace Myro.API
{
    public interface IMyroMovement
    {
        void Move(double translate, double rotate); 
        
        void Forward(double power);
        void ForwardFor(double power, double seconds);
        
        void Backward(double power);
        void BackwardFor(double power, double seconds);

        void Turn(string direction, double power);
        void TurnFor(string direction, double power, double seconds);

        void TurnLeft(double power);
        void TurnLeftFor(double power, double seconds);

        void TurnRight(double power);
        void TurnRightFor(double power, double seconds);

        void Stop();

        void SetMotors(double leftPower, double rightPower);
        void SetMotorsFor(double leftPower, double rightPower, double seconds);
    }
}
