using System;
using System.Collections.Generic;
using System.Text;

namespace Myro.API
{
    public interface IMyroMovement
    {
        void Move(float translate, float rotate); 
        
        void Forward(float power);
        void ForwardFor(float power, float seconds);
        
        void Backward(float power);
        void BackwardFor(float power, float seconds);

        void Turn(string direction, float power);
        void TurnFor(string direction, float power, float seconds);

        void TurnLeft(float power);
        void TurnLeftFor(float power, float seconds);

        void TurnRight(float power);
        void TurnRightFor(float power, float seconds);

        void Stop();

        void SetMotors(float leftPower, float rightPower);
        void SetMotorsFor(float leftPower, float rightPower, float seconds);
    }
}
