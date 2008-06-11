using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Myro.API
{
    
    public class MyroMovement : IMyroMovement
    {
        Myro.Adapters.AdapterSpec driveAdapter;

        public MyroMovement(Myro.Adapters.AdapterSpec driveAdapter)
        {
            this.driveAdapter = driveAdapter;
        }

        public enum Direction { LEFT, RIGHT };

        public void Move(float translate, float rotate)
        {
            throw new NotImplementedException("Move not yet implemented");
        }

        public void Forward(float power)
        {
            CheckPowerRange(power);
            SetMotors(power, power);
        }

        public void ForwardFor(float power, float seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(power, power, seconds);
        }

        public void Backward(float power)
        {
            CheckPowerRange(power);
            SetMotors(-power, -power);
        }

        public void BackwardFor(float power, float seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(-power, -power, seconds);
        }

        public void Turn(string direction, float power)
        {
            Direction dir = checkDirection(direction);
            if (dir == Direction.LEFT)
                TurnLeft(power);
            else if (dir == Direction.RIGHT)
                TurnRight(power);
        }

        public void TurnFor(string direction, float power, float seconds)
        {
            Direction dir = checkDirection(direction);
            if (dir == Direction.LEFT)
                TurnLeftFor(power, seconds);
            else if (dir == Direction.RIGHT)
                TurnRightFor(power, seconds);
        }

        public void TurnLeft(float power)
        {
            CheckPowerRange(power);
            SetMotors(-power, power);
        }

        public void TurnLeftFor(float power, float seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(-power, power, seconds);
        }

        public void TurnRight(float power)
        {
            CheckPowerRange(power);
            SetMotors(power, -power);
        }

        public void TurnRightFor(float power, float seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(power, -power, seconds);
        }

        public virtual void Stop()
        {
            SetMotors(0f, 0f);
        }

        public void SetMotorsFor(float leftPower, float rightPower, float seconds)
        {
            SetMotors(leftPower, rightPower);
            Thread.Sleep((int)(seconds * 1000));
            Stop();
        }

        public void SetMotors(float leftPower, float rightPower)
        {
            try
            {
                driveAdapter.GetDriveAdapter().SetMotors(leftPower, rightPower);
            }
            catch (Myro.Adapters.UnattachedAdapter) { }
        }

        private void CheckPowerRange(float power)
        {
            if (power < -1.0f || power > 1.0f)
                throw new ArgumentException("Motor power settings must be in the range of -1.0 to 1.0");
        }

        private Direction checkDirection(string direction)
        {
            if (direction == null)
                throw new ArgumentException("Direction settings when turning must be either 'left' or 'right'");
            direction = direction.ToLower().Trim();
            if (direction.Equals("left"))
                return Direction.LEFT;
            if (direction.Equals("right"))
                return Direction.RIGHT;
            throw new ArgumentException("Direction settings when turning must be either 'left' or 'right'");
        }
    }
}
