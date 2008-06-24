using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Myro.API
{
    
    public class MyroMovement : IMyroMovement
    {
        Myro.Adapters.AdapterSpec driveAdapter;

        public MyroMovement(Myro.Adapters.AdapterBank bank)
        {
            this.driveAdapter = bank.GetAdapterSpec("drive");
        }

        public enum Direction { LEFT, RIGHT };

        public void Move(double translate, double rotate)
        {
            throw new NotImplementedException("Move not yet implemented");
        }

        public void Forward(double power)
        {
            CheckPowerRange(power);
            SetMotors(power, power);
        }

        public void ForwardFor(double power, double seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(power, power, seconds);
        }

        public void Backward(double power)
        {
            CheckPowerRange(power);
            SetMotors(-power, -power);
        }

        public void BackwardFor(double power, double seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(-power, -power, seconds);
        }

        public void Turn(string direction, double power)
        {
            Direction dir = checkDirection(direction);
            if (dir == Direction.LEFT)
                TurnLeft(power);
            else if (dir == Direction.RIGHT)
                TurnRight(power);
        }

        public void TurnFor(string direction, double power, double seconds)
        {
            Direction dir = checkDirection(direction);
            if (dir == Direction.LEFT)
                TurnLeftFor(power, seconds);
            else if (dir == Direction.RIGHT)
                TurnRightFor(power, seconds);
        }

        public void TurnLeft(double power)
        {
            CheckPowerRange(power);
            SetMotors(-power, power);
        }

        public void TurnLeftFor(double power, double seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(-power, power, seconds);
        }

        public void TurnRight(double power)
        {
            CheckPowerRange(power);
            SetMotors(power, -power);
        }

        public void TurnRightFor(double power, double seconds)
        {
            CheckPowerRange(power);
            SetMotorsFor(power, -power, seconds);
        }

        public virtual void Stop()
        {
            SetMotors(0f, 0f);
        }

        public void SetMotorsFor(double leftPower, double rightPower, double seconds)
        {
            SetMotors(leftPower, rightPower);
            Thread.Sleep((int)(seconds * 1000));
            Stop();
        }

        public void SetMotors(double leftPower, double rightPower)
        {
            try
            {
                ((Myro.Adapters.DriveAdapter)driveAdapter.Adapter).SetMotors(leftPower, rightPower);
            }
            catch (Exception) { }
        }

        private void CheckPowerRange(double power)
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
