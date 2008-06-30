using System;

namespace SharpLogic.Robotics.Surveyor.Srv1
{
    public class BounceIRResponse
    {
        internal BounceIRResponse(int front, int left, int back, int right)
        {
            this._front = front;
            this._left = left;
            this._back = back;
            this._right = right;
        }

        public int Front
        {
            get { return this._front; }
            set { this._front = value; }
        }
        private int _front;

        public int Left
        {
            get { return this._left; }
            set { this._left = value; }
        }
        private int _left;

        public int Back
        {
            get { return this._back; }
            set { this._back = value; }
        }
        private int _back;

        public int Right
        {
            get { return this._right; }
            set { this._right = value; }
        }
        private int _right;
    }
}
