using System;
using System.Collections.Generic;
using System.Text;

namespace SharpLogic.Robotics.Surveyor.Srv1
{
    internal class SerialResponse
    {
        public object Data { get { return this._data; } }
        private object _data;

        public SerialCommands SerialCommand { get { return this._serialCommand; } }
        private SerialCommands _serialCommand;

        public SerialResponse(SerialCommands serialCommand) : this(serialCommand, null)
        {
        }

        public SerialResponse(SerialCommands serialCommand, object data)
        {
            this._serialCommand = serialCommand;
            this._data = data;
        }

    }
}
