//-----------------------------------------------------------------------
//  This file is part of Microsoft Robotics Developer Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: GameControllerTypes.cs $ $Revision: 2 $
//-----------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;

using System;
using System.Collections.Generic;
using System.ComponentModel;
using W3C.Soap;

using input = Microsoft.Robotics.Input;
using System.Diagnostics;

namespace Myro.Services.GameController
{

    /// <summary>
    /// Joystick Contract
    /// </summary>
    public static class Contract
    {
        /// The Unique Contract Identifier for the Joystick service
        public const String Identifier = "http://schemas.microsoft.com/robotics/2006/09/gamecontroller.html";
    }

    [DataContract]
    [Description ("The state of the game controller.")]
    public class GameControllerState
    {
        private DateTime _timeStamp = DateTime.Now;
        [DataMember]
        [Browsable(false)]
        [Description ("Indicates the time (in ms) of the last input reading of the Game Controller service.")]
        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }

        private Controller _controller = new Controller();
        [DataMember(IsRequired = true)]
        [Description("Specifies the current controller used by this instance of the Game Controller service.")]
        public Controller Controller
        {
            get { return _controller; }
            set { _controller = value; }
        }

        private Axes _axes = new Axes();
        [DataMember(IsRequired = true)]
        [Browsable(false)]
        [Description("Identifies the axes of the controller.")]
        public Axes Axes
        {
            get { return _axes; }
            set { _axes = value; }
        }

        private Buttons _buttons = new Buttons();
        [DataMember(IsRequired = true)]
        [Browsable(false)]
        [Description ("Identifies the buttons of the controller.")]
        public Buttons Buttons
        {
            get { return _buttons; }
            set { _buttons = value; }
        }

        private Sliders _sliders = new Sliders();
        [DataMember(IsRequired = true)]
        [Browsable(false)]
        [Description("Identifies the sliders of the controller.")]
        public Sliders Sliders
        {
            get { return _sliders; }
            set { _sliders = value; }
        }

        private PovHats _povHats = new PovHats();
        [DataMember(IsRequired = true)]
        [Description("Identifies the directional or Point-Of-View (POV) hats controllers.")]
        [Browsable(false)]
        public PovHats PovHats
        {
            get { return _povHats; }
            set { _povHats = value; }
        }

        public Substate Update(DateTime timestamp, input.JoystickState state)
        {
            _timeStamp = timestamp;

            Substate updated = Substate.None;

            updated |= _axes.Update(timestamp, state);
            updated |= _buttons.Update(timestamp, state.Buttons);
            updated |= _sliders.Update(timestamp, state.Sliders);
            updated |= _povHats.Update(timestamp, state.PovHats);

            return updated;
        }

        public Substate Update(DateTime timeStamp)
        {
            input.JoystickState state = _controller.GetState();

            if (state != null)
            {
                return Update(timeStamp, state);
            }
            return Substate.None;
        }
    }

    [Flags]
    [DataContract]
    public enum Substate
    {
        None = 0x00,
        Controller = 0x01,
        Axes = 0x02,
        Buttons = 0x04,
        Sliders = 0x08,
        PovHats = 0x10
    }

    [DataContract]
    public class Controller : IDisposable
    {
        private DateTime _timeStamp = DateTime.Now;
        [DataMember, Browsable(false)]
        [Description("Identifies the time (in ms) of the input reading for this instance.")]
        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }

        private Guid _instance;
        [DataMember, Browsable(false)]
        [Description("Specifies the unique identifier (GUID) for this instance.")]
        public Guid Instance
        {
            get { return _instance; }
            set { _instance = value; }
        }

        private Guid _product;
        [DataMember, Browsable(false)]
        [Description("Specifies the unique product identifier for this instance.")]
        public Guid Product
        {
            get { return _product; }
            set { _product = value; }
        }

        private string _instanceName;
        [DataMember]
        [Description("Specifies a user friendly name for this instance.")]
        public string InstanceName
        {
            get { return _instanceName; }
            set { _instanceName = value; }
        }

        private string _productName;
        [DataMember]
        [Description("Specifies a user friendly product name for this instance.")]
        public string ProductName
        {
            get { return _productName; }
            set { _productName = value; }
        }

        private bool _current;
        [DataMember, Browsable(false)]
        [Description("Specifies if this is the current controller.")]
        public bool Current
        {
            get { return _current; }
            set { _current = value; }
        }

        private input.Device _device;

        public Substate Update(DateTime timestamp, input.Device device)
        {
            _instance = device.Instance;
            _product = device.Product;
            _instanceName = device.InstanceName;
            _productName = device.ProductName;
            _current = true;

            if (_device != null)
            {
                _device.Dispose();
                _device = null;
            }
            _device = device;

            _timeStamp = timestamp;

            return Substate.Controller;
        }

        public input.JoystickState GetState()
        {
            if (_device != null)
            {
                return _device.GetState();
            }
            return null;
        }

        public bool FindInstance()
        {
            using(input.DirectInput di = new input.DirectInput())
            {
                using (input.DeviceCollection devices = di.Devices)
                {
                    bool found = false;

                    for (int pass = 0; pass < 5; pass++)
                    {
                        foreach (input.Device device in devices)
                        {
                            switch (pass)
                            {
                                case 0:
                                    if (device.Instance == _instance)
                                    {
                                        found = true;
                                    }
                                    break;
                                case 1:
                                    if (device.Product == _product)
                                    {
                                        found = true;
                                    }
                                    break;
                                case 2:
                                    if (device.InstanceName == _instanceName)
                                    {
                                        found = true;
                                    }
                                    break;
                                case 3:
                                    if (device.ProductName == _productName)
                                    {
                                        found = true;
                                    }
                                    break;
                                default:
                                    found = true;
                                    break;
                            }
                            if (found)
                            {
                                Update(DateTime.Now, device);
                                return true;
                            }
                            device.Dispose();
                        }
                    }
                }
            }
            return false;
        }

        public static IEnumerable<Controller> Attached
        {
            get
            {
                using(input.DirectInput di = new input.DirectInput())
                {
                    foreach (input.Device device in di.Devices)
                    {
                        yield return FromDevice(device);
                        device.Dispose();
                    }
                }
            }
        }

        private static Controller FromDevice(input.Device device)
        {
            Controller controller = new Controller();

            controller.Instance = device.Instance;
            controller.Product = device.Product;
            controller.InstanceName = device.InstanceName;
            controller.ProductName = device.ProductName;

            return controller;
        }

        public void Dispose()
        {
            if (_device != null)
            {
                _device.Dispose();
                _device = null;
            }
        }
    }

    [DataContract]
    [Description ("Identifies the controller axes.")]
    public class Axes
    {
        private DateTime _timeStamp = DateTime.Now;
        [DataMember]
        [Description("Identifies the time (in ms) of the reading.")]
        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }

        private int _x;
        [DataMember]
        [Description("Identifies the horizontal (X) axis value.")]
        public int X
        {
            get { return _x; }
            set { _x = value; }
        }

        private int _y;
        [DataMember]
        [Description("Identifies the vertical (Y) axis value.")]
        public int Y
        {
            get { return _y; }
            set { _y = value; }
        }

        private int _z;
        [DataMember]
        [Description("Identifies the Z-axis value.")]
        public int Z
        {
            get { return _z; }
            set { _z = value; }
        }

        private int _rx;
        [DataMember]
        [Description("Identifies horizontal (X) axis rotation value.")]
        public int Rx
        {
            get { return _rx; }
            set { _rx = value; }
        }

        private int _ry;
        [DataMember]
        [Description("Identifies vertical (Y) axis rotation value.")]
        public int Ry
        {
            get { return _ry; }
            set { _ry = value; }
        }

        private int _rz;
        [DataMember]
        [Description("Identifies z-axis rotation value.")]
        public int Rz
        {
            get { return _rz; }
            set { _rz = value; }
        }

        public Substate Update(DateTime timestamp, input.JoystickState state)
        {
            Substate updated = Substate.None;

            if (_x != state.X ||
                _y != state.Y ||
                _z != state.Z ||
                _rx != state.Rx ||
                _ry != state.Ry ||
                _rz != state.Rz)
            {
                _x = state.X;
                _y = state.Y;
                _z = state.Z;
                _rx = state.Rx;
                _ry = state.Ry;
                _rz = state.Rz;

                _timeStamp = timestamp;

                updated = Substate.Axes;
            }
            return updated;
        }
    }

    [DataContract]
    [Description("Identifies the controller buttons.")]
    public class Buttons
    {
        private DateTime _timeStamp = DateTime.Now;
        [DataMember]
        [Description("Identifies the time (in ms) of the reading.")]
        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }

        private List<bool> _pressed = new List<bool>();
        [DataMember(IsRequired=true)]
        [Description("Identifies the pressed state of the set of buttons.")]
        public List<bool> Pressed
        {
            get { return _pressed; }
            set { _pressed = value; }
        }

        public Substate Update(DateTime timestamp, bool[] buttons)
        {
            Substate updated = Substate.None;

            if (_pressed.Count != buttons.Length)
            {
                _pressed = new List<bool>(buttons);
                updated = Substate.Buttons;
            }
            else
            {
                for (int i = 0; i < buttons.Length; i++)
                {
                    if (_pressed[i] != buttons[i])
                    {
                        _pressed[i] = buttons[i];
                        updated = Substate.Buttons;
                    }
                }
            }

            if (updated != Substate.None)
            {
                _timeStamp = timestamp;
            }
            return updated;
        }
    }

    [DataContract]
    [Description("Identifies the controller sliders.")]
    public class Sliders
    {
        private DateTime _timeStamp = DateTime.Now;
        [DataMember]
        [Description("Identifies the time (in ms) of the current reading.")]
        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }

        private List<int> _position = new List<int>();
        [DataMember(IsRequired = true)]
        [Description("Identifies the set of position values of the sliders.")]
        public List<int> Position
        {
            get { return _position; }
            set { _position = value; }
        }

        public Substate Update(DateTime timestamp, int[] sliders)
        {
            Substate updated = Substate.None;

            if (_position.Count != sliders.Length)
            {
                _position = new List<int>(sliders);
                updated = Substate.Sliders;
            }
            else
            {
                for (int i = 0; i < sliders.Length; i++)
                {
                    if (_position[i] != sliders[i])
                    {
                        _position[i] = sliders[i];
                        updated = Substate.Sliders;
                    }
                }
            }

            if (updated != Substate.None)
            {
                _timeStamp = timestamp;
            }
            return updated;
        }
    }

    [DataContract]
    [Description("Identifies the current value of the directional or Point-Of-View (POV) hat controls.")]
    public class PovHats
    {
        private DateTime _timeStamp = DateTime.Now;
        [DataMember]
        [Description("Identifies the time (in ms) of the reading.")]
        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }

        private List<int> _direction = new List<int>();
        [DataMember(IsRequired = true)]
        [Description("Identifies the set of directional values of the control.")]
        public List<int> Direction
        {
            get { return _direction; }
            set { _direction = value; }
        }

        public Substate Update(DateTime timestamp, int[] povHats)
        {
            Substate updated = Substate.None;

            if (_direction.Count != povHats.Length)
            {
                _direction = new List<int>(povHats);
                updated = Substate.PovHats;
            }
            else
            {
                for (int i = 0; i < povHats.Length; i++)
                {
                    if (_direction[i] != povHats[i])
                    {
                        _direction[i] = povHats[i];
                        updated = Substate.PovHats;
                    }
                }
            }

            if (updated != Substate.None)
            {
                _timeStamp = timestamp;
            }
            return updated;
        }
    }

    [DataContract]
    public class PollRequest
    {
    }

    [DataContract]
    public class GetControllersRequest
    {
    }

    [DataContract]
    public class GetControllersResponse
    {
        private List<Controller> _controllers = new List<Controller>();
        [DataMember(IsRequired = true)]
        [Description ("Identifies the set of controllers.")]
        public List<Controller> Controllers
        {
            get { return _controllers; }
            set { _controllers = value; }
        }
    }

    [ServicePort]
    public class GameControllerOperations : PortSet<DsspDefaultLookup, DsspDefaultDrop, Get, Replace, Poll, ChangeController, UpdateAxes, UpdateButtons, UpdatePovHats, UpdateSliders, Subscribe, GetControllers>
    {
    }

    [Description("Gets the current state of the controller.")]
    public class Get : Get<GetRequestType, PortSet<GameControllerState, Fault>>
    {
    }

    [Description("Changes (or indicates a change to) the entire state of the controller.\nThis is sent initially to every connection.")]
    public class Replace : Replace<GameControllerState, PortSet<DefaultReplaceResponseType, Fault>>
    {
        public Replace()
        {
        }

        public Replace(GameControllerState body)
            : base(body)
        {
        }
    }

    [DisplayName("Poll")]
    [Description("Polls the controller and updates the state since the controller was last polled.\nThe controller is automatically polled 20 times a second.")]
    public class Poll : Submit<PollRequest, PortSet<DefaultSubmitResponseType, Fault>>
    {
        public Poll()
        {
        }

        public Poll(PollRequest body)
            : base(body)
        {
        }
    }

    [Description("Changes which game controller is in use.")]
    public class ChangeController : Update<Controller, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    [Description("Indicates when one of the axes is moved.")]
    public class UpdateAxes : Update<Axes, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    [Description("Indicates when one or more buttons is pressed or released.")]
    public class UpdateButtons : Update<Buttons, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    [Description("Indicates when the position of a slider is changed.")]
    public class UpdateSliders : Update<Sliders, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    [Description("Indicates when the position of a Point-Of-View (POV) hat switch is changed.")]
    public class UpdatePovHats : Update<PovHats, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    public class Subscribe : Subscribe<SubscribeRequestType, PortSet<SubscribeResponseType, Fault>>
    {
    }

    [Description("Returns a list of attached controllers.")]
    public class GetControllers : Query<GetControllersRequest, PortSet<GetControllersResponse, Fault>>
    {
    }
}
