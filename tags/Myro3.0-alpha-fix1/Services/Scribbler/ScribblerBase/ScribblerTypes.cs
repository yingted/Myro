//------------------------------------------------------------------------------
// ScribblerTypes.cs
//
//
//      Ben Axelrod 08/28/2006
//
//------------------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;
using Microsoft.Dss.Core.Attributes;
using W3C.Soap;
using Microsoft.Dss.Core.DsspHttp;

//[assembly: ContractNamespace(IPRE.ScribblerBase.Contract.Identifier, ClrNamespace = "IPRE.ScribblerBase")]
namespace Myro.Services.Scribbler.ScribblerBase
{

    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/schemas/2008/06/scribblerbase.html";
    }

    /// <summary>
    /// Main operations port
    /// </summary>
    public class ScribblerOperations : PortSet<
        DsspDefaultLookup,
        DsspDefaultDrop,
        Get,
        HttpGet,
        HttpPost,
        Replace,
        SetMotors,
        SetLED,
        SetAllLEDs,
        SetLEDFront,
        SetLEDBack,
        PlayTone,
        SetLoud,
        SetName,
        GetObstacle,
        ScribblerResponseMessage,
        SelectiveSubscribe, //IMPORTANT: Because SelectiveSubscribe inherits from Subscribe, it must go on top.
        Subscribe>
    {
    }

    public class SendScribblerCommand : Submit<ScribblerCommand, PortSet<ScribblerResponse, Fault>>
    {
        public SendScribblerCommand() { }
        public SendScribblerCommand(ScribblerCommand cmd)
        {
            this.Body = cmd;
        }
    }

    /// <summary>
    /// http get
    /// returns entire state
    /// </summary>
    public class Get : Get<GetRequestType, PortSet<ScribblerState, Fault>> { }

    /// <summary>
    /// replaces entire state
    /// </summary>
    public class Replace : Replace<ScribblerState, PortSet<DefaultReplaceResponseType, Fault>> { }

    /// <summary>
    /// a motor command
    /// </summary>
    public class SetMotors : Update<SetMotorsBody, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public SetMotors() { }

        public SetMotors(SetMotorsBody b)
        {
            base.Body = b;
        }
    }


    /// <summary>
    /// sends an LED message to Scribbler
    /// </summary>
    public class SetLED : Update<SetLedBody, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public SetLED() { }

        public SetLED(SetLedBody b)
        {
            base.Body = b;
        }
    }

    /// <summary>
    /// sets all LEDs on Scribbler
    /// </summary>
    public class SetAllLEDs : Update<SetAllLedsBody, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public SetAllLEDs() { }

        public SetAllLEDs(SetAllLedsBody b) : base(b) { }
    }

    /// <summary>
    /// sends an LED Front message to Scribbler
    /// </summary>
    public class SetLEDFront : Update<SetLEDFrontBody, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public SetLEDFront() { }

        public SetLEDFront(SetLEDFrontBody b) : base(b) { }
    }

    /// <summary>
    /// sends an LED Back message to Scribbler
    /// </summary>
    public class SetLEDBack : Update<SetLEDBackBody, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public SetLEDBack() { }

        public SetLEDBack(SetLEDBackBody b) : base(b) { }
    }

    /// <summary>
    /// Plays a tone on the scribbler
    /// </summary>
    public class PlayTone : Update<PlayToneBody, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public PlayTone() { }

        public PlayTone(PlayToneBody b)
        {
            base.Body = b;
        }
    }

    /// <summary>
    /// Turns loudness on or off
    /// </summary>
    public class SetLoud : Update<SetLoudBody, DsspResponsePort<DefaultUpdateResponseType>>
    {
        public SetLoud() : base() { }
        public SetLoud(SetLoudBody body) : base(body) { }
        public SetLoud(SetLoudBody body, DsspResponsePort<DefaultUpdateResponseType> respPort) : base(body, respPort) { }
    }

    /// <summary>
    /// Sets the Scribbler's name
    /// </summary>
    public class SetName : Update<SetNameBody, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public SetName() { }

        public SetName(SetNameBody b)
        {
            base.Body = b;
        }
    }

    [DataContract]
    [DataMemberConstructor]
    public class Int32Body
    {
        [DataMember]
        public Int32 Value;
        public Int32Body() {  }
        public Int32Body(Int32 value) { Value = value; }
    }

    [DataContract]
    [DataMemberConstructor]
    public class UInt16Body
    {
        [DataMember]
        public UInt16 Value;
        public UInt16Body() {  }
        public UInt16Body(UInt16 value) { Value = value; }
    }

    /// <summary>
    /// Get one of the Fluke obstacle sensors
    /// </summary>
    public class GetObstacle : Get<Int32Body, PortSet<UInt16Body, Fault>>
    {
        public GetObstacle() { }
        public GetObstacle(Int32Body b) : base(b) { }
    }

    /// <summary>
    /// updates the state after a return packet from robot
    /// </summary>
    public class ScribblerResponseMessage : Update<ScribblerResponse, PortSet<DefaultUpdateResponseType, Fault>>
    {
        public ScribblerResponseMessage() { }

        public ScribblerResponseMessage(ScribblerResponse b)
        {
            base.Body = b;
        }
    }


    ///// <summary>
    ///// configures 1 light sensor for subscriptions
    ///// </summary>
    //public class ConfigureSensor : Update<ConfigureSensorBody, PortSet<DefaultUpdateResponseType, Fault>> { }


    /// <summary>
    /// general subscription
    /// notifications on any state change
    /// </summary>
    public class Subscribe : Subscribe<SubscribeRequestType, PortSet<SubscribeResponseType, Fault>> { }

    /// <summary>
    /// a selective subscription
    /// subscribe to one or many sensor updates
    /// </summary>
    public class SelectiveSubscribe : Subscribe<MySubscribeRequestType, PortSet<SubscribeResponseType, Fault>> { }

}
