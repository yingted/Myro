using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using System.ComponentModel;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Ccr.Core;
using W3C.Soap;

using vector = Myro.Services.Generic.Vector;
using brick = Myro.Services.Scribbler.ScribblerBase.Proxy;

namespace Myro.Services.Scribbler.LED
{
    public class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/schemas/2008/06/scribblerled.html";
    }

    [DisplayName("Scribbler LED service")]
    [Description("The Scribbler LED service")]
    [Contract(Contract.Identifier)]
    [AlternateContract(vector.Contract.Identifier)]
    class ScribblerLED : vector.VectorService
    {
        [Partner("ScribblerBase",
            Contract = brick.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate,
            Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        public ScribblerLED(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
            _state = new vector.VectorState(
                new List<double>() { 0.0, 0.0, 0.0 },
                new List<string>() { "left", "middle", "right" },
                DateTime.Now);
        }

        protected override void SetCallback(Myro.Services.Generic.Vector.SetRequestInfo request)
        {
            if (request is vector.SetAllRequestInfo)
                Activate(Arbiter.Choice(
                    _scribblerPort.SetAllLEDs(new brick.SetAllLedsBody() { LeftLED = _state.GetBool(0), CenterLED = _state.GetBool(1), RightLED = _state.GetBool(2) }),
                    delegate(DefaultUpdateResponseType success) { },
                    delegate(Fault failure) { LogError(failure); }));
            else if (request is vector.SetElementsRequestInfo)
            {
                vector.SetElementsRequestInfo info = (vector.SetElementsRequestInfo)request;
                if (info.Indices.Count > 1)
                    Activate(Arbiter.Choice(
                        _scribblerPort.SetAllLEDs(new brick.SetAllLedsBody() { LeftLED = _state.GetBool(0), CenterLED = _state.GetBool(1), RightLED = _state.GetBool(2) }),
                    delegate(DefaultUpdateResponseType success) { },
                    delegate(Fault failure) { LogError(failure); }));
                else if (info.Indices.Count == 1)
                    Activate(Arbiter.Choice(
                        _scribblerPort.SetLED(new brick.SetLedBody() { LED = info.Indices[0], State = _state.GetBool(info.Indices[0]) }),
                    delegate(DefaultUpdateResponseType success) { },
                    delegate(Fault failure) { LogError(failure); }));
            }
        }
    }
}
