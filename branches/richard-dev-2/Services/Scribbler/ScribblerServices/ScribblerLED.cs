using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using System.ComponentModel;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Ccr.Core;
using W3C.Soap;
using Myro.Utilities;

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
                new List<double>() { 0.0, 0.0, 0.0, 0.0, 0.0 },
                new List<string>() { "left", "middle", "right", "front", "back" },
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
                int setLEDScribblerIndex = -1; // will be set to -2 for set all
                foreach (int i in info.Indices)
                {
                    if (i >= 0)
                        if (i <= 2)
                            if (setLEDScribblerIndex > 0)
                                setLEDScribblerIndex = -2;
                            else
                                setLEDScribblerIndex = i;
                        else if (i == 3)
                            Activate(Arbiter.Choice(_scribblerPort.SetLEDFront(_state.GetBool(3)),
                                delegate(DefaultUpdateResponseType s) { },
                                delegate(Fault f) { LogError(f); }));
                        else if (i == 4)
                            Activate(Arbiter.Choice(_scribblerPort.SetLEDBack(RSUtils.UnnormalizeDouble(_state.Get(4))),
                                delegate(DefaultUpdateResponseType s) { },
                                delegate(Fault f) { LogError(f); }));
                    if (setLEDScribblerIndex == -2)
                        Activate(Arbiter.Choice(
                            _scribblerPort.SetAllLEDs(new brick.SetAllLedsBody() { LeftLED = _state.GetBool(0), CenterLED = _state.GetBool(1), RightLED = _state.GetBool(2) }),
                            delegate(DefaultUpdateResponseType success) { },
                            delegate(Fault failure) { LogError(failure); }));
                    else if (setLEDScribblerIndex >= 0)
                        Activate(Arbiter.Choice(
                            _scribblerPort.SetLED(new brick.SetLedBody() { LED = setLEDScribblerIndex, State = _state.GetBool(setLEDScribblerIndex) }),
                            delegate(DefaultUpdateResponseType success) { },
                            delegate(Fault failure) { LogError(failure); }));
                }
            }
        }
    }
}
