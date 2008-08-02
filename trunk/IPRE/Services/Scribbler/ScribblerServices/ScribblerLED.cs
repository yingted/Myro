// Copyright (c) Microsoft Corporation.  All rights reserved.

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
    class ScribblerLED : vector.VectorServiceBase
    {
        [ServicePort(AllowMultipleInstances = false)]
        vector.VectorOperations _operationsPort = new vector.VectorOperations();
        protected override vector.VectorOperations OperationsPort { get { return _operationsPort; } }

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

        protected override IEnumerator<ITask> SetCallback(vector.SetRequestInfo request, PortSet<vector.CallbackResponseType, Fault> responsePort)
        {
            if (request is vector.SetAllRequestInfo)
                Activate(Arbiter.Choice(
                    _scribblerPort.SetAllLEDs(new brick.SetAllLedsBody() { LeftLED = _state.GetBool(0), CenterLED = _state.GetBool(1), RightLED = _state.GetBool(2) }),
                    delegate(DefaultUpdateResponseType success) { responsePort.Post(vector.CallbackResponseType.Instance); },
                    delegate(Fault failure) { responsePort.Post(failure); }));
            else if (request is vector.SetElementsRequestInfo)
            {
                vector.SetElementsRequestInfo info = (vector.SetElementsRequestInfo)request;
                int setLEDScribblerIndex = -1; // will be set to -2 for set all
                var responses = new PortSet<DefaultUpdateResponseType, Fault>();
                int nResponses = 0;
                foreach (int i in info.Indices)
                {
                    if (i >= 0)
                        if (i <= 2)
                            if (setLEDScribblerIndex >= 0)
                                setLEDScribblerIndex = -2;
                            else
                                setLEDScribblerIndex = i;
                        else if (i == 3)
                        {
                            nResponses++;
                            Activate(Arbiter.Choice(_scribblerPort.SetLEDFront(_state.GetBool(3)),
                                delegate(DefaultUpdateResponseType s) { responses.Post(DefaultUpdateResponseType.Instance); },
                                delegate(Fault f) { responses.Post(f); }));
                        }
                        else if (i == 4)
                        {
                            nResponses++;
                            byte val =
                                (_state.Get(4) <= 0.0) ?
                                (byte)0 :
                                (byte)(_state.Get(4) * (255.0 - 170.0) + 170.0);
                            Activate(Arbiter.Choice(
                                _scribblerPort.SetLEDBack(val),
                                delegate(DefaultUpdateResponseType s) { responses.Post(DefaultUpdateResponseType.Instance); },
                                delegate(Fault f) { responses.Post(f); }));
                        }
                }
                if (setLEDScribblerIndex == -2)
                {
                    nResponses++;
                    Activate(Arbiter.Choice(
                        _scribblerPort.SetAllLEDs(new brick.SetAllLedsBody() { LeftLED = _state.GetBool(0), CenterLED = _state.GetBool(1), RightLED = _state.GetBool(2) }),
                            delegate(DefaultUpdateResponseType s) { responses.Post(DefaultUpdateResponseType.Instance); },
                            delegate(Fault f) { responses.Post(f); }));
                }
                else if (setLEDScribblerIndex >= 0)
                {
                    nResponses++;
                    Activate(Arbiter.Choice(
                        _scribblerPort.SetLED(new brick.SetLedBody() { LED = setLEDScribblerIndex, State = _state.GetBool(setLEDScribblerIndex) }),
                            delegate(DefaultUpdateResponseType s) { responses.Post(DefaultUpdateResponseType.Instance); },
                            delegate(Fault f) { responses.Post(f); }));
                }
                Activate(Arbiter.MultipleItemReceive(responses, nResponses,
                    delegate(ICollection<DefaultUpdateResponseType> ss, ICollection<Fault> fs)
                    {
                        if (fs.Count == 0)
                            responsePort.Post(vector.CallbackResponseType.Instance);
                        else
                        {
                            responsePort.Post(fs.First());
                            ////f.Readon could be null
                            //var reasons = new List<ReasonText>();
                            //foreach (var f in fs)
                            //    if (f.Reason != null)
                            //        reasons.AddRange(f.Reason.AsEnumerable());
                            //responsePort.Post(new Fault() { Detail = new Detail() { Any = fs.ToArray() }, Reason = reasons.ToArray() });
                        }
                    }));
            }
            yield break;
        }
    }
}
