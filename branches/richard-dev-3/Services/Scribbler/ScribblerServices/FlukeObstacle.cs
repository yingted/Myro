// Copyright (c) Microsoft Corporation.  All rights reserved.

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Xml;
using W3C.Soap;
using Myro.Utilities;
using System.Linq;

using brick = Myro.Services.Scribbler.ScribblerBase.Proxy;
using vector = Myro.Services.Generic.Vector;

namespace Myro.Services.Scribbler.FlukeObstacle
{
    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/schemas/2008/06/flukeobstacle.html";
    }

    /// <summary>
    /// The Fluke obstacle Service
    /// </summary>
    [DisplayName("Fluke Obstacle Detector")]
    [Description("The Fluke Obstacle Detector")]
    [Contract(Contract.Identifier)]
    [AlternateContract(vector.Contract.Identifier)] //implementing the generic contract
    class FlukeObstacle : vector.VectorServiceBase
    {
        [ServicePort(AllowMultipleInstances = false)]
        vector.VectorOperations _operationsPort = new vector.VectorOperations();
        protected override vector.VectorOperations OperationsPort { get { return _operationsPort; } }

        /// <summary>
        /// Robot base partner
        /// </summary>
        [Partner("ScribblerBase", Contract = brick.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        /// <summary>
        /// Constructor
        /// </summary>
        public FlukeObstacle(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
            _state = new vector.VectorState(
                new List<double>() { 0.0, 0.0, 0.0 },
                new List<string>() { "left", "middle", "right" },
                DateTime.Now);
        }

        protected override IEnumerator<ITask> GetCallback(Myro.Services.Generic.Vector.GetRequestInfo request, PortSet<Myro.Services.Generic.Vector.CallbackResponseType, Fault> responsePort)
        {
            if (request is vector.GetAllRequestInfo)
            {
                PortSet<DefaultQueryResponseType, Fault> responses = new PortSet<DefaultQueryResponseType, Fault>();
                for (int i = 0; i < _state.Values.Count; i++)
                {
                    int myI = i;
                    Activate(Arbiter.Choice(_scribblerPort.GetObstacle(i),
                        delegate(brick.UInt16Body r)
                        {
                            //_state.Set(myI, RSUtils.NormalizeUShort(r.Value), DateTime.Now);
                            _state.Set(myI, r.Value, DateTime.Now);
                            responses.Post(DefaultQueryResponseType.Instance);
                        },
                        delegate(Fault f) { responses.Post(f); }));
                }
                yield return Arbiter.MultipleItemReceive(responses, _state.Values.Count,
                    delegate(ICollection<DefaultQueryResponseType> ss, ICollection<Fault> fs)
                    {
                        if (fs.Count == 0)
                            responsePort.Post(vector.CallbackResponseType.Instance);
                        else
                        {
                            responsePort.Post(fs.First());
                            //// f.Reason was sometimes null
                            //var reasons = new List<ReasonText>();
                            //foreach (var f in fs)
                            //    reasons.AddRange(f.Reason.AsEnumerable());
                            //responsePort.Post(new Fault() { Detail = new Detail() { Any = fs.ToArray() }, Reason = reasons.ToArray() });
                        }
                    });
            }
            else
            {
                var info = (vector.GetElementRequestInfo)request;
                yield return Arbiter.Choice(_scribblerPort.GetObstacle(info.Index),
                    delegate(brick.UInt16Body r)
                    {
                        _state.Set(info.Index, RSUtils.NormalizeUShort(r.Value), DateTime.Now);
                        responsePort.Post(vector.CallbackResponseType.Instance);
                    },
                    delegate(Fault f) { responsePort.Post(f); });
            }
            yield break;
        }
    }
}
