//------------------------------------------------------------------------------
// ToneGenerator.cs
//
//     This code was generated by the DssNewService tool.
//
//------------------------------------------------------------------------------

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

using brick = Myro.Services.Scribbler.ScribblerBase.Proxy;
using vector = Myro.Services.Generic.Vector;

namespace IPRE.ToneGenerator
{
    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/scribblertonegenerator.html";
    }

    /// <summary>
    /// The Tone Generator Service
    /// </summary>
    [DisplayName("Scribbler Tone Generator")]
    [Description("The Scribbler ToneGenerator Service")]
    [Contract(Contract.Identifier)]
    [AlternateContract(vector.Contract.Identifier)] //implementing the generic contract
    public class ScribblerToneGenerator : vector.VectorService
    {
        [Partner("ScribblerBase",
            Contract = brick.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate,
            Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();


        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public ScribblerToneGenerator(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
            _state = new vector.VectorState(
                new List<double> { 0.0, 0.0, 0.0 },
                new List<string> { "tone1", "tone2", "duration" },
                DateTime.Now);
        }

        public override IEnumerator<ITask> SetHandler(vector.SetByIndex set)
        {
            _state.Values[set.Body.Index] = set.Body.Value;
            _state.Timestamp = set.Body.Timestamp;
            SendNotification<vector.SetByIndex>(set);
            IEnumerator<ITask> ts = PlayTone(set.ResponsePort);
            do
            {
                yield return ts.Current;
            } while (ts.MoveNext());
            yield break;
        }

        public override IEnumerator<ITask> SetAllHandler(Myro.Services.Generic.Vector.SetAll setAll)
        {
            _state.Values = setAll.Body.Values;
            _state.Timestamp = setAll.Body.Timestamp;
            SendNotification<vector.SetAll>(setAll);
            IEnumerator<ITask> ts = PlayTone(setAll.ResponsePort);
            while (ts.MoveNext())
            {
                yield return ts.Current;
            };
            yield break;
        }

        private IEnumerator<ITask> PlayTone(PortSet<DefaultUpdateResponseType,Fault> responsePort)
        {
            int frequency1 = (int)Math.Round(_state.Values[0]);
            int frequency2 = (int)Math.Round(_state.Values[1]);
            int duration = (int)Math.Round(_state.Values[2]);
            if (frequency1 < 0 || frequency2 < 0 || duration < 0)
            {
                LogError("Improper PlayTone Frequency or Duration");
                yield break;
            }
            else
            {
                brick.PlayToneBody play = new brick.PlayToneBody();
                play.Frequency1 = frequency1;
                play.Frequency2 = frequency2;
                play.Duration = duration;
                yield return Arbiter.Choice(_scribblerPort.PlayTone(play),
                    delegate(DefaultUpdateResponseType response)
                    {
                        responsePort.Post(DefaultUpdateResponseType.Instance);
                    },
                    delegate(Fault fault)
                    {
                        responsePort.Post(fault);
                    }
                );
            }
        }
    }
}