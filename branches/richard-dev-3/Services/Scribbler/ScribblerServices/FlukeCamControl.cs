using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using Microsoft.Dss.Core.DsspHttp;
using W3C.Soap;

using b = Myro.Services.Scribbler.ScribblerBase;
using brick = Myro.Services.Scribbler.ScribblerBase.Proxy;

namespace Myro.Services.Scribbler.FlukeCamControl
{
    public static class Contract
    {
        [DataMember]
        public const string Identifier = "http://www.roboteducation.org/schemas/2008/06/flukecamcontrol.html";
    }

    [DataContract()]
    public class CamControlState
    {
        [DataMember()]
        public byte Val1;
        [DataMember()]
        public byte Val2;
        [DataMember()]
        public byte Brightness;
        [DataMember()]
        public byte Exposure;
        [DataMember()]
        public byte Darkness;
        [DataMember()]
        public bool AutoWhiteBalance;
        [DataMember()]
        public bool AutoGain;
        [DataMember()]
        public bool AutoExposure;
    }

    public class Get : Get<GetRequestType, PortSet<CamControlState, Fault>> { }
    public class Replace : Replace<CamControlState, PortSet<DefaultReplaceResponseType, Fault>>
    {
        public Replace() { }
        public Replace(CamControlState b) : base(b) { }
    }

    [ServicePort()]
    public class CamControlOperations : PortSet<
        DsspDefaultLookup, DsspDefaultDrop, HttpGet, Get, Replace> { }

    [DisplayName("Fluke Camera Control")]
    [Description("The Fluke Camera Control Service")]
    [Contract(Contract.Identifier)]
    class FlukeCamControl : DsspServiceBase
    {
        /// <summary>
        /// Robot base partner
        /// </summary>
        [Partner("ScribblerBase", Contract = brick.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        [ServicePort("camcontrol", AllowMultipleInstances = false)]
        private CamControlOperations _mainPort = new CamControlOperations();

        [ServiceState]
        private CamControlState _state = new CamControlState();

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="port"></param>
        public FlukeCamControl(DsspServiceCreationPort port) : base(port) { }

        protected override void Start()
        {
            base.Start();
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReplaceHandler(Replace req)
        {
            Fault fault = null;

            yield return (Arbiter.Choice(_scribblerPort.SetCamParam(0, req.Body.Darkness),
                delegate(DefaultUpdateResponseType r) { },
                delegate(Fault f) { fault = f; }));

            if (fault == null)
                yield return (Arbiter.Choice(_scribblerPort.SetCamParam(1, req.Body.Val1),
                    delegate(DefaultUpdateResponseType r) { },
                    delegate(Fault f) { fault = f; }));

            if (fault == null)
                yield return (Arbiter.Choice(_scribblerPort.SetCamParam(2, req.Body.Val2),
                    delegate(DefaultUpdateResponseType r) { },
                    delegate(Fault f) { fault = f; }));

            if (fault == null)
                yield return (Arbiter.Choice(_scribblerPort.SetCamParam(
                        b.ScribblerHelper.CameraParams.CAM_BRT, req.Body.Brightness),
                    delegate(DefaultUpdateResponseType r) { },
                    delegate(Fault f) { fault = f; }));

            if (fault == null)
                yield return (Arbiter.Choice(_scribblerPort.SetCamParam(
                        b.ScribblerHelper.CameraParams.CAM_EXP, req.Body.Exposure),
                    delegate(DefaultUpdateResponseType r) { },
                    delegate(Fault f) { fault = f; }));

            byte coma = req.Body.AutoWhiteBalance ?
                b.ScribblerHelper.CameraParams.CAM_COMA_WHITE_BALANCE_ON :
                b.ScribblerHelper.CameraParams.CAM_COMA_WHITE_BALANCE_OFF;
            if (fault == null)
                yield return (Arbiter.Choice(_scribblerPort.SetCamParam(
                        b.ScribblerHelper.CameraParams.CAM_COMA, coma),
                    delegate(DefaultUpdateResponseType r) { },
                    delegate(Fault f) { fault = f; }));

            byte comb = b.ScribblerHelper.CameraParams.CAM_COMB_DEFAULT;
            comb = (byte)(req.Body.AutoGain ?
                comb | b.ScribblerHelper.CameraParams.CAM_COMB_GAIN_CONTROL_ON :
                comb & b.ScribblerHelper.CameraParams.CAM_COMB_GAIN_CONTROL_OFF);
            comb = (byte)(req.Body.AutoExposure ?
                comb | b.ScribblerHelper.CameraParams.CAM_COMB_EXPOSURE_CONTROL_ON :
                comb & b.ScribblerHelper.CameraParams.CAM_COMB_EXPOSURE_CONTROL_OFF);
            if (fault == null)
                yield return (Arbiter.Choice(_scribblerPort.SetCamParam(
                        b.ScribblerHelper.CameraParams.CAM_COMB, comb),
                    delegate(DefaultUpdateResponseType r) { },
                    delegate(Fault f) { fault = f; }));

            if (fault == null)
            {
                req.ResponsePort.Post(DefaultReplaceResponseType.Instance);
                _state = req.Body;
            }
            else
                req.ResponsePort.Post(fault);

        }
    }
}
