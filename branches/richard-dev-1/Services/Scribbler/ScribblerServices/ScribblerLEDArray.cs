//------------------------------------------------------------------------------
// Scribbler LED Array Service
//
//  Implements the generic LEDArray1D contract
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
using System.Security.Permissions;
using xml = System.Xml;
using soap = W3C.Soap;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using W3C.Soap;

using brick = IPRE.ScribblerBase.Proxy;
using led = IPREGenericContracts.LEDarray.Proxy;
using myro = MyroInterfaces;

namespace IPRE.ScribblerLEDArray
{
    [DisplayName("Scribbler LED Array")]
    [Description("The Scribbler LED Array Service")]
    [Contract(Contract.Identifier)]
    [AlternateContract(led.Contract.Identifier)] //implementing the generic contract
    public class ScribblerLEDArray : DsspServiceBase
    {
        private ScribblerLEDArrayState _state;
        private led.LedarrayState _alternatestate;

        [ServicePort("scribblerledarray", AllowMultipleInstances = false)]
        private ScribblerLEDArrayOperations _mainPort = new ScribblerLEDArrayOperations();

        [AlternateServicePort("ledarray",
           AllowMultipleInstances = false, AlternateContract = led.Contract.Identifier)]
        private led.LedarrayOperations _alternatePort = new led.LedarrayOperations();

        [Partner("ScribblerBase", 
            Contract = brick.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, 
            Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        [Partner("SubMgr", 
            Contract = submgr.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.CreateAlways, 
            Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private bool _subscribed = false;

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public ScribblerLEDArray(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
        }

        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
            // Alternate Port Operations handlers
            Activate(Arbiter.ReceiveWithIterator<led.Get>(true, _alternatePort, AlternateGetHandler));
            // Alternate Port Operations handlers
            Activate(Arbiter.ReceiveWithIterator<led.SetSingle>(true, _alternatePort, AlternateSetSingleHandler));
            // Alternate Port Operations handlers
            Activate(Arbiter.ReceiveWithIterator<led.SetVector>(true, _alternatePort, AlternateSetVectorHandler));

            led.LEDVector ledvec = new led.LEDVector();
            List<led.LEDVector> leds = new List<led.LEDVector>(1);

            //configure default state
            if (_alternatestate == null)
            {
                ledvec.LEDVec = new List<led.LED>(3);
                ledvec.LEDVec.Add(new led.LED());
                ledvec.LEDVec.Add(new led.LED());
                ledvec.LEDVec.Add(new led.LED());

                _state = new ScribblerLEDArrayState();
                _state.LEDs = leds;
                _state.LEDs.Add(ledvec);

                //set hardware identifier equat to array index
                for (int i = 0; i < _state.LEDs.Count; i++)
                    _state.LEDs[0].LEDVec[i].HardwareIdentifier = i;
            }

            //configure alternate state
            if (_alternatestate == null)
            {
                _alternatestate = new led.LedarrayState();
                _alternatestate.LEDs = leds;
            }

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

            // display HTTP service Uri
            LogInfo(LogGroups.Console, "Service uri: ");

        }

        #region ScribblerLEDOperations
        /// <summary>
        /// Get Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }


        ///// <summary>
        ///// Replace Handler
        ///// </summary>
        //[ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        //public virtual IEnumerator<ITask> ReplaceHandler(led.Replace replace)
        //{
        //    _state = replace.Body;
        //    replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
        //    yield break;
        //}

        /// <summary>
        /// SetSingle LED handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> SetSingleHandler(SetSingle request)
        {
            //error check
            if (request.Body.Which >= _state.LEDs[0].LEDVec.Count || request.Body.Which < 0)
            {
                LogError("Improper LED Identifier");
                request.ResponsePort.Post(new Fault());
                yield break;
            }

            //update our state
            _state.LEDs[0].LEDVec[request.Body.Which].State = request.Body.State;
            _state.LEDs[0].LEDVec[request.Body.Which].TimeStamp = DateTime.Now;

            //send message to brick service
            brick.SetLedBody setSingle = new brick.SetLedBody();
            setSingle.LED = request.Body.Which;
            setSingle.State = request.Body.State;
            _scribblerPort.SetLED(setSingle);

            request.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }

        /// <summary>
        /// Set all LEDs with a vector
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> SetVectorHandler(SetVector request)
        {
            brick.SetAllLedsBody setAll = new brick.SetAllLedsBody();
            SetVectorRequest ledreq = request.Body;

            //decompose binary
            setAll.RightLED = ledreq.State.LEDVec[2].State;
            setAll.CenterLED = ledreq.State.LEDVec[1].State;
            setAll.LeftLED = ledreq.State.LEDVec[0].State;

            //update our state
            _state.LEDs[ledreq.Which].LEDVec[2].State = setAll.RightLED;
            _state.LEDs[ledreq.Which].LEDVec[1].State = setAll.CenterLED;
            _state.LEDs[ledreq.Which].LEDVec[0].State = setAll.LeftLED;
            _state.LEDs[ledreq.Which].LEDVec[0].TimeStamp = DateTime.Now;
            _state.LEDs[ledreq.Which].LEDVec[1].TimeStamp = DateTime.Now;
            _state.LEDs[ledreq.Which].LEDVec[2].TimeStamp = DateTime.Now;

            //send message to brick service
            _scribblerPort.SetAllLEDs(setAll);

            request.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }
        #endregion

        #region LEDArrayOperations
        /// <summary>
        /// Get Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent,PortFieldName="_alternatePort")]
        public virtual IEnumerator<ITask> AlternateGetHandler(led.Get get)
        {
            get.ResponsePort.Post(_alternatestate);
            yield break;
        }


        ///// <summary>
        ///// Replace Handler
        ///// </summary>
        //[ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        //public virtual IEnumerator<ITask> ReplaceHandler(led.Replace replace)
        //{
        //    _alternatestate = replace.Body;
        //    replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
        //    yield break;
        //}

        /// <summary>
        /// SetSingle LED handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive, PortFieldName = "_alternatePort")]
        public virtual IEnumerator<ITask> AlternateSetSingleHandler(led.SetSingle request)
        {
            //error check
            if (request.Body.Which >= _alternatestate.LEDs[0].LEDVec.Count || request.Body.Which < 0)
            {
                LogError("Improper LED Identifier");
                request.ResponsePort.Post(new Fault());
                yield break;
            }

            //update our state
            _alternatestate.LEDs[0].LEDVec[request.Body.Which].State = request.Body.State;
            _alternatestate.LEDs[0].LEDVec[request.Body.Which].TimeStamp = DateTime.Now;

            //send message to brick service
            brick.SetLedBody setSingle = new brick.SetLedBody();
            setSingle.LED = request.Body.Which;
            setSingle.State = request.Body.State;
            _scribblerPort.SetLED(setSingle);

            request.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }

        /// <summary>
        /// Set all LEDs with a vector
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive, PortFieldName = "_alternatePort")]
        public virtual IEnumerator<ITask> AlternateSetVectorHandler(led.SetVector request)
        {
            brick.SetAllLedsBody setAll = new brick.SetAllLedsBody();
            led.SetVectorRequest ledreq = request.Body;

            //decompose binary
            setAll.RightLED = ledreq.State.LEDVec[2].State;
            setAll.CenterLED = ledreq.State.LEDVec[1].State;
            setAll.LeftLED = ledreq.State.LEDVec[0].State;

            //update our state
            _alternatestate.LEDs[ledreq.Which].LEDVec[2].State = setAll.RightLED;
            _alternatestate.LEDs[ledreq.Which].LEDVec[1].State = setAll.CenterLED;
            _alternatestate.LEDs[ledreq.Which].LEDVec[0].State = setAll.LeftLED;
            _alternatestate.LEDs[ledreq.Which].LEDVec[0].TimeStamp = DateTime.Now;
            _alternatestate.LEDs[ledreq.Which].LEDVec[1].TimeStamp = DateTime.Now;
            _alternatestate.LEDs[ledreq.Which].LEDVec[2].TimeStamp = DateTime.Now;

            //send message to brick service
            _scribblerPort.SetAllLEDs(setAll);

            request.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }
        #endregion


        ///// <summary>
        ///// Set all LEDs with a volume display
        ///// </summary>
        //[ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        //public virtual IEnumerator<ITask> SetVolumeHandler(led.SetVolume request)
        //{
        //    brick.SetAllLedsBody setAll = new brick.SetAllLedsBody();

        //    //initialize
        //    setAll.RightLED = false;
        //    setAll.CenterLED = false;
        //    setAll.LeftLED = false;

        //    //decompose volume
        //    if (request.Body.Volume >= 0.1)
        //        setAll.RightLED = true;

        //    if (request.Body.Volume >= 0.4)
        //        setAll.CenterLED = true;

        //    if (request.Body.Volume >= 0.7)
        //        setAll.LeftLED = true;

        //    //update our state
        //    _alternatestate.LEDs[0].State = setAll.RightLED;
        //    _alternatestate.LEDs[1].State = setAll.CenterLED;
        //    _alternatestate.LEDs[2].State = setAll.LeftLED;
        //    _alternatestate.LEDs[0].TimeStamp = DateTime.Now;
        //    _alternatestate.LEDs[1].TimeStamp = DateTime.Now;
        //    _alternatestate.LEDs[2].TimeStamp = DateTime.Now;

        //    //send message to brick service
        //    _scribblerPort.SetAllLEDs(setAll);

        //    request.ResponsePort.Post(DefaultUpdateResponseType.Instance);
        //    yield break;
        //}


        //#region IMyroLED Members

        //public void setBinary(uint number)
        //{
        //    led.SetBinaryRequest req = new led.SetBinaryRequest();
        //    req.Binary = number;
        //    led.SetBinary msg = new led.SetBinary();
        //    msg.Body = req;
        //    _alternatePort.Post(msg);
        //}

        //public void setLED(string position, int state)
        //{
        //    throw new Exception("The method or operation is not implemented.");
        //}

        //public void setLED(string position, string state)
        //{
        //    throw new Exception("The method or operation is not implemented.");
        //}

        //#endregion
    }


}
