//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.ComponentModel;


using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Services.Serializer;
using dssp = Microsoft.Dss.ServiceModel.Dssp;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using W3C.Soap;

using SharpLogic.Robotics.Surveyor.Srv1;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1
{
    [Contract(Contract.Identifier)]
    [ActivationSettings(ShareDispatcher = false, ExecutionUnitsPerDispatcher = 1)]
    [DisplayName("SRV-1 Controller")]
    [Description("Provides access to the Surveyor SRV-1 controller service.")]
    public partial class Srv1Service : DsspServiceBase
    {
        // shared access to state is protected by the interleave pattern
        // when we Activate the handlers
        [InitialStatePartner(Optional = false)]
        Srv1State _state = new Srv1State();

        [ServicePort("/srv1", AllowMultipleInstances = false)]
        Srv1Operations _mainPort = new Srv1Operations();

        [Partner(dssp.Partners.SubscriptionManagerString, Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();
        string _subMgrUri;

        // SRV-1
        static internal Srv1Controller Srv1Controller = null;

        public Srv1Service(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
        }

        protected override void Start()
        {
            dssp.PartnerType p = FindPartner(dssp.Partners.SubscriptionManager, ServiceInfo.PartnerList);

            if (p != null && p.Service != null)
            {
                _subMgrPort = ServiceForwarder<submgr.SubscriptionManagerPort>(p.Service);
                _subMgrUri = p.Service;
            }

            try { InitControllerInternal(); }
            catch
            {
                LogError("Service failed to initialize. Shutting down.");
                Shutdown();
            }

            ActivateDsspOperationHandlers();

            DirectoryInsert();

            Console.WriteLine("COM is: COM" + _state.ComPort);
        }

        #region DSS Handlers

        /// <summary>
        /// Get Handler returns Srv1Service State.
        /// </summary>
        /// <param name="get"></param>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> GetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        /// <summary>
        /// Replace Handler returns Srv1Service State.
        /// </summary>
        /// <param name="replace"></param>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReplaceHandler(Replace replace)
        {
            int comPort = replace.Body.ComPort;
            if (comPort > 0)
            {
                Srv1Controller.PortName = "COM" + comPort;
                Srv1Controller = Srv1Controller.Srv1Factory();

                _subMgrPort.Post(new submgr.Submit(_state, DsspActions.ReplaceRequest));
                _state = replace.Body;
            }
            yield break;
        }

        /// <summary>
        /// Drop Handler shuts down Srv1Service
        /// </summary>
        /// <param name="drop"></param>
        [ServiceHandler(ServiceHandlerBehavior.Teardown)]
        public IEnumerator<ITask> DropHandler(DsspDefaultDrop drop)
        {
            Srv1Controller.Dispose();

            base.DefaultDropHandler(drop);
            yield break;
        }

        /// <summary>
        /// Subscribe Handler adds another subscriber to Srv1Service
        /// </summary>
        /// <param name="subscribe"></param>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(Subscribe subscribe)
        {
            base.SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort);
            yield break;
        }

        /// <summary>
        /// Initializes the SRV-1 Controller
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> InitControllerHandler(InitController init)
        {
            try
            {
                if (Srv1Controller == null)
                {
                    InitControllerInternal();                    
                }
                init.ResponsePort.Post(new DefaultSubmitResponseType());
            }
            catch
            {
                init.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                    "Invalid COM port")
                );
            }
            yield break;
        }
        #endregion

        public void InitControllerInternal()
        {
            int comPort = this._state.ComPort;           
            if (comPort > 0)
            {
                Srv1Controller.PortName = "COM" + comPort;
                Srv1Controller = Srv1Controller.Srv1Factory();
            }
            else
            {
                LogError("Surveyor SRV-1 Controller failed to initialize. Please configure the COM port.");
                throw new Exception();
            }
        }
    }
}
