//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: NxtDirect.cs $ $Revision$
//-----------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Security.Permissions;
using brick = Microsoft.Robotics.Services.LegoNxt;
using W3C.Soap;

namespace Microsoft.Robotics.Services.LegoNxt.NxtDirect
{

    [Contract(Contract.Identifier)]
    [DisplayName("LEGO® NXT Direct Commands")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT Direct Commands.")]
    public class NxtDirectService : DsspServiceBase
    {
        [ServicePort("/LegoNxt/directcommands", AllowMultipleInstances = true)]
        private NxtDirectOperations _nxtDirectPort = new NxtDirectOperations();

        private NxtDirectState _state = new NxtDirectState();

        [Partner("LegoNxt", 
            Contract = brick.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.UseExisting, 
            Optional = false)]
        private LegoNxtOperations _mainPort = new LegoNxtOperations();

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public NxtDirectService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {
			
        }

        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
            InitializeState();

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local service Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");
        }

        private void InitializeState()
        {
            _state.Status = "Not connected";

            DsspDefaultLookup lu = new DsspDefaultLookup();
            _mainPort.Post(lu);
            Activate(Arbiter.Choice(lu.ResponsePort,
                delegate(LookupResponse response) 
                { 
                    _state.Status = "Connected"; 
                    _state.BrickService = response.HttpUri().AbsoluteUri; 
                },
                delegate(Fault fault) { _state.Status = "Failed to connect"; }));
        }

        /// <summary>
        /// standard Get handler returns state
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> GetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }


        /// <summary>
        /// Get Input Values Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoGetInputValuesHandler(SendLegoGetInputValues submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            _mainPort.Post(cmd);
            yield return Arbiter.Choice(cmd.ResponsePort,
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseGetInputValues)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });

            yield break;
        }


        /// <summary>
        /// Get Button State Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoGetButtonStateHandler(SendLegoGetButtonState submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            _mainPort.Post(cmd);
            yield return Arbiter.Choice(cmd.ResponsePort,
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseGetButtonState)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });

            yield break;
        }

        /// <summary>
        /// LegoGetOutputState Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoGetOutputStateHandler(SendLegoGetOutputState submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            _mainPort.Post(cmd);

            yield return Arbiter.Choice(cmd.ResponsePort,
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseGetOutputState)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });

            yield break;
        }

        /// <summary>
        /// LegoKeepAlive Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoKeepAliveHandler(SendLegoKeepAlive submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            _mainPort.Post(cmd);

            yield return Arbiter.Choice(cmd.ResponsePort,
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseKeepAlive)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });
            yield break;
        }

        /// <summary>
        /// LegoLSGetStatus Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoLSGetStatusHandler(SendLegoLSGetStatus submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            _mainPort.Post(cmd);

            yield return Arbiter.Choice(cmd.ResponsePort,
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseLSGetStatus)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });
            yield break;
        }

        /// <summary>
        /// LegoLSRead Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoLSReadHandler(SendLegoLSRead submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            _mainPort.Post(cmd);

            yield return Arbiter.Choice(cmd.ResponsePort,
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseLSRead)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });
            yield break;
        }

        /// <summary>
        /// LegoLSWrite Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoLSWriteHandler(SendLegoLSWrite submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoMessageRead Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoMessageReadHandler(SendLegoMessageRead submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            _mainPort.Post(cmd);

            yield return Arbiter.Choice(cmd.ResponsePort,
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseMessageRead)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });
            yield break;
        }

        /// <summary>
        /// LegoMessageWrite Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoMessageWriteHandler(SendLegoMessageWrite submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoResetInputScaledValue Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoResetInputScaledValueHandler(SendLegoResetInputScaledValue submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoResetMotorPosition Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoResetMotorPositionHandler(SendLegoResetMotorPosition submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoSetInputMode Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoSetInputModeHandler(SendLegoSetInputMode submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoSetOutputState Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoSetOutputStateHandler(SendLegoSetOutputState submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

    }
}
