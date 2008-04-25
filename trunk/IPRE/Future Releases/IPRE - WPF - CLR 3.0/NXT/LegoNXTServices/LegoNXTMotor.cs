//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTMotor.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;

using System;
using System.Net;
using System.Collections.Generic;
using System.ComponentModel;
using System.Security.Permissions;

using W3C.Soap;
using motor = Microsoft.Robotics.Services.Motor.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using legoNXT = Microsoft.Robotics.Services.LegoNxt.Proxy;



namespace Microsoft.Robotics.Services.LegoNxt.Motor
{

    [Contract(Microsoft.Robotics.Services.LegoNxt.Motor.Contract.Identifier)]
    [AlternateContract(motor.Contract.Identifier)]
    [DisplayName("NXT Generic Motor")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT motor.\n(Uses the Generic Motor contract.)")]
    public class MotorService : DsspServiceBase
    {
        /// <summary>
        /// Manifest name for the initial state partner configuration settings
        /// </summary>
        [EmbeddedResource("Microsoft.Robotics.Services.LegoNxt.LegoNXTMotor.xslt")]
        string _transform = null;
        
        [InitialStatePartner(Optional = true)]
        private motor.MotorState _state;

        [ServicePort("LegoNxtMotor", AllowMultipleInstances = true)]
        private motor.MotorOperations _mainPort = new motor.MotorOperations();

        [Partner("LegoNxt", Contract = legoNXT.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private legoNXT.LegoNxtOperations _legoPort = new legoNXT.LegoNxtOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        public MotorService(DsspServiceCreationPort creationPort)
            :
                base(creationPort)
        {
            
        }

        protected override void Start()
        {
            //configure motor
            if (_state == null)
            {
                _state = new motor.MotorState("Unconfigured Motor", 1, 100, false, new PhysicalModel.Proxy.Pose());
                
                SaveState(_state);
            }

            // Listen for each of my handlers
            ActivateDsspOperationHandlers();

            // Insert ourselves into the directory so that others can find us
            DirectoryInsert();

            // display HTTP service Uri
            LogInfo(LogGroups.Console, "Service uri: ");
        }


        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> GetHandler(motor.Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> HttpGetHandler(HttpGet httpGet)
        {
            httpGet.ResponsePort.Post(new HttpResponseType(
                HttpStatusCode.OK,
                _state,
                _transform)
            );
            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReplaceHandler(motor.Replace replace)
        {
            _state = replace.Body;
            SaveState(_state);
            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            yield break;
        }

        private int _requestPending = 0;

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SetMotorPowerHandler(motor.SetMotorPower setMotorPower)
        {
            // For now, just dump requests that come in too fast.
            if (_requestPending > 0)
            {
                setMotorPower.ResponsePort.Post(new DefaultUpdateResponseType());
                yield break;
            }

            //flip direction if nessisary
            double revPow = setMotorPower.Body.TargetPower;
            if (_state.ReversePolarity)
                revPow = (-1) * revPow;

            //update state
            _state.CurrentPower = revPow;

            //convert power
            int power = (int)Math.Round(revPow * _state.PowerScalingFactor);

            //send hardware specific motor data
            legoNXT.LegoSetOutputState motordata = new legoNXT.LegoSetOutputState();
            motordata.OutputPort = _state.HardwareIdentifier - 1;
            motordata.PowerSetPoint = power;
            motordata.Mode = legoNXT.LegoOutputMode.PowerControl;
            motordata.RegulationMode = legoNXT.LegoRegulationMode.MotorSpeed;
            motordata.TurnRatio = 0;
            motordata.RunState = legoNXT.LegoRunState.Running;
            motordata.RotationLimit = 0; //run forever
            motordata.RequireResponse = false;
            _requestPending++;

            Activate(Arbiter.Choice(_legoPort.SendLegoCommand(motordata),
                delegate(legoNXT.LegoResponse status)
                {
                    _requestPending--;
                },
                delegate(Fault failure)
                {
                    _requestPending--;
                }
            ));

            //reply to sender
            setMotorPower.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }


    }

    public static class Contract
    {
        public const string Identifier = "http://schemas.microsoft.com/2006/06/legonxtmotor.html";
    }

}
