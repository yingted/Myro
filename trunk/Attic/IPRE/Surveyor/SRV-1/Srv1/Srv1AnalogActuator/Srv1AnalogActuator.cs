//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1AnalogActuator.CS $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using SharpLogic.Robotics.Surveyor.Srv1;
using System;
using System.Collections.Generic;
using System.ComponentModel;

using System.Text;
using W3C.Soap;

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Services.Serializer;
using Microsoft.Dss.ServiceModel.DsspServiceBase;

using dssp = Microsoft.Dss.ServiceModel.Dssp;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using srv1srv = SharpLogic.Robotics.Services.Surveyor.Srv1;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogActuator
{
    [Contract(Contract.Identifier)]
    [DisplayName("SRV-1 Analog Actuator")]
    [Description("Provides access to a Surveyor SRV-1 analog actuator.")]
    partial class AnalogActuatorService : DsspServiceBase
    {
        // shared access to state is protected by the interleave pattern
        AnalogActuatorState _state = new AnalogActuatorState();

        [ServicePort("/srv1analogactuator")]
        Srv1AnalogActuatorOperations _mainPort = new Srv1AnalogActuatorOperations();

        // ceate an update port to receive the update form SRV-1 controller
        ActuatorUpdateOperations _updatePort = new ActuatorUpdateOperations();

        // Partners
        [Partner(dssp.Partners.SubscriptionManagerString, Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        [Partner(srv1srv.Partners.Srv1, Contract = srv1srv.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        srv1srv.Srv1Operations _controllerPort = new srv1srv.Srv1Operations();

        // SRV-1
        private static Srv1Controller _srv1 = null;

        public AnalogActuatorService(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
        }

        protected override void Start()
        {
            Port<bool> successPort = new Port<bool>();
            SpawnIterator(successPort, StartIterator);
            Activate(Arbiter.Receive(false, successPort,
                delegate(bool success)
                {
                    if (!success)
                    {
                        LogError("Service failed to start. Shutting down.");
                        Shutdown();
                    }
                }
            ));
        }

        private IEnumerator<ITask> StartIterator(Port<bool> successPort)
        {
            bool success = false;

            InitController init = new InitController();
            _controllerPort.Post(init);
            yield return Arbiter.Choice(
                init.ResponsePort,
                delegate(DefaultSubmitResponseType response)
                {
                    // The controller initialized successfully.
                    _srv1 = srv1srv.Srv1Service.Srv1Controller;
                    success = true;
                },
                delegate(Fault fault) { }
            );

            if (!success)
            {
                successPort.Post(false);
                yield break;
            }

            // Activate service handlers.
            StartBehavior();
            successPort.Post(true);
            yield break;
        }

        private void StartBehavior()
        {
            base.Start();
            MainPortInterleave.CombineWith(
                Arbiter.Interleave
                (
                    new TeardownReceiverGroup(),
                    new ExclusiveReceiverGroup
                    (
                        Arbiter.Receive<ActuatorConfiguration>(true, _updatePort, ActuatorConfigurationHandler),
                        Arbiter.Receive<InternalSetActuatorValue>(true, _updatePort, SetActuatorValueHandler)
                    ),
                    new ConcurrentReceiverGroup()
                )
            );
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
            ProcessConfigurationString(ref _state, replace.Body.configuration);
            replace.ResponsePort.Post(new dssp.DefaultReplaceResponseType());
            _subMgrPort.Post(new submgr.Submit(_state, DsspActions.ReplaceRequest));
            _state = replace.Body;
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
        /// Drop Handler shuts down Srv1AnalogActuatorService
        /// </summary>
        /// <param name="drop"></param>
        [ServiceHandler(ServiceHandlerBehavior.Teardown)]
        public void DropHandler(DsspDefaultDrop drop)
        {
            base.DefaultDropHandler(drop);
            base.Shutdown();
        }

        #endregion // DSS handlers

        #region custom handers 
        /// <summary>
        /// Process configuration string
        /// </summary>
        /// <param name="state"></param>
        /// <param name="body"></param>
        void ProcessConfigurationString(ref AnalogActuatorState state, string body)
        {
            if ((body != null) && (body.Length > 0))
            {
                string[] sections = body.Split(new char[] { '&' });

                foreach (string rawPair in sections)
                {
                    string[] keyValuePair = rawPair.Split(new char[] { '=' }, 2);

                    if (keyValuePair.Length == 2)
                    {
                        string key = Uri.UnescapeDataString(keyValuePair[0]).ToUpper();
                        string value = Uri.UnescapeDataString(keyValuePair[1]);
                        if (0 == key.CompareTo("ACTUATORINIT"))
                        {
                            // ACTUATORINIT=_index_,_name_,_value_

                            string[] indexAttrib = value.Split(new char[] { ',' });

                            int actuatorID = Convert.ToInt32(indexAttrib[0]);

                            if ((actuatorID > 0) && (actuatorID <= 2) && (indexAttrib.Length == 3))
                            {
                                UpdateActuatorConfiguration(
                                    actuatorID,
                                    indexAttrib[1].ToString(),
                                    Convert.ToInt32(indexAttrib[2]));
                            }
                        }
                        else if (0 == key.CompareTo("ACTUATORSET"))
                        {
                            // ACTUATORSET=_value1_,_value2_,_value3_

                            string[] indexAttrib = value.Split(new char[] { ',' });

                            if (indexAttrib.Length == 3)
                                for (int i = 0; i < 3; i++)
                                {
                                    SetActuator_Value(i + 1, Convert.ToInt32(indexAttrib[i]));
                                }
                        }

                    }
                }
            }
        }

        void UpdateActuatorConfiguration(int actuatorID, string name, int actuatorValue)
        {
            ActuatorConfiguration config = new ActuatorConfiguration();
            config.actuatorID = actuatorID;
            config.name = name;
            config.value = actuatorValue;
            config.ResponsePort = new DsspResponsePort<DefaultUpdateResponseType>();

            _updatePort.Post(config);

            Activate(
            Arbiter.Choice(config.ResponsePort,
                delegate(DefaultUpdateResponseType response)
                {
                },
                delegate(Fault fault)
                {
                    LogError("Failed to send notification");
                }
            ));
        }


        private void SetActuator_Value(int actuatorID, float? actuatorValue)
        {
            if (actuatorValue == null)
                return;

            InternalSetActuatorValue update = new InternalSetActuatorValue();
            update.actuatorID = actuatorID;
            update.value = (float)actuatorValue;
            update.ResponsePort = new DsspResponsePort<DefaultUpdateResponseType>();

            _updatePort.Post(update);

            Activate(
            Arbiter.Choice(update.ResponsePort,
                delegate(DefaultUpdateResponseType response)
                {
                },
                delegate(Fault fault)
                {
                    LogError("Failed to send notification");
                }
            ));
        }

        #region Actuator Configuration

        void ActuatorConfigurationHandler(ActuatorConfiguration actuatorconfiguration)
        {
            int actuatorID = actuatorconfiguration.actuatorID;

            if ((actuatorID > 0) && (actuatorID <= 2))
            {
                float value = _state.actuator[actuatorID - 1].value = actuatorconfiguration.value;
                _state.actuator[actuatorID - 1].name = actuatorconfiguration.name;

                LogVerbose("--- Actuator configuration " + actuatorID.ToString() + " updated to " + value);

                switch (actuatorID)
                {
                    case 1:
                        _srv1.LeftMotorPower = value;
                        break;
                    case 2:
                        _srv1.RightMotorPower = value;
                        break;
                }
                actuatorconfiguration.ResponsePort.Post(new DefaultUpdateResponseType());
            }
            else
            {
                actuatorconfiguration.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                   "Bad Index")
                    );
            }
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SetActuatorValuesHandler(SetActuatorValues setActuatorValues)
        {    
            if (setActuatorValues.Body.activateA)
                SetActuator_Value(1, setActuatorValues.Body.actuatorA);
            if (setActuatorValues.Body.activateB)
                SetActuator_Value(2, setActuatorValues.Body.actuatorB);
            setActuatorValues.ResponsePort.Post(new DefaultUpdateResponseType());
            yield break;
        }

        void SetActuatorValueHandler(InternalSetActuatorValue setActuatorValue)
        {
            int actuatorID = setActuatorValue.actuatorID;

            if ((actuatorID > 0) && (actuatorID <= 2))
            {
                float value = _state.actuator[actuatorID - 1].value = setActuatorValue.value;

                LogVerbose("--- Actuator " + actuatorID.ToString() + " updated to " + value);

                switch (actuatorID)
                {
                    case 1:
                        _srv1.LeftMotorPower = value;
                        break;
                    case 2:
                        _srv1.RightMotorPower = value;
                        break;
                }
                setActuatorValue.ResponsePort.Post(new DefaultUpdateResponseType());
            }
            else
            {
                setActuatorValue.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                   "Bad Index")
                    );
            }
        }

        #endregion 


        #endregion

    }
}
