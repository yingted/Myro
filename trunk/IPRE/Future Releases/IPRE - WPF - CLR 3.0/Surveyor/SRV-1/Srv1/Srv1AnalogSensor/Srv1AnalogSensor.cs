//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1AnalogSensor.CS $ $Revision$
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
using analogarray = Microsoft.Robotics.Services.AnalogSensorArray.Proxy;
using analog = Microsoft.Robotics.Services.AnalogSensor.Proxy;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogSensor
{
    [DisplayName("SRV-1 Analog Sensor")]
    [Description("Provides access to a Surveyor SRV-1 analog sensor.")]
    [Contract(Contract.Identifier)]
    [AlternateContract(analogarray.Contract.Identifier)]
    partial class AnalogSensorService : DsspServiceBase
    {
        // shared access to state is protected by the interleave pattern
        // when we Activate the handlers
        AnalogSensorState _state = new AnalogSensorState();
        private analogarray.AnalogSensorArrayState _alternateState = null;

        [ServicePort("/srv1analogsensor")]
        Srv1AnalogSensorOperations _mainPort = new Srv1AnalogSensorOperations();

        [AlternateServicePort("/analogarray",AllowMultipleInstances=false,AlternateContract=analogarray.Contract.Identifier)]
        private analogarray.AnalogSensorOperations _alternatePort = new analogarray.AnalogSensorOperations();

        //ceate an update port to receive the update form SRV-1 controller
        SensorUpdateOperations _updatePort = new SensorUpdateOperations();

        Port<DateTime> _timerPort = new Port<DateTime>();

        [Partner(dssp.Partners.SubscriptionManagerString, Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        [Partner(srv1srv.Partners.Srv1, Contract = srv1srv.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        srv1srv.Srv1Operations _controllerPort = new srv1srv.Srv1Operations();

        // SRV-1
        private static Srv1Controller _srv1 = null;

        public AnalogSensorService(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
        }

        //protected override void Start()
        //{
        //    base.Start();
        //    MainPortInterleave.CombineWith(
        //        new Interleave(
        //            new TeardownReceiverGroup(),
        //            new ExclusiveReceiverGroup(
        //                Arbiter.Receive<SensorUpdate>(true, _updatePort, SensorUpdateHandler),
        //                Arbiter.Receive<SensorConfiguration>(true, _updatePort, SensorConfigurationHandler),
        //                Arbiter.Receive<DateTime>(true, _timerPort, TimerHandler)
        //            ),
        //            new ConcurrentReceiverGroup()
        //        )
        //    );
        //}

        protected override void Start()
        {
            // Alternate Port Operations handlers
            Activate(Arbiter.ReceiveWithIterator<analogarray.Get>(true, _alternatePort, AlternateGetHandler));

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

            // Start the timer
            this._timerPort.Post(DateTime.Now);
            
            successPort.Post(true);
            yield break;
        }

        private void StartBehavior()
        {
            base.Start();
            MainPortInterleave.CombineWith(
                new Interleave(
                    new TeardownReceiverGroup(),
                    new ExclusiveReceiverGroup(
                        Arbiter.Receive<SensorUpdate>(true, _updatePort, SensorUpdateHandler),
                        Arbiter.Receive<SensorConfiguration>(true, _updatePort, SensorConfigurationHandler),
                        Arbiter.Receive<DateTime>(true, _timerPort, TimerHandler)
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
        /// Drop Handler shuts down Srv1AnalogSensorService
        /// </summary>
        /// <param name="drop"></param>
        [ServiceHandler(ServiceHandlerBehavior.Teardown)]
        public void DropHandler(DsspDefaultDrop drop)
        {
            base.DefaultDropHandler(drop);
            base.Shutdown();
        }

        #endregion // DSS handlers

        #region custom handlers
        private void TimerHandler(DateTime signal)
        {
            const int msTimerInterval = 1000;

            this.UpdateIRState();

            Activate(
                Arbiter.Receive(false, base.TimeoutPort(msTimerInterval),
                    delegate(DateTime time)
                    {
                        _timerPort.Post(time);
                    }
                )
            );
        }

        private void UpdateIRState()
        {
            BounceIRResponse irResponse;
            try { irResponse = _srv1.GetProximity(); }
            catch (Exception ex)
            {
                base.LogError(ex);
                return;
            }
            int[] irValues = {
                irResponse.Front,
                irResponse.Left,
                irResponse.Back,
                irResponse.Right
            };

            for (int i = 0; i < irValues.Length; i++)
            {
                this._state.Sensors[i].value = irValues[i];
                Sensor_ValueChanged(i + 1, irValues[i]);
            }
        }

        /// <summary>
        /// Configuration Handler
        ///
        /// _state protected by interleave.
        /// </summary>
        /// <param name="replace"></param>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ConfigurationHandler(Configuration configuration)
        {
            configuration.ResponsePort.Post(new dssp.DefaultSubmitResponseType());
            _subMgrPort.Post(new submgr.Submit(_state, DsspActions.ReplaceRequest));
            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SensorInitHandler(SensorInit sensorInit)
        {
            UpdateSensorConfiguration(1, "Front IR Emitter", (int)Srv1SensorType.InfraRed);
            UpdateSensorConfiguration(2, "Left IR Emitter", (int)Srv1SensorType.InfraRed);
            UpdateSensorConfiguration(3, "Rear IR Emitter", (int)Srv1SensorType.InfraRed);
            UpdateSensorConfiguration(4, "Right IR Emitter", (int)Srv1SensorType.InfraRed);
            sensorInit.ResponsePort.Post(new DefaultSubmitResponseType());
            _subMgrPort.Post(new submgr.Submit(sensorInit.Body, DsspActions.UpdateRequest));
            yield break;
        }

        /// <summary>
        /// Process configuration string.
        /// </summary>
        /// <param name="state"></param>
        /// <param name="body"></param>

        void ProcessConfigurationString(ref AnalogSensorState state, string body)
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

                        if (0 == key.CompareTo("SENSORINIT"))
                        {
                            // Sensor=_index_,_name_,_type_
                            string[] indexAttrib = value.Split(new char[] { ',' });

                            int sensorID = Convert.ToInt32(indexAttrib[0]);

                            if ((sensorID > 0) && (sensorID <= 4) && (indexAttrib.Length == 3))
                            {
                                string name = indexAttrib[1].ToString();
                                int type = Convert.ToInt32(indexAttrib[2]);
                                UpdateSensorConfiguration(sensorID, name, type);
                            }
                        }
                        else if (0 == key.CompareTo("PIN"))
                        {
                        }
                        else if (0 == key.CompareTo("CONFIGURATION"))
                        {
                        }
                    }
                }
            }
        }

        #endregion custom handlers

        #region Sensor Configuration


        /// <summary>
        /// Internal handler, not on public port
        /// </summary>
        /// <param name="sensorConfiguration"></param>
        void SensorConfigurationHandler(SensorConfiguration sensorConfiguration)
        {
            int sensorID = sensorConfiguration.sensorID;
            string name = sensorConfiguration.name;
            Srv1SensorType type = (Srv1SensorType)sensorConfiguration.type;

            if ((sensorID > 0) && (sensorID <= 4))
            {
                _state.Sensors[sensorID - 1].type = type;
                _state.Sensors[sensorID - 1].name = name;

                sensorConfiguration.ResponsePort.Post(new DefaultUpdateResponseType());
                _subMgrPort.Post(new submgr.Submit(_state, DsspActions.ReplaceRequest));
            }
            else
            {
                sensorConfiguration.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                   "Bad Index")
                    );
            }
        }

        #endregion Sensor Configuration

        void SensorUpdateHandler(SensorUpdate sensorUpdate)
        {
            int sensorID = sensorUpdate.sensorID;
            if ((sensorID > 0) && (sensorID <= 4))
            {
                //LogInfo("--- Sensor State Updated " + sensorUpdate.value.ToString());
                _state.Sensors[sensorID - 1].value = sensorUpdate.value;
                _subMgrPort.Post(new submgr.Submit(_state, DsspActions.ReplaceRequest));
                sensorUpdate.ResponsePort.Post(new DefaultUpdateResponseType());
            }
            else
            {
                sensorUpdate.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                   "Bad Index")
                );
            }
        }

        #region config and update helper routines

        void UpdateSensorConfiguration(int sensorID, string name, int type)
        {
            SensorConfiguration config = new SensorConfiguration();
            config.sensorID = sensorID;
            config.name = name;
            config.type = type;

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

        private void Sensor_ValueChanged(int sensorID, int sensorValue)
        {
            SensorUpdate update = new SensorUpdate();
            update.sensorID = sensorID;
            update.value = sensorValue;
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

        #endregion config and update helper routines

        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> AlternateGetHandler(analogarray.Get get)
        {
            _alternateState = new analogarray.AnalogSensorArrayState();
            _alternateState.Sensors = new List<analog.AnalogSensorState>();
            analog.AnalogSensorState sens = new analog.AnalogSensorState();
            sens.RawMeasurement = _state.Sensors[0].value;
            _alternateState.Sensors.Add(sens);
            sens = new analog.AnalogSensorState();
            sens.RawMeasurement = _state.Sensors[1].value;
            _alternateState.Sensors.Add(sens);
            sens = new analog.AnalogSensorState();
            sens.RawMeasurement = _state.Sensors[2].value;
            _alternateState.Sensors.Add(sens);
            sens = new analog.AnalogSensorState();
            sens.RawMeasurement = _state.Sensors[3].value;
            _alternateState.Sensors.Add(sens);
            get.ResponsePort.Post(_alternateState);
            yield break;
        }
    }
}

