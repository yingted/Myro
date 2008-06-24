//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTBumper.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;

using System;
using System.Net;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Security.Permissions;
using xml = System.Xml;
using W3C.Soap;

using bumper = Microsoft.Robotics.Services.ContactSensor.Proxy;
using nxt = Microsoft.Robotics.Services.LegoNxt.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;

/* Notes on Lego NXT Binary Sensor
 *   Identifier must be the port number on the NXT Brick. Range: 1 - 4.
 */

namespace Microsoft.Robotics.Services.LegoNxt.ContactSensor
{
    
    [Contract(Contract.Identifier)]
    [AlternateContract(bumper.Contract.Identifier)]
    [DisplayName("NXT Generic Contact Sensors")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT touch or sonar sensors when used as a bumper or contact sensors.\n(Uses the Generic Contact Sensors contract.)")]
    public class ContactSensorService : DsspServiceBase
    {
        [EmbeddedResource("Microsoft.Robotics.Services.LegoNxt.NTXBumper.xslt")]
        string _transform = null;
        
        [InitialStatePartner(Optional = true, ServiceUri = "LEGO.NXT.Bumper.config.xml")]
        private LegoNxtBumperState _bumperConfigState = new LegoNxtBumperState();

        [ServicePort("/LegoNxt/ContactSensor", AllowMultipleInstances=true)]
        private ContactSensorArrayOperations _mainPort = new ContactSensorArrayOperations();

        private InternalOperations _internalPort = new InternalOperations();

        [Partner("LegoNxt", Contract = nxt.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, 
            Optional = false)]
        private nxt.LegoNxtOperations _legoPort = new nxt.LegoNxtOperations();

        // Lego Notifications
        nxt.LegoNxtOperations _notificationPort = new nxt.LegoNxtOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private bool _subscribed = false;

        public ContactSensorService(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
			
        }

        #region Initialization Code

        protected override void Start()
        {
            LogVerbose(LogGroups.Console, "Configuring Lego Contact Sensor Array... ");

            #region Initialize Service State
            bool saveState = false;
            if (_bumperConfigState == null)
            {
                _bumperConfigState = new LegoNxtBumperState();
                _bumperConfigState.ContactSensorArrayState = new bumper.ContactSensorArrayState();
                saveState = true;
            }
            else if (_bumperConfigState.Status != LegoNxtBumperStatus.Uninitialized)
            {
                _bumperConfigState.Status = LegoNxtBumperStatus.Uninitialized;
                saveState = true;
            }

            if (saveState)
            {
                SaveState(_bumperConfigState);
                saveState = false;
            }

            #endregion

            // Listen for each of my handlers
            Interleave mainInterleave = ActivateDsspOperationHandlers();

            // Add the internal only handlers
            mainInterleave.CombineWith(
                new Interleave(
                    new ExclusiveReceiverGroup(
                        Arbiter.Receive<UpdateStatus>(true, _internalPort, UpdateStatusHandler)
                    ), 
                    new ConcurrentReceiverGroup()));

            // Publish the service to the local Node Directory
            DirectoryInsert();

            #region Wait for Configuration to be completed
            Port<bool> donePort = new Port<bool>();
            SpawnIterator<Port<bool>>(donePort, InitializeBumperConnection);
            Activate(Arbiter.Receive(false, donePort,
                delegate(bool success)
                {
                    if (success)
                    {
                        // display HTTP service Uri
                        LogInfo(LogGroups.Console, "Service uri: ");
                    }
                    else
                    {
                        LogError(LogGroups.Console, "LEGO Bumper service failed to start.");
                        _mainPort.Post(new DsspDefaultDrop());
                    }

                }));
            #endregion
        }

        /// <summary>
        /// Multi-stage bumper initialization code
        /// </summary>
        /// <returns></returns>
        private IEnumerator<ITask> InitializeBumperConnection(Port<bool> donePort)
        {
            bool serviceSuccess = false;

            //configure default state
            Port<LegoNxtBumperState> configBumperPort = new Port<LegoNxtBumperState>();
            SpawnIterator<Port<LegoNxtBumperState>>(configBumperPort, GetLegoBumperConfiguration);
            yield return (Arbiter.Receive<LegoNxtBumperState>(false, configBumperPort,
                delegate(LegoNxtBumperState newBumperState)
                {
                    _bumperConfigState = newBumperState;
                }));

            // Subscribe to NXT for touch sensor notifications
            if (ValidState(_bumperConfigState.ContactSensorArrayState))
            {
                SaveState(_bumperConfigState);
                Port<bool> subscribeDonePort = new Port<bool>();
                SpawnIterator<Port<bool>>(subscribeDonePort, SubscribeToNXT);
                yield return(Arbiter.Receive<bool>(false, subscribeDonePort, 
                    delegate(bool success)
                    {
                        serviceSuccess = success;
                    }));
            }

            if (serviceSuccess)
            {
                yield return Arbiter.Choice(
                    UpdateServiceStatus(LegoNxtBumperStatus.ConnectedToNXT),
                        delegate(DefaultUpdateResponseType response) { },
                        delegate(Fault fault) { serviceSuccess = false; });
            }

            donePort.Post(serviceSuccess);
            yield break;
        }

        /// <summary>
        /// Get state from LEGO and set up a contact sensor
        /// for each of the configured ports.
        /// </summary>
        /// <param name="resultPort"></param>
        /// <returns></returns>
        private IEnumerator<ITask> GetLegoBumperConfiguration(Port<LegoNxtBumperState> resultPort)
        {
            LegoNxtBumperState configArray = new LegoNxtBumperState();
            nxt.LegoNxtState legoState = null;
            bool existingBumperConfig = (_bumperConfigState != null && _bumperConfigState.BumperConfigList != null && _bumperConfigState.BumperConfigList.Count > 0);
            bool validLegoState = false;
            int tryCount = 0;

            while (!validLegoState)
            {
                tryCount++;

                // See if the LEGO is configured.
                yield return Arbiter.Choice(_legoPort.Get(),
                    delegate(nxt.LegoNxtState legoNxtState)
                    {
                        legoState = legoNxtState;
                    },
                    delegate(Fault fault)
                    {
                        LogError(fault);
                    });

                validLegoState = (legoState != null
                    && legoState.BrickConfig != null
                    && legoState.BrickConfig.SensorPort != null
                    && legoState.BrickConfig.SensorPort.Length > 0
                    && legoState.ComPort > 0);

                // If we don't have valid configuration from the NXT Brick
                // wait a little and try again.
                if (!validLegoState)
                {
                    LogVerbose(LogGroups.Console, "Waiting for LEGO NXT to be initialized");
                    System.Threading.Thread.Sleep(500);
                }
            }

            configArray.ContactSensorArrayState = new bumper.ContactSensorArrayState();
            configArray.ContactSensorArrayState.Sensors = new List<bumper.ContactSensor>();
            configArray.BumperConfigList = new List<LegoNxtBumperConfig>();

            // We do not have a valid configuration from the NXT.
            if (!validLegoState)
            {
                // Do we have a valid prior configuration?
                if (existingBumperConfig)
                {
                    configArray.BumperConfigList = _bumperConfigState.BumperConfigList;
                    configArray.ContactSensorArrayState = _bumperConfigState.ContactSensorArrayState;
                }
                else
                {
                    // We have no prior configuration
                    LegoNxtBumperConfig bumperConfig = new LegoNxtBumperConfig();
                    bumperConfig.HardwareIdentifier = 1;
                    bumperConfig.SensorType = nxt.SensorDefinition.SensorType.Touch;
                    bumperConfig.Name = "Default Bumper 1";
                    bumperConfig.ThresholdLow = 1;
                    bumperConfig.ThresholdHigh = 1;
                    configArray.BumperConfigList.Add(bumperConfig);

                    bumper.ContactSensor defaultBumper = new bumper.ContactSensor();
                    defaultBumper.HardwareIdentifier = bumperConfig.HardwareIdentifier;
                    defaultBumper.Name = bumperConfig.Name;
                    configArray.ContactSensorArrayState.Sensors.Add(defaultBumper);
                }
            }
            else // We have valid LEGO NXT Configuration
            {
                int hardwareIdentifier = 1;
                foreach (nxt.SensorConfig sensorConfig in legoState.BrickConfig.SensorPort)
                {
                    LegoNxtBumperConfig bumperConfig = new LegoNxtBumperConfig();
                    bumperConfig.HardwareIdentifier = hardwareIdentifier;
                    bumperConfig.SensorType = sensorConfig.Type;

                    if (sensorConfig.Type ==  nxt.SensorDefinition.SensorType.Touch)
                    {
                        bumperConfig.Name = "Touch Bumper " + hardwareIdentifier.ToString();
                        bumperConfig.ThresholdLow = 1;
                        bumperConfig.ThresholdHigh = 1;
                    }
                    else if (sensorConfig.Type == nxt.SensorDefinition.SensorType.Sonar)
                    {
                        bumperConfig.Name = "Sonar Bumper " + hardwareIdentifier.ToString();
                        if (sensorConfig.ExternalRange)
                        {
                            bumperConfig.ThresholdLow = sensorConfig.LowThresh;
                            bumperConfig.ThresholdHigh = sensorConfig.HighThresh;
                        }
                        else
                        {
                            bumperConfig.ThresholdLow = 0;
                            bumperConfig.ThresholdHigh = 15;
                        }
                    }
                    // Sound or Light with an external range for a trigger?
                    else if (sensorConfig.ExternalRange &&
                        (0 != (sensorConfig.Type &
                                (     nxt.SensorDefinition.SensorType.LightOn 
                                    | nxt.SensorDefinition.SensorType.LightOff 
                                    | nxt.SensorDefinition.SensorType.Sound))))
                    {
                        bumperConfig.Name = sensorConfig.Type.ToString() + " Sensor " + hardwareIdentifier.ToString();
                        bumperConfig.ThresholdLow = sensorConfig.LowThresh;
                        bumperConfig.ThresholdHigh = sensorConfig.HighThresh;
                    }

                    #region Merge with prior config
                    // Look to see if this sensor was already configured
                    if (existingBumperConfig)
                    {
                        LegoNxtBumperConfig prevBumperConfig = _bumperConfigState.BumperConfigList.Find(delegate(LegoNxtBumperConfig cfg) { return cfg.HardwareIdentifier == hardwareIdentifier; });
                        if (prevBumperConfig != null)
                        {
                            // yes, use the old name.
                            bumperConfig.Name = prevBumperConfig.Name;

                            // if the sensor type has not changed, use the old threshold values.
                            if (prevBumperConfig.SensorType == bumperConfig.SensorType)
                            {
                                bumperConfig.ThresholdLow = prevBumperConfig.ThresholdLow;
                                bumperConfig.ThresholdHigh = prevBumperConfig.ThresholdHigh;
                            }
                        }
                    }
                    #endregion

                    if (!string.IsNullOrEmpty(bumperConfig.Name))
                    {
                        configArray.BumperConfigList.Add(bumperConfig);
                        configArray.ContactSensorArrayState.Sensors.Add(new bumper.ContactSensor(hardwareIdentifier, bumperConfig.Name));
                    }
                    hardwareIdentifier++;
                }
            }

            resultPort.Post(configArray);
            yield break;
        }


        /// <summary>
        /// Subscribe to appropriate sensor type and port on NXT
        /// Hook up notifications
        /// and then post success or failure
        /// <param name="successPort"></param>
        /// </summary>
        private IEnumerator<ITask> SubscribeToNXT(Port<bool> successPort)
        {
            if (!_subscribed)
            {

                //create a custom subscription request
                nxt.CustomSubscribeRequestType request = new nxt.CustomSubscribeRequestType();

                //select only the sensor and port we want
                //NOTE: this name must match the NXT sensor name. (see NXT readme)
                request.Sensors = new Collection<nxt.SensorDefinition>();

                foreach (LegoNxtBumperConfig config in _bumperConfigState.BumperConfigList)
                {
                    nxt.SensorDefinition sensor = new nxt.SensorDefinition();
                    sensor.Type = config.SensorType;
                    sensor.Port = config.HardwareIdentifier;
                    request.Sensors.Add(sensor);
                }

                //Subscribe to the NXT and wait for a response
                yield return
                    Arbiter.Choice(_legoPort.SelectiveSubscribe(request, _notificationPort),
                        delegate(SubscribeResponseType Rsp)
                        {
                            //update our state with subscription status
                            _subscribed = true;
                        },
                        delegate(Fault fail)
                        {
                            LogError("Bumper subscription failed", fail);
                            successPort.Post(false);
                        }
                    );

                if (_subscribed)
                {
                    LogInfo("Bumper subscription success");

                    //Subscription was successful, start listening for sensor change notifications
                    Activate(
                        Arbiter.Receive<nxt.Configure>(true, _notificationPort, SensorNotificationHandler)
                    );

                }
            }

            successPort.Post(_subscribed);

            yield break;
        }

        #endregion

        #region Internal Port Handlers

        /// <summary>
        /// Exclusive Update Service Status
        /// </summary>
        /// <param name="config"></param>
        private void UpdateStatusHandler(UpdateStatus config)
        {
            _bumperConfigState.Status = config.Body;
            config.ResponsePort.Post(DefaultUpdateResponseType.Instance);
        }

        /// <summary>
        /// Find the hardware configuration for the specified hardware identifier.
        /// </summary>
        /// <param name="hardwareIdentifier"></param>
        /// <returns>The LegoNxtBumperConfig or null</returns>
        private LegoNxtBumperConfig GetBumperConfig(int hardwareIdentifier)
        {
            return _bumperConfigState.BumperConfigList.Find(
                delegate(LegoNxtBumperConfig cfg) 
                {
                    return cfg.HardwareIdentifier == hardwareIdentifier; 
                });
        }

        /// <summary>
        /// Handle sensor update message from NXT
        /// </summary>
        private void SensorNotificationHandler(nxt.Configure notify)
        {
            //update state
            foreach (bumper.ContactSensor sensor in _bumperConfigState.ContactSensorArrayState.Sensors)
            {
                bool newContactValue;
                LegoNxtBumperConfig bumperConfig = GetBumperConfig(sensor.HardwareIdentifier);
                if (bumperConfig == null)
                {
                    // Old logic works only for touch sensor
                    newContactValue = (notify.Body.SensorPort[sensor.HardwareIdentifier - 1] == 1) ? true : false;
                }
                else
                {
                    newContactValue = ((notify.Body.SensorPort[sensor.HardwareIdentifier - 1] >= bumperConfig.ThresholdLow)
                    && (notify.Body.SensorPort[sensor.HardwareIdentifier - 1] <= bumperConfig.ThresholdHigh));
                }
   
                bool changed = (sensor.Pressed != newContactValue);
                sensor.TimeStamp = DateTime.Now;
                sensor.Pressed = newContactValue;

                if (changed)
                {
                    //notify subscribers on any bumper pressed or unpressed
                    SendNotification<bumper.Update>(_subMgrPort, sensor);
                }
            }
        }



        #endregion

        #region Standard Operation Handlers

        /// <summary>
        /// Return the standard ContactSensorArray state
        /// </summary>
        /// <param name="get"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> GetHandler(bumper.Get get)
        {
            if (_bumperConfigState.Status == LegoNxtBumperStatus.Uninitialized )
                throw new InvalidOperationException("LEGO NXT Bumper service is not initialized");

            get.ResponsePort.Post(_bumperConfigState.ContactSensorArrayState);
            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> HttpGetHandler(HttpGet httpGet)
        {
            httpGet.ResponsePort.Post(new HttpResponseType(
                HttpStatusCode.OK,
                _bumperConfigState.ContactSensorArrayState,
                _transform)
            );
            yield break;
        }

        /// <summary>
        /// Update our state before shutting down.
        /// </summary>
        /// <param name="drop"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Teardown)]
        public IEnumerator<ITask> DropHandler(DsspDefaultDrop drop)
        {
            // Update our state, but only wait for one second.
            PortSet<DefaultUpdateResponseType, Fault> responsePort = UpdateServiceStatus(LegoNxtBumperStatus.ShuttingDown);
            yield return Arbiter.Choice(
                Arbiter.Receive<DefaultUpdateResponseType>(false, responsePort,
                    delegate(DefaultUpdateResponseType response) { }),
                Arbiter.Receive<Fault>(false, responsePort,
                    delegate(Fault fault) { }),
                Arbiter.Receive<DateTime>(false, TimeoutPort(1000),
                    delegate(DateTime timeout) { }));

            base.DefaultDropHandler(drop);

            yield break;
        }

        /// <summary>
        /// Return the entire LEGO bumper configuration
        /// </summary>
        /// <param name="query"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> QueryConfigHandler(QueryConfig query)
        {
            if (_bumperConfigState.Status == LegoNxtBumperStatus.Uninitialized)
                throw new InvalidOperationException("LEGO NXT Bumper service is not initialized");

            query.ResponsePort.Post(_bumperConfigState);
            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReplaceHandler(bumper.Replace replace)
        {
            if (_subscribed)
            {
                throw new InvalidOperationException("Already subscribed");
            }
            else if (ValidState(replace.Body))
            {
                _bumperConfigState.ContactSensorArrayState = replace.Body;
                SaveState(_bumperConfigState);

                Port<bool> subscribeDonePort = new Port<bool>();
                SpawnIterator<Port<bool>>(subscribeDonePort, SubscribeToNXT);
                yield return (Arbiter.Receive<bool>(false, subscribeDonePort,
                    delegate(bool success)
                    {
                        if (success)
                        {
                            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
                        }
                        else
                        {
                            replace.ResponsePort.Post(
                                Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                                DsspFaultCodes.OperationFailed,
                                "LegoNxtBumper service subscription to LEGO NXT Brick failed")
                            );
                        }
                    }));
            }
            else
            {
                throw new InvalidOperationException("Invalid State");
            }

            yield break;
        }


        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(bumper.Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    ProcessSubscribeResponse(subscribe.Body.Subscriber);
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }


        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReliableSubscribeHandler(bumper.ReliableSubscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    ProcessSubscribeResponse(subscribe.Body.Subscriber);
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }

        /// <summary>
        /// Handle the LEGO specific bumper configuration
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> UpdateConfigHandler(UpdateConfig update)
        {
            _bumperConfigState.BumperConfigList = update.Body;

            if (_bumperConfigState.BumperConfigList == null)
                _bumperConfigState.BumperConfigList = new List<LegoNxtBumperConfig>();

            update.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }

        #endregion

        #region Subroutines

        /// <summary>
        /// Process the subscription response
        /// </summary>
        /// <param name="subscriber"></param>
        private void ProcessSubscribeResponse(string subscriber)
        {
            if (_bumperConfigState.Status == LegoNxtBumperStatus.ConnectedToNXT)
                SendNotification<bumper.Replace>(_subMgrPort, subscriber, _bumperConfigState.ContactSensorArrayState);
            else if (_bumperConfigState.Status == LegoNxtBumperStatus.WaitingForNXT)
                LogWarning(LogGroups.Console, "Subscribed to LEGO NXT Bumper service before the LEGO NXT Brick is connected");
            else if (_bumperConfigState.Status == LegoNxtBumperStatus.NotYetConfigured)
                LogWarning(LogGroups.Console, "Subscribed to LEGO NXT Bumper service before the bumper service is initialized");
            else if (_bumperConfigState.Status == LegoNxtBumperStatus.ShuttingDown)
                LogError(LogGroups.Console, "Subscribed to LEGO NXT Bumper service while the service is shutting down");
            else if (_bumperConfigState.Status == LegoNxtBumperStatus.Uninitialized)
                LogInfo(LogGroups.Console, "Subscribed to LEGO NXT Bumper service during service startup");
        }

        /// <summary>
        /// Send the UpdateStatus message to update internal state.
        /// </summary>
        /// <param name="connectedToBrick"></param>
        /// <returns>Standard Update Response PortSet</returns>
        private PortSet<DefaultUpdateResponseType, Fault> UpdateServiceStatus(LegoNxtBumperStatus status)
        {
            UpdateStatus updateStatus = new UpdateStatus(status);
            _internalPort.Post(updateStatus);
            return updateStatus.ResponsePort;
        }

        /// <summary>
        /// Inspect the ContactSensorArrayState to make sure it
        /// exists and that each sensor is associated with a valid
        /// LEGO sensor port (1-4).
        /// </summary>
        /// <param name="state"></param>
        /// <returns></returns>
        private static bool ValidState(bumper.ContactSensorArrayState state)
        {
            if (state != null)
            {
                if (state.Sensors != null)
                {
                    foreach (bumper.ContactSensor sensor in state.Sensors)
                    {
                        if (sensor.HardwareIdentifier < 1 || sensor.HardwareIdentifier > 4)
                            return false;
                    }
                    return true;
                }
            }
            return false;
        }

        #endregion

    }


}
