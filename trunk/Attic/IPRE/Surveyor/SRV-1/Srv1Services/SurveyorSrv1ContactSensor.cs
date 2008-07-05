//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: SurveyorSrv1ContactSensor.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
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

using bumper = Microsoft.Robotics.Services.ContactSensor.Proxy;
using srv1bumper = SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogSensor.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using contactsensorbase = Microsoft.Robotics.Services.ContactSensor.Proxy;
using W3C.Soap;



namespace SharpLogic.Robotics.Services.Surveyor.Srv1.ContactSensor
{
    
    [Contract(Contract.Identifier)]
    [AlternateContract(bumper.Contract.Identifier)]
    [DisplayName("SRV-1 Contact Sensor")]
    [Description("Provides access to the Surveyor SRV-1 IR as touch sensor.\n(Uses Generic Contact Sensor Array contract.)")]
    public class ContactSensorService : DsspServiceBase
    {
        /// <summary>
        /// Default configuration file for the Surveyor SRV-1 Encoder service
        /// </summary>
        public const string DefaultConfigFile = "DefaultSurveyorSrv1Encoder.xml";

        [InitialStatePartner(Optional = true, ServiceUri = DefaultConfigFile)]
        private bumper.ContactSensorArrayState _state = new bumper.ContactSensorArrayState();

        [ServicePort("/SurveyorSrv1ContactSensor", AllowMultipleInstances=false)]
        private bumper.ContactSensorArrayOperations _mainPort = new bumper.ContactSensorArrayOperations();
        
        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        // SRV-1 Sensor service 
        [Partner("Srv1AnalogSensor", Contract = srv1bumper.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        srv1bumper.Srv1AnalogSensorOperations _bumperPort = new srv1bumper.Srv1AnalogSensorOperations(); 

        public ContactSensorService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {			
        }

        protected override void Start()
        {
            //configure initial state
            if (_state == null)
            {
                _state = new bumper.ContactSensorArrayState();
                _state.Sensors = new List<contactsensorbase.ContactSensor>();

                contactsensorbase.ContactSensor defaultBumper = new contactsensorbase.ContactSensor();
                defaultBumper.HardwareIdentifier = 1;
                defaultBumper.Name = "Default Bumper 1";

                _state.Sensors.Add(defaultBumper);

                SaveState(_state);
            }

            Port<bool> successPort = new Port<bool>();
            SpawnIterator(successPort, ConfigureHardware);
            Activate(Arbiter.Receive(false, successPort,
                delegate(bool success)
                {
                    if (!success)
                    {
                        LogError("Surveyor SRV-1 Service failed to start.  Shutting down.");
                        Shutdown();
                    }
                }
            ));

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

            // display HTTP service Uri
            LogInfo(LogGroups.Console, "Service uri: ");
        }

        /// <summary>
        /// Get Handler
        /// </summary>
        /// <param name="get"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(bumper.Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        private IEnumerator<ITask> ConfigureHardware(Port<bool> successPort)
        {
            bool success = false;

            // Walk through bumper ports and send config to Surveyor SRV-1
            srv1bumper.InitRequest req = new srv1bumper.InitRequest();
            req.s1 = srv1bumper.Srv1SensorType.NoChange;
            req.s2 = srv1bumper.Srv1SensorType.NoChange;
            req.s3 = srv1bumper.Srv1SensorType.NoChange;
            req.s4 = srv1bumper.Srv1SensorType.NoChange;

            foreach (bumper.ContactSensor sensor in _state.Sensors)
            {
                switch (sensor.HardwareIdentifier)
                {
                    case 1:
                        req.s1 = srv1bumper.Srv1SensorType.Touch;
                        break;
                    case 2:
                        req.s2 = srv1bumper.Srv1SensorType.Touch;
                        break;
                    case 3:
                        req.s3 = srv1bumper.Srv1SensorType.Touch;
                        break;
                    case 4:
                        req.s4 = srv1bumper.Srv1SensorType.Touch;
                        break;
                }
            }

            srv1bumper.SensorInit sensorInit = new srv1bumper.SensorInit(req);
            _bumperPort.Post(sensorInit);
            yield return (Arbiter.Choice(sensorInit.ResponsePort,
                delegate(DefaultSubmitResponseType r) 
                { 
                    success = true;
                },
                delegate(Fault f) 
                { 
                    LogError("Subscription failed"); 
                }));

            if (!success)
            {
                successPort.Post(false);
                yield break;
            }

            // Now subscribe to appropriate ports on the hardware
            // Create a notification port
            srv1bumper.Srv1AnalogSensorOperations srv1NotificationPort = new srv1bumper.Srv1AnalogSensorOperations();

            // Subscribe to the srv1 and wait for a response
            yield return (
                Arbiter.Choice(_bumperPort.Subscribe(new SubscribeRequestType(), srv1NotificationPort),
                    delegate(SubscribeResponseType Rsp)
                    {
                    },
                    delegate(Fault F)
                    {
                        success = false;
                        LogError("Subscription failed");
                    }
                )
            );

            if (!success)
            {
                successPort.Post(false);
                yield break;
            }


            // Subscription was successful, start listening for sensor change notifications
            Activate(
                Arbiter.Receive<srv1bumper.Replace>(true, srv1NotificationPort, SensorNotificationHandler)
            );

            SaveState(_state);

            successPort.Post(success);
            yield break;
        }

        /// <summary>
        /// Replace Handler
        /// </summary>
        /// <param name="replace"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> ReplaceHandler(bumper.Replace replace)
        {
            _state = replace.Body;
            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            yield break;
        }

        /// <summary>
        /// Update Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> UpdateHandler(bumper.Update update)
        {
            bumper.ContactSensor sensor = _state.Sensors.Find(
                delegate(bumper.ContactSensor s) { return update.Body.HardwareIdentifier == s.HardwareIdentifier; });
            if (sensor != null)
            {
                sensor.Name = update.Body.Name;
                sensor.Pressed = update.Body.Pressed;
                sensor.TimeStamp = update.Body.TimeStamp;
                update.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            }
            throw new InvalidOperationException("Contact Sensor " + update.Body.HardwareIdentifier + " : " + update.Body.Name + " does not exist.");
        }
        
        /// <summary>
        /// Subscribe Handler
        /// </summary>
        /// <param name="subscribe"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(bumper.Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    _subMgrPort.Post(new submgr.Submit(
                        subscribe.Body.Subscriber, DsspActions.ReplaceRequest, _state, null));
                },
                null
            );
        }

        /// <summary>
        /// ReliableSubscribe Handler
        /// </summary>
        /// <param name="subscribe"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReliableSubscribeHandler(bumper.ReliableSubscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    _subMgrPort.Post(new submgr.Submit(
                        subscribe.Body.Subscriber, DsspActions.ReplaceRequest, _state, null));
                },
                null
            );
        }

        /// <summary>
        /// receive sensor data from the Srv1
        /// </summary>
        /// <param name="replace"></param>
        private void SensorNotificationHandler(srv1bumper.Replace replace)
        {
            const int sensorThreshold = 100;

            for (int ix = 1; ix <= replace.Body.Sensors.Length; ix++ )
            {
                bumper.ContactSensor bump = _state.Sensors.Find(delegate(bumper.ContactSensor s) { return s.HardwareIdentifier == ix; });
                if (bump == null)
                    continue;

                srv1bumper.Sensor sensor = replace.Body.Sensors[ix-1];
                bool sensorPressed = (sensor.value > sensorThreshold);
                bool changed = (sensorPressed != bump.Pressed);

                if (changed)
                {
                    bump.Pressed = sensorPressed;

                    //notify subscribers on any bumper pressed or unpressed
                    _subMgrPort.Post(new submgr.Submit(bump, DsspActions.UpdateRequest));
                }
            }
        }
    }

    public static class Contract
    {
        public const string Identifier = "http://schemas.sharplogic.com/robotics/2006/11/surveyorsrv1contactsensor.html";
    }



}
