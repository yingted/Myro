//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTBackupBeep.cs $ $Revision$
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
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Security.Permissions;

using xml = System.Xml;
using legoNXT = Microsoft.Robotics.Services.LegoNxt.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using W3C.Soap;


/* Notes on Lego NXT BackupBeep Sensor
 *   Motor port numbers must be in the range: 1 - 4.
 *   Frequency must be 200 - 14000
 *   Pause and Play Durations must be positive
 */

namespace Microsoft.Robotics.Services.LegoNxt.BackupBeep
{
    
    [Contract(Contract.Identifier)]
    [DisplayName("NXT Backup Beep")]
    [Description("Monitors the LEGO® MINDSTORMS® NXT motors and requests the NXT brick to sound a backup beeping sound when the motors are in reverse.")]
    public class BackupBeepService : DsspServiceBase
    {
        [EmbeddedResource("Microsoft.Robotics.Services.LegoNxt.LegoNXTBackupBeep.xslt")]
        string _transform = null; 
        
        [InitialStatePartner(Optional = true)]
        private LegoNxtBackupBeepState _state;

        [ServicePort("/LegoNxtBackupBeep", AllowMultipleInstances=true)]
        private LegoNxtBackupBeepOperations _mainPort = new LegoNxtBackupBeepOperations();

        [Partner("LegoNxt", Contract = legoNXT.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private legoNXT.LegoNxtOperations _legoPort = new legoNXT.LegoNxtOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private bool _subscribed = false;

        private bool beeping;

        public BackupBeepService(DsspServiceCreationPort creationPort)
            : 
                base(creationPort)
        {
			
        }
        protected override void Start()
        {
            //configure default state
            if (_state == null)
            {
                _state = new LegoNxtBackupBeepState();
                _state.Motor1Port = 1;
                _state.Motor2Port = 3;

                _state.PauseDuration = 800;
                _state.PlayDuration = 800;
                _state.Frequency = 1500;

                SaveState(_state);
            }

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");

            // Subscribe to NXT for sound sensor notifications
            if (ValidState(_state))
                SubscribeToNXT();

        }


        private static bool ValidState(LegoNxtBackupBeepState state)
        {
            if (state != null &&
                state.Motor1Port >= 1 && state.Motor1Port <= 3 &&
                state.Motor2Port >= 1 && state.Motor2Port <= 3 &&
                state.Frequency >= 200 && state.Frequency <= 14000 &&
                state.PauseDuration > 0 &&
                state.PlayDuration > 0)
            {
                return true;
            }
            return false;
        }


        /// <summary>
        /// Subscribe to appropriate sensor type and port on NXT
        /// </summary>
        private void SubscribeToNXT()
        {
            // Create a notification port
            legoNXT.LegoNxtOperations _notificationPort = new legoNXT.LegoNxtOperations();

            //create a custom subscription request
            legoNXT.CustomSubscribeRequestType request = new legoNXT.CustomSubscribeRequestType();

            //select only the sensor and port we want
            //NOTE: this name must match the NXT sensor name. (see NXT readme)
            request.Sensors = new Collection<legoNXT.SensorDefinition>();

            legoNXT.SensorDefinition sensor1 = new legoNXT.SensorDefinition();
            sensor1.Type = legoNXT.SensorDefinition.SensorType.Motor;
            sensor1.Port = _state.Motor1Port;
            request.Sensors.Add(sensor1);

            legoNXT.SensorDefinition sensor2 = new legoNXT.SensorDefinition();
            sensor2.Type = legoNXT.SensorDefinition.SensorType.Motor;
            sensor2.Port = _state.Motor2Port;
            request.Sensors.Add(sensor2);

            //Subscribe to the NXT and wait for a response
            Activate(
                Arbiter.Choice(_legoPort.SelectiveSubscribe(request, _notificationPort),
                    delegate(SubscribeResponseType Rsp)
                    {
                        //update our state with subscription status
                        _subscribed = true;

                        LogInfo("lego backup subscription success");

                        //Subscription was successful, start listening for sensor change notifications
                        Activate(
                            Arbiter.Receive<legoNXT.Configure>(true, _notificationPort, SensorNotificationHandler)
                        );
                    },
                    delegate(Fault F)
                    {
                        LogError(sensor1.Type + sensor1.Port + "subscription failed");
                    }
                )
            );
        }


        /// <summary>
        /// Handle sensor update message from NXT
        /// </summary>
        public void SensorNotificationHandler(legoNXT.Configure notify)
        {
            if (notify == null)
                throw new ArgumentNullException("notify");

            if (beeping)
                return;

            if (notify.Body.MotorOutputPort[_state.Motor1Port - 1] < 0 &&
                notify.Body.MotorOutputPort[_state.Motor2Port - 1] < 0)
            {
                SpawnIterator(ToneHandler);
            }
        }

        IEnumerator<ITask> ToneHandler()
        {
            beeping = true;

            legoNXT.LegoPlayTone tone = new legoNXT.LegoPlayTone();
            tone.Frequency = _state.Frequency;
            tone.Duration = _state.PlayDuration;
            tone.RequireResponse = false;
            _legoPort.SendLegoCommand(tone);

            //wait play time + pause time
            yield return Arbiter.Receive(false, TimeoutPort(_state.PlayDuration+_state.PauseDuration), 
                delegate(DateTime t) { });

            beeping = false;

            //_legoPort.Get(new GetRequestType());
            yield return Arbiter.Receive<legoNXT.LegoNxtState>(false, _legoPort.Get(new GetRequestType()),
                delegate(legoNXT.LegoNxtState legoState)
                {
                    SensorNotificationHandler(new legoNXT.Configure(legoState));
                }
            );

        }


        /// <summary>
        /// Get Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        /// <summary>
        /// Get Handler
        /// </summary>
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


        /// <summary>
        /// Replace Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> ReplaceHandler(Replace replace)
        {
            if (_subscribed)
                throw new Exception("Already subscribed");
            else if (ValidState(replace.Body))
            {
                _state = replace.Body;
                SaveState(_state);
                replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
                SubscribeToNXT();
            }
            else
                throw new Exception("Invalid State");

            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<Replace>(_subMgrPort, subscribe.Body.Subscriber, new Replace(_state));
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReliableSubscribeHandler(ReliableSubscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<Replace>(_subMgrPort, subscribe.Body.Subscriber, new Replace(_state));
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }

    }
}
