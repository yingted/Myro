//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTSound.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Security.Permissions;
using xml = System.Xml;
using legoNXT = Microsoft.Robotics.Services.LegoNxt.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using W3C.Soap;
using analog = Microsoft.Robotics.Services.AnalogSensor.Proxy;


/* Notes on Lego NXT Sound Sensor
 *   Identifier must be the port number on the NXT Brick. Range: 1 - 4.
 */

namespace Microsoft.Robotics.Services.LegoNxt.Sound
{
    
    [Contract(Contract.Identifier)]
    [DisplayName("NXT Sound Sensor")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT sound sensor.")]
    [AlternateContract(analog.Contract.Identifier)]
    public class SoundService : DsspServiceBase
    {
        [InitialStatePartner(Optional = true)]
        private SoundState _state;

        private analog.AnalogSensorState _alternateState;

        [ServicePort("/LegoNxtSound", AllowMultipleInstances=true)]
        private LegoNxtSoundOperations _mainPort = new LegoNxtSoundOperations();

        [AlternateServicePort("/analog", AllowMultipleInstances = false, AlternateContract = analog.Contract.Identifier)]
        private analog.AnalogSensorOperations _alternatePort = new analog.AnalogSensorOperations();

        [Partner("LegoNxt", Contract = legoNXT.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private legoNXT.LegoNxtOperations _legoPort = new legoNXT.LegoNxtOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private bool _subscribed = false;

        public SoundService(DsspServiceCreationPort creationPort)
            : 
                base(creationPort)
        {
			
        }
        protected override void Start()
        {
            // Alternate Port Operations handlers
            Activate(Arbiter.ReceiveWithIterator<analog.Get>(true, _alternatePort, AlternateGetHandler));

            //configure default state
            if (_state == null)
            {
                _state = new SoundState();
                _state.HardwareIdentifier = 2;

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


        private static bool ValidState(SoundState state)
        {
            if (state != null)
            {
                if (state.HardwareIdentifier >= 1 && state.HardwareIdentifier <= 4)
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
            legoNXT.SensorDefinition sensor = new legoNXT.SensorDefinition();
            sensor.Type = legoNXT.SensorDefinition.SensorType.Sound;
            sensor.Port = _state.HardwareIdentifier;
            request.Sensors.Add(sensor);

            //Subscribe to the NXT and wait for a response
            Activate(
                Arbiter.Choice(_legoPort.SelectiveSubscribe(request, _notificationPort),
                    delegate(SubscribeResponseType Rsp)
                    {
                        //update our state with subscription status
                        _subscribed = true;

                        LogInfo(sensor.Type + sensor.Port + " subscription success");

                        //Subscription was successful, start listening for sensor change notifications
                        Activate(
                            Arbiter.Receive<legoNXT.Configure>(true, _notificationPort, SensorNotificationHandler)
                        );
                    },
                    delegate(Fault F)
                    {
                        LogError(sensor.Type + sensor.Port + "subscription failed");
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

            //update time
            _state.TimeStamp = DateTime.Now;

            //Update reading
            _state.SoundReading = notify.Body.SensorPort[_state.HardwareIdentifier - 1];

            //notify subscribers on any state change
            SendNotification<Replace>(_subMgrPort, new Replace(_state));
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
        /// Alternate Get Handler
        /// </summary>
        /// <param name="get"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> AlternateGetHandler(analog.Get get)
        {
            _alternateState = new analog.AnalogSensorState();
            _alternateState.RawMeasurement = _state.SoundReading;
            _alternateState.TimeStamp = _state.TimeStamp;
            get.ResponsePort.Post(_alternateState);
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
                throw new Exception("State not valid");

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
