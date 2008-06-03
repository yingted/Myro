//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTLightSensor.cs $ $Revision$
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
using W3C.Soap;

using analog = Microsoft.Robotics.Services.AnalogSensor.Proxy;
using legoNXT = Microsoft.Robotics.Services.LegoNxt.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;

/* Notes on Lego NXT Light Sensor
 *   Identifier must be the port number on the NXT Brick. Range: 1 - 4.
 *   To select whether the LED should be on or off, you need to edit the 
 *     NXT configuration file.  (see NXT readme)
 */

namespace Microsoft.Robotics.Services.LegoNxt.LightSensor
{
    
    [Contract(Contract.Identifier)]
    [AlternateContract(Microsoft.Robotics.Services.AnalogSensor.Proxy.Contract.Identifier)]
    [DisplayName("NXT Generic Analog Sensor")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT light sensor.\n(Uses the Generic Analog Sensor contract.)")]
    public class LightSensorService : DsspServiceBase
    {
        [InitialStatePartner(Optional = true)]
        private analog.AnalogSensorState _state;

        [ServicePort("/LegoNxtLightSensor", AllowMultipleInstances=true)]
        private analog.AnalogSensorOperations _mainPort = new analog.AnalogSensorOperations();

        [Partner("LegoNxt", Contract = legoNXT.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private legoNXT.LegoNxtOperations _legoPort = new legoNXT.LegoNxtOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private bool _subscribed = false;

        public LightSensorService(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
			
        }

        protected override void Start()
        {
            //configure default state
            if (_state == null)
            {
                _state = new analog.AnalogSensorState();
                _state.HardwareIdentifier = 3;
                _state.RawMeasurementRange = 100;

                SaveState(_state);
            }
  
            // Listen for each of my handlers
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

            // display HTTP service Uri
            LogInfo(LogGroups.Console, "Service uri: ");

            // Subscribe to NXT for light sensor notifications
            if (ValidState(_state))
                SubscribeToNXT();
        }

        private static bool ValidState(analog.AnalogSensorState state)
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
            sensor.Type = legoNXT.SensorDefinition.SensorType.LightOn;
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
            _state.RawMeasurement = notify.Body.SensorPort[_state.HardwareIdentifier - 1];
            _state.NormalizedMeasurement = _state.RawMeasurement;

            //notify subscribers on any state change
            SendNotification<analog.Replace>(_subMgrPort, new analog.Replace(_state));
        }


        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> GetHandler(analog.Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }


        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReplaceHandler(analog.Replace replace)
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
        public IEnumerator<ITask> SubscribeHandler(analog.Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<analog.Replace>(_subMgrPort, subscribe.Body.Subscriber, new analog.Replace(_state));
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReliableSubscribeHandler(analog.ReliableSubscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<analog.Replace>(_subMgrPort, subscribe.Body.Subscriber, new analog.Replace(_state));
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }


    }

     
    public static class Contract
    {
        public const string Identifier = "http://schemas.microsoft.com/2006/06/legonxtlightsensor.html";
    }

}
