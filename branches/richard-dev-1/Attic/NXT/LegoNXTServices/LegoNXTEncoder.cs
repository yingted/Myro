//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTEncoder.cs $ $Revision$
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
using W3C.Soap;

using encoder = Microsoft.Robotics.Services.Encoder.Proxy;
using legoNXT = Microsoft.Robotics.Services.LegoNxt.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;

/* Notes on Lego NXT Encoder
 *   Identifier must be the motor port number on the NXT Brick. Range: 1 - 3.
 *   TicksPerRevolution is usually 6.
 */

namespace Microsoft.Robotics.Services.LegoNxt.Encoder
{
    
    [Contract(Contract.Identifier)]
    [AlternateContract(Microsoft.Robotics.Services.Encoder.Proxy.Contract.Identifier)]
    [DisplayName("NXT Generic Encoder")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT motor encoder.\n(Uses the Generic Encoder contract.)")]
    public class EncoderService : DsspServiceBase
    {
        [EmbeddedResource("Microsoft.Robotics.Services.LegoNxt.LegoNXTEncoder.xslt")]
        string _transform = null; 

        [InitialStatePartner(Optional = true)]
        private encoder.EncoderState _state;

        [ServicePort("/LegoNxtEncoder", AllowMultipleInstances=true)]
        private encoder.EncoderOperations _mainPort = new encoder.EncoderOperations();

        [Partner("LegoNxt", Contract = legoNXT.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private legoNXT.LegoNxtOperations _legoPort = new legoNXT.LegoNxtOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private bool _subscribed = false;

        Interleave _mainInterleave;

        public EncoderService(DsspServiceCreationPort creationPort)
            : 
                base(creationPort)
        {
			
        }

        protected override void Start()
        {
            //configure default state
            if (_state == null)
            {
                _state = new encoder.EncoderState();
                _state.HardwareIdentifier = -1;
                _state.TicksPerRevolution = 6;

                SaveState(_state);
            }
  
            // Listen for each of my handlers
            _mainInterleave = ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

            // display HTTP service Uri
            LogInfo(LogGroups.Console, "Service uri: ");

            // Subscribe to NXT for encoder notifications
            if (ValidState(_state))
                SubscribeToNXT();
        }

        private static bool ValidState(encoder.EncoderState state)
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
            sensor.Type = legoNXT.SensorDefinition.SensorType.Encoder;
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
                        _mainInterleave.CombineWith(new Interleave(
                            new ExclusiveReceiverGroup(
                                Arbiter.Receive<legoNXT.Configure>(true, _notificationPort, SensorNotificationHandler)
                            ),
                            new ConcurrentReceiverGroup()
                        ));
                            
                        //Activate(
                            
                        //);
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

            int newReading = notify.Body.MotorSensorPort[_state.HardwareIdentifier - 1];
            
            //increment counter (and account for any missing ticks
            if (Math.Abs(newReading - _state.CurrentReading) <= 3)
                _state.TicksSinceReset += Math.Abs(newReading - _state.CurrentReading);
            else
                _state.TicksSinceReset++;

            //Update angle
            _state.CurrentReading = newReading;

            //notify subscribers on any state change
            encoder.UpdateTickCount updateHeader = new encoder.UpdateTickCount(new encoder.UpdateTickCountRequest(DateTime.Now, _state.TicksSinceReset));
            SendNotification<encoder.UpdateTickCount>(_subMgrPort, updateHeader);
        }


        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> GetHandler(encoder.Get get)
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
        public IEnumerator<ITask> ResetHandler(encoder.Reset reset)
        {
            _state.TicksSinceReset = 0;
            reset.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }


        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReplaceHandler(encoder.Replace replace)
        {
            if (_subscribed)
                throw new InvalidOperationException("Already subscribed");
            else if (ValidState(replace.Body))
            {
                _state = replace.Body;
                SaveState(_state);
                replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
                SubscribeToNXT();
            }
            else
                throw new InvalidOperationException("Invalid State for replacement");

            yield break;
        }


        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(encoder.Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<encoder.Replace>(_subMgrPort, subscribe.Body.Subscriber, new encoder.Replace(_state));
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReliableSubscribeHandler(encoder.ReliableSubscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<encoder.Replace>(_subMgrPort, subscribe.Body.Subscriber, new encoder.Replace(_state));
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
        public const string Identifier = "http://schemas.microsoft.com/2006/06/legonxtencoder.html";
    }

}
