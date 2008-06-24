//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNXTBattery.cs $ $Revision$
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
using xml = System.Xml;
using W3C.Soap;

using battery = Microsoft.Robotics.Services.Battery.Proxy;
using legoNXT = Microsoft.Robotics.Services.LegoNxt.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;



/* Notes on Lego NXT Battery
 *   This service implements the Battery data contract 
 *   It polls for the battery level at a specified frequency
 */


namespace Microsoft.Robotics.Services.LegoNxt.Battery
{
    
    [Contract(Contract.Identifier)]
    [AlternateContract(battery.Contract.Identifier)]
    [DisplayName("NXT Generic Battery")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT battery sensor.\n(Uses the Generic Battery contract.)")]
    public class BatteryService : DsspServiceBase
    {
        [EmbeddedResource("Microsoft.Robotics.Services.LegoNxt.LegoNXTBattery.xslt")]
        string _transform = null; 
        
        [InitialStatePartner(Optional = true)]
        private LegoNxtBatteryState _state;

        [ServicePort("/LegoNxtBattery", AllowMultipleInstances = false)]
        private LegoNxtBatteryOperations _mainPort = new LegoNxtBatteryOperations();

        [AlternateServicePort("/Battery", AllowMultipleInstances = true, AlternateContract=battery.Contract.Identifier)]
        private battery.BatteryOperations _batteryPort = new battery.BatteryOperations();

        [Partner("LegoNxt", Contract = legoNXT.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private legoNXT.LegoNxtOperations _legoPort = new legoNXT.LegoNxtOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways, Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private Port<DateTime> _timerPort = new Port<DateTime>();

        public BatteryService(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
			
        }

        protected override void Start()
        {
            //configure default state
            if (_state == null)
            {
                _state = new LegoNxtBatteryState();
                _state.PollDelayTime = 10000; //10 seconds
                _state.MaxBatteryPower = 9.0;

                SaveState(_state);
            }

            // Listen on the main port for requests and call the appropriate handler.
            Interleave mainInterleave = ActivateDsspOperationHandlers();

            //listen on alternate service port for requests and call the appropriate handler.           
            mainInterleave.CombineWith(new Interleave(
                new TeardownReceiverGroup(
                    Arbiter.Receive<DsspDefaultDrop>(false, _batteryPort, DefaultDropHandler)
                ),
                new ExclusiveReceiverGroup(
                    Arbiter.ReceiveWithIterator<battery.Replace>(true, _batteryPort, ReplaceHandler),
                    Arbiter.ReceiveWithIterator<battery.Subscribe>(true, _batteryPort, SubscribeHandler)
                ),
                new ConcurrentReceiverGroup(
                    Arbiter.ReceiveWithIterator<battery.Get>(true, _batteryPort, GetHandler),
                    Arbiter.Receive<DsspDefaultLookup>(true, _batteryPort, DefaultLookupHandler)
                )
            ));

            // Publish the service to the local Node Directory
            DirectoryInsert();

            // display HTTP service Uri
            LogInfo(LogGroups.Console, "Service uri: ");

            //wait until the nxt is connected to start polling for the battery level

            // Create a temporary notification port
            legoNXT.LegoNxtOperations _notificationPort = new legoNXT.LegoNxtOperations();
            
            Activate(
                Arbiter.Choice(_legoPort.Subscribe(_notificationPort),
                    delegate(SubscribeResponseType Rsp)
                    {
                        LogInfo("Lego NXT battery initial subscription success.  Waiting for NXT to connect.");

                        //Subscription was successful, start listening for sensor change notifications
                        Activate(
                            Arbiter.Receive<legoNXT.Configure>(false, _notificationPort,
                            delegate(legoNXT.Configure msg)
                            {
                                //start polling
                                LogInfo("NXT Battery service notified of NXT connection");
                                _timerPort.Post(DateTime.Now);
                                Activate(Arbiter.Receive(true, _timerPort, TimerHandler));
                            })
                        );
                    },
                    delegate(Fault F)
                    {
                        LogError("Lego NXT battery initial subscription failed");
                    }
                )
            );

        }



        void TimerHandler(DateTime signal)
        {
            legoNXT.LegoGetBatteryLevel getBatt = new legoNXT.LegoGetBatteryLevel();
            getBatt.RequireResponse = true;
            Activate(Arbiter.Choice(_legoPort.SendLegoCommand(getBatt),
                delegate(legoNXT.LegoResponse status)
                {
                    legoNXT.LegoResponseGetBatteryLevel legoResponseGetBatteryLevel = status as legoNXT.LegoResponseGetBatteryLevel;
                    if (legoResponseGetBatteryLevel != null && legoResponseGetBatteryLevel.ErrorCode == legoNXT.LegoErrorCode.Success)
                    {
                        double voltage = legoResponseGetBatteryLevel.Voltage;
                        _state.PercentBatteryPower = voltage / 1000.0 / _state.MaxBatteryPower;
                    }
                },
                delegate(Fault failure)
                {
                    LogError(failure);
                }
            ));

            //re-call ourselves
            Activate(
                Arbiter.Receive(false, TimeoutPort(_state.PollDelayTime),
                    delegate(DateTime time)
                    {
                        _timerPort.Post(time);
                    }
                )
            );
        }


        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> MyGetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }
        
        public IEnumerator<ITask> GetHandler(battery.Get get)
        {
            get.ResponsePort.Post((battery.BatteryState)_state);
            yield break;
        }

        /// <summary>
        /// HttpGet Handler
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


        public IEnumerator<ITask> ReplaceHandler(battery.Replace replace)
        {
            throw new InvalidOperationException("The LEGO NXT Battery sensor data can not be updated from outside the service");
        }

        public IEnumerator<ITask> SubscribeHandler(battery.Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<battery.Replace>(_subMgrPort, subscribe.Body.Subscriber, new battery.Replace(_state));
                },
                delegate(Exception fault)
                {
                    LogError(fault);
                }
            );
        }


    }  

}
