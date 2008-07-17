// Copyright (c) Microsoft Corporation.  All rights reserved.

//------------------------------------------------------------------------------
// Scribbler Bumper Service
//
//  Implements the generic ContactSensorArray service
//  Provides standard interface for the scribbler's IR obstacle sensors and stall sensor      
//
//------------------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using Microsoft.Dss.Core.DsspHttp;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Security.Permissions;
using System.Threading;
using xml = System.Xml;
using soap = W3C.Soap;
using Myro.Services;
using System.Linq;

using brick = Myro.Services.Scribbler.ScribblerBase.Proxy;
using vector = Myro.Services.Generic.Vector;

namespace Myro.Services.Scribbler.Bumper
{

    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/schemas/2008/06/scribblerirbumper.html";
    }

    [DisplayName("Scribbler Bumper")]
    [Description("The Scribbler Bumper Service")]
    [Contract(Contract.Identifier)]
    [AlternateContract(vector.Contract.Identifier)] //implementing the generic contract
    public class BumperService : vector.VectorServiceBase
    {
        [ServicePort(AllowMultipleInstances = false)]
        vector.VectorOperations _operationsPort = new vector.VectorOperations();
        protected override vector.VectorOperations OperationsPort { get { return _operationsPort; } }

        [Partner("ScribblerBase",
            Contract = brick.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate,
            Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        private bool _subscribed = false;

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public BumperService(DsspServiceCreationPort creationPort) :
            base(creationPort)
        {
            _state = new vector.VectorState(
                new List<double> { 0.0, 0.0 },
                new List<string> { "left", "right" },
                DateTime.Now);
        }

        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
            base.Start();
            LogInfo(LogGroups.Console, "Service uri: ");

            SubscribeToScribblerBase();
        }

        /// <summary>
        /// Subscribe to appropriate sensors on Scribbler base
        /// </summary>
        private void SubscribeToScribblerBase()
        {
            // Create a notification port
            brick.ScribblerOperations _notificationPort = new brick.ScribblerOperations();

            //create a custom subscription request
            brick.MySubscribeRequestType request = new brick.MySubscribeRequestType();

            //select only the sensor and port we want
            //NOTE: this name must match the scribbler sensor name.
            request.Sensors = new List<string>();

            request.Sensors.Add("IRLeft");
            request.Sensors.Add("IRRight");
            //request.Sensors.Add("Stall");

            //Subscribe to the ScribblerBase and wait for a response
            Activate(
                Arbiter.Choice(_scribblerPort.SelectiveSubscribe(request, _notificationPort),
                    delegate(SubscribeResponseType Rsp)
                    {
                        //update our state with subscription status
                        _subscribed = true;

                        LogInfo("ScribblerBumper subscription success");

                        //Subscription was successful, start listening for sensor change notifications
                        Activate(
                            Arbiter.Receive<brick.Replace>(true, _notificationPort, SensorNotificationHandler)
                        );
                    },
                    delegate(soap.Fault F)
                    {
                        LogError("ScribblerBumper subscription failed");
                    }
                )
            );
        }

        /// <summary>
        /// Handle sensor update message from Scribbler
        /// </summary>
        public void SensorNotificationHandler(brick.Replace notify)
        {
            bool[] values = { notify.Body.IRLeft, notify.Body.IRRight };
            OperationsPort.Post(new vector.SetAllElements(new List<double>(
                from v in values
                select (!v ? 1.0 : 0.0)))); // NOTE: v is being inverted here
        }
    }


}
