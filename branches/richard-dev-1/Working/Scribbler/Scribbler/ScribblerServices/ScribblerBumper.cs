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
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Security.Permissions;
using xml = System.Xml;
using soap = W3C.Soap;

using submgr = Microsoft.Dss.Services.SubscriptionManager;

using brick = IPRE.ScribblerBase.Proxy;
using bumper = Microsoft.Robotics.Services.ContactSensor.Proxy;


namespace IPRE.ScribblerBumper
{

    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/scribblerbumper.html";
    }

    [DisplayName("Scribbler Bumper")]
    [Description("The Scribbler Bumper Service")]
    [Contract(Contract.Identifier)]
    [AlternateContract(bumper.Contract.Identifier)] //implementing the generic contract
    public class ScribblerBumper : DsspServiceBase
    {
        private bumper.ContactSensorArrayState _state;

        [ServicePort("ScribblerBumper", 
            AllowMultipleInstances = false)]
        private bumper.ContactSensorArrayOperations _mainPort = new bumper.ContactSensorArrayOperations();

        [Partner("ScribblerBase", 
            Contract = brick.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, 
            Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        [Partner("SubMgr", 
            Contract = submgr.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.CreateAlways, 
            Optional = false)]
        private submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private bool _subscribed = false;

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public ScribblerBumper(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {

        }

        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
            //configure default state
            if (_state == null)
            {
                _state = new bumper.ContactSensorArrayState();
                _state.Sensors = new List<bumper.ContactSensor>();

                //A note on these names:
                //the bumper names must contain 'front' for RoboticsTutorial3 to work
                //the bumpers should be labeled left and right for the Scribbler Wander service to work
                //also for the SensorNotificationHandler below
                //If multiple words, should be spaced properly so TTS can say it right
                bumper.ContactSensor leftBumper = new bumper.ContactSensor();
                leftBumper.Name = "Front Left"; 

                bumper.ContactSensor rightBumper = new bumper.ContactSensor();
                rightBumper.Name = "Front Right";

                bumper.ContactSensor stallSensor = new bumper.ContactSensor();
                stallSensor.Name = "Stall";

                _state.Sensors.Add(leftBumper);
                _state.Sensors.Add(rightBumper);
                _state.Sensors.Add(stallSensor);
            }

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

			// display HTTP service Uri
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
            request.Sensors.Add("Stall");

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
            //update state
            foreach (bumper.ContactSensor sensor in _state.Sensors)
            {
                bool newval = true;
                if (sensor.Name.ToUpper().Contains("LEFT"))
                {
                    newval = !notify.Body.IRLeft;                   //NOTE: inverting logic here
                }
                else if (sensor.Name.ToUpper().Contains("RIGHT"))
                {
                    newval = !notify.Body.IRRight;
                }
                else if (sensor.Name.ToUpper().Contains("STALL"))
                {
                    newval = notify.Body.Stall;
                }
                else
                    LogError("Bumper name missmatch");

                bool changed = (sensor.Pressed != newval);
                sensor.TimeStamp = DateTime.Now;
                sensor.Pressed = newval;

                if (changed)
                {
                    //notify subscribers on any bumper pressed or unpressed
                    _subMgrPort.Post(new submgr.Submit(sensor, DsspActions.UpdateRequest));
                }
            }
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

    }


}
