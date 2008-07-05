//-----------------------------------------------------------------------
//  IPRE Wander Service
//  Drives around trying not to bump into anything
//  Also speaks
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.Security.Permissions;
using System.ComponentModel;

using contactsensor = Microsoft.Robotics.Services.ContactSensor.Proxy;
using drive = Microsoft.Robotics.Services.Drive.Proxy;
using motor = Microsoft.Robotics.Services.Motor.Proxy;

using speech = Microsoft.Robotics.Technologies.Speech.TextToSpeech.Proxy;


namespace IPRE.Wander
{
    [DisplayName("Wander")]
    [Description("Scribbler Wander Service")]
    [Contract(Contract.Identifier)]
    public class WanderService : DsspServiceBase
    {
        private WanderState _state = new WanderState();

        [Partner("Bumper", 
            Contract = contactsensor.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExisting)]
        contactsensor.ContactSensorArrayOperations _contactSensorPort = new contactsensor.ContactSensorArrayOperations();

        [Partner("Drive",
            Contract = drive.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExisting)]
        drive.DriveOperations _drivePort = new drive.DriveOperations();

        // target port for bumper notifications
        contactsensor.ContactSensorArrayOperations _contactNotificationPort = new contactsensor.ContactSensorArrayOperations();

        [ServicePort("/Wander", AllowMultipleInstances=false)]
        private WanderOperations _mainPort = new WanderOperations();

        [Partner("voice", Contract = speech.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        private speech.SpeechTextOperations _speechPort = new speech.SpeechTextOperations();

        public WanderService(DsspServiceCreationPort creationPort) : base(creationPort)
        {

        }

        protected override void Start()
        {

            // Listen on the main port for requests and call the appropriate handler.
            Interleave mainInterleave = ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");

            SubscribeToBumperSensors();

        }

        void SubscribeToBumperSensors()
        {
            _contactSensorPort.Subscribe(_contactNotificationPort);

            // attach handler to update notification on bumpers
            Activate(Arbiter.Receive<contactsensor.Update>(true, _contactNotificationPort, BumperNotificationHandler));
        }

        void BumperNotificationHandler(contactsensor.Update updateNotification)
        {
            contactsensor.ContactSensor s = updateNotification.Body;
            if (!s.Pressed)
                return;

            if (!string.IsNullOrEmpty(s.Name) && s.Name.ToLowerInvariant().Contains("left"))
            {
                //say something
                speech.SayTextRequest saythis = new speech.SayTextRequest();
                saythis.SpeechText = "Turning right";
                _speechPort.SayText(saythis);

                //update state
                _state.CurrentAction = "Turning right";

                //move
                SpawnIterator<int>(-800, BackUpTurnAndMove);

            }
            else if (!string.IsNullOrEmpty(s.Name) && s.Name.ToLowerInvariant().Contains("right"))
            {
                //say something
                speech.SayTextRequest saythis = new speech.SayTextRequest();
                saythis.SpeechText = "Turning left";
                _speechPort.SayText(saythis);

                //update state
                _state.CurrentAction = "Turning left";

                //move
                SpawnIterator<int>(800, BackUpTurnAndMove);
            }
            else if (!string.IsNullOrEmpty(s.Name) && s.Name.ToLowerInvariant().Contains("stall"))
            {
                //say something
                speech.SayTextRequest saythis = new speech.SayTextRequest();
                saythis.SpeechText = "Ouch, let go.";
                _speechPort.SayText(saythis);

                //update state
                _state.CurrentAction = "Turning around";

                //move
                SpawnIterator<int>(800, BackUpTurnAndMove);
            }
        }

        /// <summary>
        /// Implements a simple sequential state machine that makes the robot wander
        /// </summary>
        /// <param name="turnAmount">Amount of time to turn left.  If value is negative, the robot will turn right</param>
        IEnumerator<ITask> BackUpTurnAndMove(int turnAmount)
        {
            // start moving backwards
            _drivePort.SetDrivePower(new drive.SetDrivePowerRequest(-0.9, -0.9));

            //wait while reversing
            yield return Arbiter.Receive(false, TimeoutPort(800), delegate(DateTime t) { });

            // start turning
            if (turnAmount > 0)
                _drivePort.SetDrivePower(new drive.SetDrivePowerRequest(-0.7, 0.7));
            else
                _drivePort.SetDrivePower(new drive.SetDrivePowerRequest(0.7, -0.7));

            //wait turn time
            yield return Arbiter.Receive(false, TimeoutPort(Math.Abs(turnAmount)), delegate(DateTime t) { });

            // continue forwards
            _drivePort.SetDrivePower(new drive.SetDrivePowerRequest(0.9, 0.9));

            //update state
            _state.CurrentAction = "Driving forward";

            // done
            yield break;
        }

        

    }
}
