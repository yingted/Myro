// Copyright (c) Microsoft Corporation.  All rights reserved.

//------------------------------------------------------------------------------
// Scribbler Drive Service
//
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
using brick = Myro.Services.Scribbler.ScribblerBase.Proxy;
using drive = Microsoft.Robotics.Services.Drive;
using W3C.Soap;

namespace Myro.Services.Scribbler.Drive
{
    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/schemas/2008/06/scribblerdrive.html";
    }

    [DisplayName("Scribbler Drive")]
    [Description("The Scribbler Drive Service")]
    [Contract(Contract.Identifier)]
    [AlternateContract(drive.Contract.Identifier)] //implementing the generic motor contract

    public class ScribblerDrive : DsspServiceBase
    {
        private drive.DriveDifferentialTwoWheelState _state = null;

        [ServicePort("ScribblerDrive", AllowMultipleInstances = false)]
        private drive.DriveOperations _mainPort = new drive.DriveOperations();

        [Partner("ScribblerBase", Contract = brick.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public ScribblerDrive(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {

        }

        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
            if (_state == null)
            {
                //add default state.  
                //NOTE: because this is a custom implementation of the drive service, we can hard code values
                //also, because there are no encoders, we do not need any of the wheel measurements.

                _state = new drive.DriveDifferentialTwoWheelState();
                _state.IsEnabled = true;

                _state.LeftWheel = new Microsoft.Robotics.Services.Motor.WheeledMotorState();
                _state.LeftWheel.Name = "Left Wheel";
                _state.LeftWheel.MotorState = new Microsoft.Robotics.Services.Motor.MotorState();
                _state.LeftWheel.MotorState.Name = "Left Motor";

                _state.RightWheel = new Microsoft.Robotics.Services.Motor.WheeledMotorState();
                _state.RightWheel.Name = "Right Wheel";
                _state.RightWheel.MotorState = new Microsoft.Robotics.Services.Motor.MotorState();
                _state.RightWheel.MotorState.Name = "Right Motor";
            }

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");
        }

        #region Standard Port Operations

        /// <summary>
        /// Get Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(drive.Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        /// <summary>
        /// Update Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> UpdateHandler(drive.Update update)
        {
            _state = update.Body;
            _state.TimeStamp = DateTime.Now;
            update.ResponsePort.Post(new DefaultUpdateResponseType());
            yield break;
        }

        /// <summary>
        /// Subscribe Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(drive.Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    _subMgrPort.Post(new submgr.Submit(
                        subscribe.Body.Subscriber, DsspActions.UpdateRequest, _state, null));
                },
                delegate(Exception ex)
                {
                    LogError(ex);
                    throw ex;
                }
            );
        }

        /// <summary>
        /// Reliable Subscribe Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReliableSubscribeHandler(drive.ReliableSubscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    _subMgrPort.Post(new submgr.Submit(
                        subscribe.Body.Subscriber, DsspActions.UpdateRequest, _state, null));
                },
                delegate(Exception ex)
                {
                    LogError(ex);
                    throw ex;
                }
            );
        }

        #endregion

        #region Drive Operations

        /// <summary>
        /// Enable Drive Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> EnableDriveHandler(drive.EnableDrive enableDrive)
        {
            _state.IsEnabled = enableDrive.Body.Enable;
            _state.TimeStamp = DateTime.Now;

            // send notification to subscription manager
            _subMgrPort.Post(new submgr.Submit(_state, DsspActions.UpdateRequest));

            enableDrive.ResponsePort.Post(new DefaultUpdateResponseType());
            yield break;
        }

        int RequestPending = 0;

        /// <summary>
        /// Set Drive Power Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SetDrivePowerHandler(drive.SetDrivePower setDrivePower)
        {
            if (setDrivePower.Body == null)
            {
                LogError("setDrivePower.Body == null");
                setDrivePower.ResponsePort.Post(new Fault());
                yield break;
            }

            double leftPower = setDrivePower.Body.LeftWheelPower;
            double rightPower = setDrivePower.Body.RightWheelPower;

            // Dump requests that come too fast.
            // but let stop messages through
            if (RequestPending > 1 && leftPower != 0 && rightPower != 0)
            {
                setDrivePower.ResponsePort.Post(DefaultUpdateResponseType.Instance);
                yield break;
            }
            RequestPending++;

            BoundsCheck(leftPower);
            BoundsCheck(rightPower);

            //update state
            _state.TimeStamp = DateTime.Now;
            _state.LeftWheel.MotorState.CurrentPower = leftPower;
            _state.RightWheel.MotorState.CurrentPower = rightPower;

            //convert to native units
            int leftPowerScaled = (int)Math.Round(leftPower * 100 + 100);
            int rightPowerScaled = (int)Math.Round(rightPower * 100 + 100);

            //send hardware specific motor data
            brick.SetMotorsBody motordata = new brick.SetMotorsBody(leftPowerScaled, rightPowerScaled);

            Activate(Arbiter.Choice(_scribblerPort.SetMotors(motordata),
                 delegate(DefaultUpdateResponseType status)
                 {
                     setDrivePower.ResponsePort.Post(DefaultUpdateResponseType.Instance);
                     RequestPending--;
                 },
                 delegate(soap.Fault failure)
                 {
                     setDrivePower.ResponsePort.Post(failure);
                     RequestPending--;
                 }
            ));

            yield break;
        }


        private void BoundsCheck(double power)
        {
            if (power < -1 || power > 1)
            {
                LogError("motor power set incorrectly");
            }
        }


        /// <summary>
        /// All Stop Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> AllStopHandler(drive.AllStop allStop)
        {
            drive.SetDrivePower zeroPower = new drive.SetDrivePower();
            zeroPower.Body = new drive.SetDrivePowerRequest(0.0, 0.0);
            zeroPower.ResponsePort = allStop.ResponsePort;
            _mainPort.Post(zeroPower);
            yield break;
        }

        /// <summary>
        /// Set Drive Speed Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SetDriveSpeedHandler(drive.SetDriveSpeed setDriveSpeed)
        {
            LogError("Drive speed is not implemented");
            yield break;
        }


        /// <summary>
        /// Rotate Degrees Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> RotateDegreesHandler(drive.RotateDegrees rotateDegrees)
        {
            LogError("Rotate degrees is not implemented");
            yield break;
        }

        /// <summary>
        /// Drive Distance Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> DriveDistanceHandler(drive.DriveDistance driveDistance)
        {
            LogError("Drive distance is not implemented");
            yield break;
        }

        #endregion

#if false
        #region IMyroMovement Members

        public void Backward(float power)
        {
            drive.SetDrivePowerRequest request = new drive.SetDrivePowerRequest(-power, -power);
            _mainPort.SetDrivePower(request);
        }

        public void BackwardFor(float power, float seconds)
        {
            SpawnIterator<float, float, float>(-power, -power, seconds * 1000, MoveForHelper);
        }

        public void Forward(float power)
        {
            drive.SetDrivePowerRequest forwards = new drive.SetDrivePowerRequest(power, power);
            _mainPort.SetDrivePower(forwards);
        }

        public void ForwardFor(float power, float seconds)
        {
            SpawnIterator<float, float, float>(power, power, seconds * 1000, MoveForHelper);
        }

        public void Move(float translate, float rotate)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        public void SetMotors(float leftPower, float rightPower)
        {
            drive.SetDrivePowerRequest request = new drive.SetDrivePowerRequest(leftPower, rightPower);
            _mainPort.SetDrivePower(request);
        }

        public void SetMotorsFor(float leftPower, float rightPower, float seconds)
        {
            SpawnIterator<float, float, float>(leftPower, rightPower, seconds * 1000, MoveForHelper);
        }

        public void Stop()
        {
            drive.SetDrivePowerRequest request = new drive.SetDrivePowerRequest(0, 0);
            _mainPort.SetDrivePower(request);
        }

        public void Turn(string direction, float power)
        {
            if (direction.ToLower().CompareTo("left") == 0)
                TurnLeft(power);
            else if (direction.ToLower().CompareTo("right") == 0)
                TurnRight(power);
            else
                Console.WriteLine("Unrecognized direction");
        }

        public void TurnFor(string direction, float power, float seconds)
        {
            if (direction.ToLower().CompareTo("left") == 0)
                TurnLeftFor(power, seconds);
            else if (direction.ToLower().CompareTo("right") == 0)
                TurnRightFor(power, seconds);
            else
                Console.WriteLine("Unrecognized direction");
        }

        public void TurnLeft(float power)
        {
            drive.SetDrivePowerRequest left = new drive.SetDrivePowerRequest(-power, power);
            _mainPort.SetDrivePower(left);
        }

        public void TurnLeftFor(float power, float seconds)
        {
            SpawnIterator<float, float, float>(-power, power, seconds * 1000, MoveForHelper);
        }

        public void TurnRight(float power)
        {
            drive.SetDrivePowerRequest right = new drive.SetDrivePowerRequest(power, -power);
            _mainPort.SetDrivePower(right);
        }

        public void TurnRightFor(float power, float seconds)
        {
            SpawnIterator<float, float, float>(power, -power, seconds * 1000, MoveForHelper);
        }

        IEnumerator<ITask> MoveForHelper(float leftpower, float rightpower, float milliseconds)
        {
            drive.SetDrivePowerRequest request = new drive.SetDrivePowerRequest(leftpower, rightpower);
            _mainPort.SetDrivePower(request);

            yield return Arbiter.Receive(false, TimeoutPort((int)milliseconds), delegate(DateTime t) { });

            drive.SetDrivePowerRequest stop = new drive.SetDrivePowerRequest(0, 0);
            _mainPort.SetDrivePower(stop);
        }

        #endregion
#endif
    }


}
