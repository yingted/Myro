//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNxtDrive.cs $ $Revision$
//-----------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;

using System;
using System.Net;
using System.Collections.Generic;
using System.ComponentModel;
using System.Security.Permissions;

using drive = Microsoft.Robotics.Services.Drive.Proxy;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using nxt = Microsoft.Robotics.Services.LegoNxt.Proxy;
using motor = Microsoft.Robotics.Services.Motor.Proxy;
using W3C.Soap;

namespace Microsoft.Robotics.Services.Sample.LegoNxtDrive
{
    
    /// <summary>
    /// Lego Nxt Drive Service
    /// </summary>
    [Contract(Contract.Identifier)]
    [AlternateContract("http://schemas.microsoft.com/robotics/2006/05/drive.html")]
    [DisplayName("NXT Generic Drive")]
    [Description("Controls the LEGO® MINDSTORMS® NXT motor configuration as a two-wheel differential drive.\n(Uses the Generic Drive contract.)")]
    public class LegoNxtDriveService : DsspServiceBase
    {
        private int _requestPending = 0;

        [EmbeddedResource("Microsoft.Robotics.Services.LegoNxt.LegoDrive.xslt")]
        string _transform = null; 

        /// <summary>
        /// The LEGO NXT Drive state
        /// </summary>
        [InitialStatePartner(Optional = true, ServiceUri = "LEGO.NXT.TriBot.LegoDrive.Config.xml")]
        drive.DriveDifferentialTwoWheelState _state = new drive.DriveDifferentialTwoWheelState();

        /// <summary>
        /// main Port
        /// </summary>
        [ServicePort("/legonxtdrive", AllowMultipleInstances=true)]
        private drive.DriveOperations _mainPort = new Microsoft.Robotics.Services.Drive.Proxy.DriveOperations();

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        [Partner("Nxt", Contract = nxt.Contract.Identifier, 
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, 
            Optional = false)]
        private nxt.LegoNxtOperations _legoPort = new nxt.LegoNxtOperations();

        ////For XSLT
        //DsspHttpUtilitiesPort _httpUtilities = new DsspHttpUtilitiesPort();
        //const string _transform = ServicePaths.Transforms + "/Drive.xslt";

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public LegoNxtDriveService(DsspServiceCreationPort creationPort) : 
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
                CreateDefaultState();
            }

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local service Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");
        }

        /// <summary>
        /// Set up the standard Tri-bot configuration
        /// </summary>
        void CreateDefaultState()
        {
            _state = new drive.DriveDifferentialTwoWheelState();

            //initialize motor platform specific to Lego NXT
            _state.DistanceBetweenWheels = 0.115;
            _state.TimeStamp = DateTime.Now;
            _state.IsEnabled = false;

            _state.LeftWheel = new motor.WheeledMotorState();
            _state.LeftWheel.Name = "Medium Wheel";
            _state.LeftWheel.Radius = 0.028;
            _state.LeftWheel.GearRatio = 1.0;
            _state.LeftWheel.MotorState = new motor.MotorState("Left Motor", 3, 100.0, false, new PhysicalModel.Proxy.Pose());
            _state.LeftWheel.EncoderState = new Encoder.Proxy.EncoderState(_state.LeftWheel.MotorState.HardwareIdentifier, 6);

            _state.RightWheel = new motor.WheeledMotorState();
            _state.RightWheel.Name = "Medium Wheel";
            _state.RightWheel.Radius = 0.028;
            _state.RightWheel.GearRatio = 1.0;
            _state.RightWheel.MotorState = new motor.MotorState("Right Motor", 2, 100.0, true, new PhysicalModel.Proxy.Pose());
            _state.RightWheel.EncoderState = new Encoder.Proxy.EncoderState(_state.RightWheel.MotorState.HardwareIdentifier, 6);

            SaveState(_state);
        }

        #region Service Handlers

        /// <summary>
        /// Get Handler
        /// </summary>
        /// <param name="get"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(drive.Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        /// <summary>
        /// HttpGet Handler
        /// </summary>
        /// <param name="get"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> HttpGetHandler(HttpGet httpGet)
        {
            HttpResponseType rsp = new HttpResponseType(HttpStatusCode.OK, _state, _transform);
            httpGet.ResponsePort.Post(rsp);
            yield break;

        }

        /// <summary>
        /// Update Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> UpdateHandler(drive.Update update)
        {
            _state = update.Body;
            SaveState(_state);
            update.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }

        /// <summary>
        /// Subscribe Handler
        /// </summary>
        /// <param name="subscribe"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> SubscribeHandler(drive.Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    SendNotification<drive.Update>(_subMgrPort, subscribe.Body.Subscriber, _state);
                },
                null
            );

            yield break;
        }

        /// <summary>
        /// ReliableSubscribe Handler
        /// </summary>
        /// <param name="subscribe"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> ReliableSubscribeHandler(drive.ReliableSubscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort),
                    delegate(SuccessResult success)
                    {
                        SendNotification<drive.Update>(_subMgrPort, subscribe.Body.Subscriber, _state);
                    }, null);

            yield break;
        }

        /// <summary>
        /// EnableDrive Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> EnableDriveHandler(drive.EnableDrive update)
        {
            _state.IsEnabled = update.Body.Enable;
            _state.TimeStamp = DateTime.Now;
            update.ResponsePort.Post(new DefaultUpdateResponseType());
            SendNotification<drive.Update>(_subMgrPort, _state);
            yield break;
        }

        /// <summary>
        /// SetDrivePower Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> SetDrivePowerHandler(drive.SetDrivePower update)
        {
            // For now, just dump requests that come in too fast.
            if (_requestPending > 0)
            {
                update.ResponsePort.Post(new DefaultUpdateResponseType());
                yield break;
            }
            //flip direction if necessary
            double revPow = update.Body.LeftWheelPower;
            if (_state.LeftWheel.MotorState.ReversePolarity)
                revPow = (-1) * revPow;


            //send left motor command
            nxt.LegoSetOutputState motors = new nxt.LegoSetOutputState();
            motors.OutputPort = _state.LeftWheel.MotorState.HardwareIdentifier - 1;
            motors.PowerSetPoint = (int)(update.Body.LeftWheelPower * _state.LeftWheel.MotorState.PowerScalingFactor);
            motors.Mode = nxt.LegoOutputMode.PowerControl;
            motors.RegulationMode = nxt.LegoRegulationMode.MotorSpeed;
            motors.TurnRatio = 0;
            motors.RunState = nxt.LegoRunState.Running;
            motors.RotationLimit = 0; //run forever
            motors.RequireResponse = false;
            _requestPending++;

            PortSet<nxt.LegoResponse, Fault> resp1Port = _legoPort.SendLegoCommand(motors);
/*            Activate(Arbiter.Choice(_legoPort.SendLegoCommand(motors),
                delegate(nxt.LegoResponse response)
                {
                    _requestPending--;
                },
                delegate(Fault fault)
                {
                    _requestPending--;
                }
            ));
            */
            //send right motor command
            motors = new nxt.LegoSetOutputState();
            motors.OutputPort = _state.RightWheel.MotorState.HardwareIdentifier - 1;
            motors.PowerSetPoint = (int)(update.Body.RightWheelPower * _state.RightWheel.MotorState.PowerScalingFactor);
            motors.Mode = nxt.LegoOutputMode.PowerControl;
            motors.RegulationMode = nxt.LegoRegulationMode.MotorSpeed;
            motors.TurnRatio = 0;
            motors.RunState = nxt.LegoRunState.Running;
            motors.RotationLimit = 0; //run forever
            motors.RequireResponse = false;
            _requestPending++;
            //Activate(Arbiter.Choice(_legoPort.SendLegoCommand(motors),
            //    delegate(nxt.LegoResponse response)
            //    {
            //        _requestPending--;
            //    },
            //    delegate(Fault fault)
            //    {
            //        _requestPending--;
            //    }
            //));

            yield return Arbiter.Choice(_legoPort.SendLegoCommand(motors),
                delegate(nxt.LegoResponse response)
                {
                    _requestPending--;
                },
                delegate(Fault fault)
                {
                    _requestPending--;
                }
            );

            yield return Arbiter.Choice(resp1Port,
                delegate(nxt.LegoResponse response)
                {
                    _requestPending --;
                },
                delegate(Fault fault)
                {
                    _requestPending --;
                }
            );

            LogInfo(update.Body.LeftWheelPower * 100 + ":" + update.Body.RightWheelPower * 100);
            _state.TimeStamp = DateTime.Now;

            // forward notification
            SendNotification(_subMgrPort, new drive.Update(_state));

            //reply to sender
            update.ResponsePort.Post(new DefaultUpdateResponseType());
            yield break;
        }
        
        /// <summary>
        /// SetDriveSpeed Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> SetDriveSpeedHandler(drive.SetDriveSpeed update)
        {
            // TODO: Implement SetDriveSpeed operation here.
            throw new NotImplementedException("SetDriveSpeed is not Implemented.");
        }

        /// <summary>
        /// RotateDegrees Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> RotateDegreesHandler(drive.RotateDegrees update)
        {
            // TODO: Implement RotateDegrees operation here.
            throw new NotImplementedException("RotateDegrees is not Implemented.");
        }

        /// <summary>
        /// DriveDistance Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> DriveDistanceHandler(drive.DriveDistance update)
        {
            // TODO: Implement DriveDistance operation here.
            throw new NotImplementedException("DriveDistance is not Implemented.");
        }
        
        /// <summary>
        /// AllStop Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> AllStopHandler(drive.AllStop update)
        {
            // Create a Drive request with both wheels stopped.
            // Call the SetDrivePowerHandler, but redirect the responses
            // to our response port.
            drive.SetDrivePower setDrivePower = new drive.SetDrivePower(new drive.SetDrivePowerRequest(), update.ResponsePort);
            SpawnIterator<drive.SetDrivePower>(setDrivePower, SetDrivePowerHandler);
            yield break;
        }

        /// <summary>
        /// HttpPost Handler
        /// </summary>
        /// <param name="submit"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> HttpPostHandler(HttpPost submit)
        {
            throw new NotImplementedException();
            //// Use helper to read form data
            //ReadFormData readForm = new ReadFormData(httpPost.Body.Context);
            //_httpUtilities.Post(readForm);

            //// Wait for result
            //Activate(Arbiter.Choice(readForm.ResultPort,
            //    delegate(NameValueCollection parameters)
            //    {
            //        if (parameters["StartDashboard"] != null)
            //        {
            //            string Dashboardcontract = "http://schemas.microsoft.com/robotics/2006/01/simpledashboard.html";
            //            ServiceInfoType info = new ServiceInfoType(Dashboardcontract);
            //            cons.Create create = new cons.Create(info);
            //            create.TimeSpan = DsspOperation.DefaultShortTimeSpan;

            //            ConstructorPort.Post(create);
            //            Arbiter.Choice(
            //                create.ResponsePort,
            //                delegate(CreateResponse createResponse) { },
            //                delegate(Fault f) { LogError(f); }
            //            );
            //        }
            //        else if (parameters["EnableDrive"] != null)
            //        {
            //            _mainPort.Post(new EnableDrive());
            //        }
            //        else if (parameters["AllStop"] != null)
            //        {
            //            _mainPort.Post(new AllStop());
            //        }
            //        else if (parameters["DrivePower"] != null)
            //        {
            //            double power = 0;
            //            double.TryParse(parameters["Power"], out power);
            //            SetDrivePowerRequest drivepower = new SetDrivePowerRequest();
            //            drivepower.LeftWheelPower = power;
            //            drivepower.RightWheelPower = power;
            //            _mainPort.Post(new SetDrivePower(drivepower));
            //        }
            //        else if (parameters["DriveDistance"] != null)
            //        {
            //            double power = 0;
            //            double.TryParse(parameters["Power"], out power);
            //            double distance = 0;
            //            double.TryParse(parameters["Distance"], out distance);
            //            DriveDistanceRequest drivedist = new DriveDistanceRequest(distance, power);
            //            _mainPort.Post(new DriveDistance(drivedist));
            //        }
            //        else if (parameters["RotateAngle"] != null)
            //        {
            //            double power = 0;
            //            double.TryParse(parameters["Power"], out power);
            //            double angle = 0;
            //            double.TryParse(parameters["Angle"], out angle);
            //            RotateDegreesRequest turn = new RotateDegreesRequest(angle, power);
            //            _mainPort.Post(new RotateDegrees(turn));
            //        }
            //        else if (parameters["CustomPower"] != null)
            //        {
            //            double leftpower = 0;
            //            double.TryParse(parameters["LeftPower"], out leftpower);
            //            double rightpower = 0;
            //            double.TryParse(parameters["RightPower"], out rightpower);
            //            SetDrivePowerRequest drivepower = new SetDrivePowerRequest();
            //            drivepower.LeftWheelPower = leftpower;
            //            drivepower.RightWheelPower = rightpower;
            //            _mainPort.Post(new SetDrivePower(drivepower));
            //        }
            //        else
            //        {
            //            HttpPostFailure(httpPost, "Unknown Http Post");
            //        }
            //    },
            //    delegate(Exception Failure)
            //    {
            //        LogError(Failure.Message);
            //    })
            //);
            //yield break;
        }

        #endregion
    }
}
