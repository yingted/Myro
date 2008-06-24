//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: SurveyorSrv1Services.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

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
using drivesrv = Microsoft.Robotics.Services.Drive.Proxy;
using motor = SharpLogic.Robotics.Services.Surveyor.Srv1.Motor;
using motorbase = Microsoft.Robotics.Services.Motor.Proxy;
using encoderbase = Microsoft.Robotics.Services.Encoder.Proxy;
using W3C.Soap;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Drive
{
    
    [Contract(Contract.Identifier)]
    [DisplayName("SRV-1 Generic Drive")]
    [Description("Provides access to the Surveyor SRV-1 differential drive service.\n(Uses the Generic Differential Drive contract.)")]
    public class DriveService : DsspServiceBase
    {
        /// <summary>
        /// Manifest name for the initial state partner configuration settings
        /// </summary>
        public const string InitialStatePartnerName = "SurveyorSrv1DifferentialDrive";

        /// <summary>
        /// xml configuration settings filename for default Surveyor SRV-1 Drive service
        /// </summary>
        public const string DefaultSurveyorDriveConfigFile = "DefaultSurveyorSrv1DifferentialDrive.xml";

        [InitialStatePartner(Optional = true, ServiceUri = DefaultSurveyorDriveConfigFile)]
        private DriveState _state = new DriveState();

        [ServicePort("/SurveyorSrv1Services", AllowMultipleInstances=false)]
        private SurveyorSrv1ServicesOperations _mainPort = new SurveyorSrv1ServicesOperations();

        [Partner("LeftMotor",
            Contract = motor.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.CreateAlways,
            Optional = false)]
        private motorbase.MotorOperations _leftMotorPort = new motorbase.MotorOperations();

        [Partner("RightMotor",
            Contract = motor.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.CreateAlways,
            Optional = false)]
        private motorbase.MotorOperations _rightMotorPort = new motorbase.MotorOperations();

        [Partner("DiffDrive",
           Contract = drivesrv.Contract.Identifier,
           CreationPolicy = PartnerCreationPolicy.CreateAlways,
           Optional = false)]
        private drivesrv.DriveOperations _drivePort = new drivesrv.DriveOperations();

        private bool _initFault = false;


        public DriveService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {
        }

        protected override void Start()
        {
            ConfigureInitialState();

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            //configure components synchronously
            SpawnIterator<DriveState>(_state, ConfigureDrive);

            // Publish the service to the local Node Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");
        }

        private void ConfigureInitialState()
        {
            //configure initial state
            if (_state == null)
            {
                _state = new DriveState();

                _state.WheelRadius = 0.052;
                _state.DistanceBetweenWheels = 0.165;
                _state.LeftMotorPortNumber = 1;
                _state.RightMotorPortNumber = 3;
                _state.LeftEncoderPortNumber = 4;
                _state.RightEncoderPortNumber = 5;

                _state.ForwardSpeed = 1.0;
                _state.ReverseSpeed = 0.8;
                _state.RotateSpeed = 0.5;

            }
        }

        public IEnumerator<ITask> ConfigureDrive(DriveState state)
        {
            PartnerType leftMotorPartner = this.ServiceInfo.PartnerList.Find(delegate(PartnerType partner) { return partner.Name.Name == "LeftMotor"; });
            PartnerType rightMotorPartner = this.ServiceInfo.PartnerList.Find(delegate(PartnerType partner) { return partner.Name.Name == "RightMotor"; });

            // Configure the left motor connection
            motorbase.MotorState Lconfigure = new motorbase.MotorState();
            Lconfigure.HardwareIdentifier = _state.LeftMotorPortNumber;
            Lconfigure.Name = "Left Motor";
            Lconfigure.PowerScalingFactor = 1;
            yield return Arbiter.Choice(_leftMotorPort.Replace(Lconfigure),
                delegate(DefaultReplaceResponseType success)
                {
                    LogInfo("Left Motor Port set");
                },
                delegate(Fault fault)
                {
                    LogError(fault);
                    _initFault = true;
                });
            if (_initFault)
                yield break;

            // Configure the right motor connection
            motorbase.MotorState Rconfigure = new motorbase.MotorState();
            Rconfigure.HardwareIdentifier = _state.RightMotorPortNumber;
            Rconfigure.Name = "Right Motor";
            Rconfigure.PowerScalingFactor = 1;
            yield return Arbiter.Choice(_rightMotorPort.Replace(Rconfigure),
                delegate(DefaultReplaceResponseType success)
                {
                    LogInfo("Right Motor Port set");
                },
                delegate(Fault fault)
                {
                    LogError(fault);
                    _initFault = true;
                });

            if (_initFault)
                yield break;

            // Configure the drive service
            drivesrv.DriveDifferentialTwoWheelState driveState = new drivesrv.DriveDifferentialTwoWheelState();
            driveState.DistanceBetweenWheels = state.DistanceBetweenWheels;
            driveState.TimeStamp = DateTime.Now;
            driveState.LeftWheel = new motorbase.WheeledMotorState();
            driveState.LeftWheel.Radius = _state.WheelRadius;
            driveState.LeftWheel.Name = "Left Wheel";
            driveState.LeftWheel.MotorState = new motorbase.MotorState();
            driveState.LeftWheel.GearRatio = 1.0;
            driveState.LeftWheel.EncoderState = new encoderbase.EncoderState();

            driveState.RightWheel = new motorbase.WheeledMotorState();
            driveState.RightWheel.Radius = _state.WheelRadius;
            driveState.RightWheel.Name = "Right Wheel";
            driveState.RightWheel.MotorState = new motorbase.MotorState();
            driveState.RightWheel.GearRatio = 1.0;
            driveState.RightWheel.EncoderState = new encoderbase.EncoderState();

            driveState.IsEnabled = true;

            yield return Arbiter.Choice(_drivePort.Update(driveState),
                delegate(DefaultUpdateResponseType success)
                {
                    LogInfo("Drive configuration updated");
                },
                delegate(Fault f)
                {
                    LogError(f);
                    _initFault = true;
                });
            if (_initFault)
                yield break;


            drivesrv.EnableDriveRequest en = new drivesrv.EnableDriveRequest();
            en.Enable = true;
            yield return Arbiter.Receive< DefaultUpdateResponseType>(false, _drivePort.EnableDrive(en),
                delegate(DefaultUpdateResponseType response){});

            yield break;
        }

        #region Service Handlers
        /// <summary>
        /// Get Handler
        /// </summary>
        /// <param name="get"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(Get get)
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
        public virtual IEnumerator<ITask> ReplaceHandler(Replace replace)
        {
            _state = replace.Body;
            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            yield break;
        }

        #endregion
    }
}
