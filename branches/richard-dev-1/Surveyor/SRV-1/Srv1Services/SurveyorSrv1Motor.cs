//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: SurveyorSrv1Motor.cs $ $Revision$
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
using motor = Microsoft.Robotics.Services.Motor.Proxy;

using actuator = SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogActuator.Proxy;



namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Motor
{ 
    [Contract(Contract.Identifier)]
    [AlternateContract(motor.Contract.Identifier)]
    [DisplayName("SRV-1 Generic Motor")]
    [Description("Provides access to the Surveyor SRV-1 motor.\n(Uses the Generic Motor contract.)")]
    public class MotorService : DsspServiceBase
    {
        /// <summary>
        /// xml configuration settings filename for default Surveyor SRV-1 motor
        /// </summary>
        public const string DefaultSurveyorSrv1MotorConfigFile = "DefaultSurveyorSrv1MotorState.xml";

        [InitialStatePartner(Optional = true, ServiceUri = DefaultSurveyorSrv1MotorConfigFile)]
        private motor.MotorState _state = new motor.MotorState();

        [ServicePort("/SurveyorSrv1Motor", AllowMultipleInstances=true)]
        private motor.MotorOperations _mainPort = new motor.MotorOperations();

        // SRV-1 Actuator service
        [Partner(actuator.Partners.Srv1AnalogActuator, Contract = actuator.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        actuator.Srv1AnalogActuatorOperations _actuator = new actuator.Srv1AnalogActuatorOperations(); 

        public MotorService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {	
        }

        protected override void Start()
        {
            // Motor is uninitialized
            if (_state == null)
            {
                _state = new motor.MotorState();
                _state.HardwareIdentifier = 1;
                _state.Name = "Default Surveyor SRV-1 Motor on Port 1";
                _state.CurrentPower = 0.0;
                _state.PowerScalingFactor = 1.0;
                SaveState(_state);
            }

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");
        }

        /// <summary>
        /// Get Handler
        /// </summary>
        /// <param name="get"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(motor.Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        /// <summary>
        /// Configure the Surveyor SRV-1 Motor
        /// </summary>
        /// <param name="replace"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> ReplaceHandler(motor.Replace replace)
        {
            LogInfo(string.Format("Configuring {0} to port {1}", replace.Body.Name, replace.Body.HardwareIdentifier));
            _state = replace.Body;
            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            SaveState(_state);
            yield break;
        }


        /// <summary>
        /// SetMotorPower Handler
        /// </summary>
        /// <param name="update"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> SetMotorPowerHandler(motor.SetMotorPower update)
        {

            //Console.WriteLine("In set motor power handler .Got " + update.Body.TargetPower);
            if (_state.HardwareIdentifier == 0)
                throw new InvalidOperationException("Surveyor SRV-1 Motor is not configured");

            _state.CurrentPower = update.Body.TargetPower;
            float srv1Power = EnsureMinimumPower(_state.CurrentPower);

            actuator.ActuatorValueRequest req = new actuator.ActuatorValueRequest();
            // Leave the other motors unchanged
            req.activateA = false;
            req.activateB = false;

            switch (_state.HardwareIdentifier)
            {
                case 1:
                    req.actuatorA = srv1Power;
                    req.activateA = true;
                    break;
                case 2:
                    req.actuatorB = srv1Power;
                    req.activateB = true;
                    break;
            }
            // Post a message to the Surveyor SRV-1 Actuator
            //Console.WriteLine("==== Posting to actuator: " + req.actuatorA.Value + "," + req.actuatorB.Value);
            actuator.SetActuatorValues msg = new actuator.SetActuatorValues(req);

            _actuator.Post(msg);
            //_actuator.SetActuatorValues(req);

            yield return Arbiter.Receive<DefaultUpdateResponseType>(false,msg.ResponsePort,delegate(DefaultUpdateResponseType resp){});

            update.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }

        private float EnsureMinimumPower(double power)
        {
            double minPower = 0.15;
            if (Math.Abs(power) < minPower) { power = 0; }
            return unchecked((float)power); ;
        }
    }

    
    public static class Contract
    {
        public const string Identifier = "http://schemas.sharplogic.com/robotics/2006/11/surveyorsrv1motor.html";
    }

}
