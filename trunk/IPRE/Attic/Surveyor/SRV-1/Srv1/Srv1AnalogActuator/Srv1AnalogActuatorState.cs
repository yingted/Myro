//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1AnalogActuatorState.CS $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using Microsoft.Dss.Core.Attributes;

namespace SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogActuator
{

    // Srv1 Actuator power states
    [DataContract]
    public enum Srv1ActuatorPowerState
    {
        /// Off (the motor is off and locked in position)
        Locked = 0,
        /// On (the motor is actively running)
        On = 1,
        /// Float (the motor is off and coasting without power)
        Float = 2
    };

    // Srv1 Actuator 
    [DataContract]
    public class Actuator
    {
        [DataMember(IsRequired = true)]
        public string name = "";
        [DataMember(IsRequired = true)]
        public float value = 0;
    }


    /// <summary>
    /// Analog Actuator State
    /// </summary>
    [DataContract]
    public class AnalogActuatorState
    {
        const int actuatorPadCount = 2;

        /// <summary>
        /// Provide sane initial values
        /// </summary>
        public AnalogActuatorState()
        {
            Initialize("<not Initialized>");
            actuator = new Actuator[actuatorPadCount]; 
            for (int i = 0; i < actuatorPadCount; i++)
            {
                actuator[i] = new Actuator();
            }
        }

        public void Initialize(string serialNumber)
        {
            configuration = serialNumber;
        }


        /// <summary>
        /// Comma separated configuration information consisting of:
        ///     SerialNumber
        /// Specify with Query arguments:
        ///     Configuration=_serial_number_
        /// </summary>
        [DataMember(IsRequired = true)]
        public String configuration;

        [DataMember(IsRequired = true)]
        public Actuator[] actuator;
    }
}
