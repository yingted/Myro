//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1AnalogSensorState.CS $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using Microsoft.Dss.Core.Attributes;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogSensor
{

    //     Sensor types
    [DataContract]
    public enum Srv1SensorType
    {
        /// <summary>
        /// Keep previous configuration
        /// </summary>
        NoChange = -1,

        /// <summary>
        /// No sensor.
        /// </summary>
        None = 0,

        /// <summary>
        /// Touch sensor. The sensor's value will either be 1 for "pressed" 
        /// or 0 for "not pressed".
        /// </summary>
        Touch = 1,

        /// <summary>
        /// Infra red
        /// </summary>
        InfraRed = 2
    }

    // Srv1 Actuator 
    [DataContract]
    public class Sensor
    {
        [DataMember(IsRequired = true)]
        public string name = "";
        [DataMember(IsRequired = true)]
        public Srv1SensorType type = Srv1SensorType.None;
        [DataMember(IsRequired = true)]
        public int value = 0;
    }


    
    /// <summary>
    /// Srv1 Analog Sensor State
    /// </summary>
    [DataContract]
    public class AnalogSensorState
    {

        const int sensorCount = 4;

        /// <summary>
        /// Provide sane initial values
        /// </summary>
        public AnalogSensorState()
        {
            Initialize("<not attached to hardware>", 0);
        }

        public void Initialize(string serialNumber, int count)
        {
            Sensors = new Sensor[sensorCount];
            for (int i = 0; i < sensorCount; i++)
            {
                Sensors[i] = new Sensor();
            }
        }


        [DataMember(IsRequired = true)]
        public Sensor[] Sensors;

    }
}
