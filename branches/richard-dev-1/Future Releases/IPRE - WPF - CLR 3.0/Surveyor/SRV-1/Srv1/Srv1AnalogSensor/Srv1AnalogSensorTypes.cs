//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1AnalogSensorTypes.CS $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;

using System.ComponentModel;
using W3C.Soap;

using dssp = Microsoft.Dss.ServiceModel.Dssp;

namespace SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogSensor
{

    /// <summary>
    /// DSS Contract for Srv1AnalogSensor
    /// </summary>
    static class Contract
    {
        /// <summary>
        /// The DSS Namespace for Srv1AnalogSensor
        /// </summary>                  
        public const string Identifier = "http://schemas.sharplogic.com/robotics/2006/11/srv1AnalogSensor.html";
    }

    /// <summary>
    /// The Srv1AnalogSensor Operations Port
    /// </summary>
    public class Srv1AnalogSensorOperations : PortSet<DsspDefaultLookup, DsspDefaultDrop, Get, Replace, Subscribe,
                                                       Configuration, SensorInit>
    {
    }

    /// <summary>
    /// DSS Get Definition for Srv1AnalogSensor
    /// </summary>
    [DisplayName("Get")]
    [Description("Gets the SRV-1 analog sensor state.")]
    public class Get : Get<dssp.GetRequestType, PortSet<AnalogSensorState, W3C.Soap.Fault>>
    {
    }

    /// <summary>
    /// DSS Replace Definition for Srv1AnalogSensor
    /// </summary>
    [DisplayName("ChangeState")]
    [Description("Changes or indicates a change to the entire state of the SRV-1 analog sensor.")]
    public class Replace : Replace<AnalogSensorState, PortSet<dssp.DefaultReplaceResponseType, W3C.Soap.Fault>>
    {
    }

    /// <summary>
    /// Subscription Manager Subscribe Definition for Srv1AnalogSensor
    /// </summary>
    public class Subscribe : Subscribe<dssp.SubscribeRequestType, PortSet<dssp.SubscribeResponseType, Fault>>
    {
    }

    /// <summary>
    /// DSS Configuration Definition for Srv1AnalogSensor
    /// </summary>
    [DisplayName("Configure")]
    [Description("Sets the configuration the SRV-1 analog sensor.")]
    public class Configuration : Submit<ConfigurationRequest, DsspResponsePort<DefaultSubmitResponseType>>
    {
    }


    /// <summary>
    /// DSS Configuration Request Definition for Srv1AnalogSensor
    /// </summary>
    [DataContract]
    public class ConfigurationRequest
    {
        [DataMember]
        public int serialNumber;
    }




    /// <summary>
    /// DSS Configuration Definition for Srv1AnalogSensor
    /// </summary>
    [DisplayName("SetSensorConfiguration")]
    [Description("Set the sensor type configuration for the SRV-1.")]
    public class SensorInit : Submit<InitRequest, DsspResponsePort<DefaultSubmitResponseType>>
    {
    }


    /// <summary>
    /// DSS Configuration Request Definition for Srv1AnalogSensor
    /// </summary>
    [DataContract]
    public class InitRequest
    {
        [DataMember]
        public Srv1SensorType s1;

        [DataMember]
        public Srv1SensorType s2;

        [DataMember]
        public Srv1SensorType s3;

        [DataMember]
        public Srv1SensorType s4;
    }



    [Publish(PublishGroup.Partners)]
    public static class Partners
    {
        public const string Srv1AnalogSensor = "Srv1AnalogSensorService";
    }

    #region Update Types

    internal class SensorUpdateOperations
        : PortSet<SensorConfiguration, SensorUpdate>
    {
    }

    public class UpdateRequestType
    {
        public DsspResponsePort<DefaultUpdateResponseType> ResponsePort;
    }

    public class SensorConfiguration : UpdateRequestType
    {
        public int sensorID;
        public string name;
        public int type;
    }

    public class SensorUpdate : UpdateRequestType
    {
        public int sensorID;
        public int value;
    }

    #endregion

}
