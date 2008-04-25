//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1AnalogActuatorTypes.CS $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;

using System.ComponentModel;
using W3C.Soap;

using dssp = Microsoft.Dss.ServiceModel.Dssp;

namespace SharpLogic.Robotics.Services.Surveyor.Srv1.AnalogActuator
{

    /// <summary>
    /// DSS Contract for Srv1AnalogActuator
    /// </summary>
    static class Contract
    {
        /// <summary>
        /// The DSS Namespace for Srv1AnalogActuator
        /// </summary>
        public const string Identifier = "http://schemas.sharplogic.com/robotics/2006/11/srv1analogactuator.html";
    }

    /// <summary>
    /// The Srv1AnalogActuator Operations Port
    /// </summary>
    public class Srv1AnalogActuatorOperations : PortSet<DsspDefaultLookup, DsspDefaultDrop, Get, Replace, Subscribe,
                                                         SetActuatorValues>
    {
    }

    #region message types
    /// <summary>
    /// DSS Get Definition for Srv1AnalogActuator
    /// </summary>
    [DisplayName("Get")]
    [Description("Gets the state of the SRV-1 analog actuator.")]
    public class Get : Get<dssp.GetRequestType, PortSet<AnalogActuatorState, W3C.Soap.Fault>>
    {
    }

    /// <summary>
    /// DSS Replace Definition for Srv1AnalogActuator
    /// </summary>
    [DisplayName("ChangeState")]
    [Description("Changes or indicates a change to the entire state of the SRV-1 analog actuator.")]
    public class Replace : Replace<AnalogActuatorState, PortSet<dssp.DefaultReplaceResponseType, W3C.Soap.Fault>>
    {
    }

    /// <summary>
    /// Subscription Manager Subscribe Definition for Srv1AnalogActuator
    /// </summary>
    public class Subscribe : Subscribe<dssp.SubscribeRequestType, PortSet<dssp.SubscribeResponseType, Fault>>
    {
    }

    /// <summary>
    /// DSS Configuration Definition for Srv1AnalogActuator
    /// </summary>
    [DisplayName("SetActuatorValues")]
    [Description("Sets the values of the actuators.")]
    public class SetActuatorValues : Update<ActuatorValueRequest, PortSet<DefaultUpdateResponseType,Fault>> { }

    [DataContract]
    public class ActuatorValueRequest
    {
        [DataMember]
        public float actuatorA;

        [DataMember]
        public bool activateA;

        [DataMember]
        public float actuatorB;

        [DataMember]
        public bool activateB;
    }

    #endregion 

    #region Update Types

    internal class ActuatorUpdateOperations
        : PortSet<ActuatorConfiguration, InternalSetActuatorValue>
    {
    }

    [DataContract]
    public class UpdateRequestType
    {
        public DsspResponsePort<DefaultUpdateResponseType> ResponsePort;
    }

    [DataContract]
    public class ActuatorConfiguration : UpdateRequestType
    {
        [DataMember]
        public int actuatorID;
        [DataMember]
        public string name;
        [DataMember]
        public float value;
    }

    [DataContract]
    public class InternalSetActuatorValue : UpdateRequestType
    {
        [DataMember]
        public int actuatorID;
        [DataMember]
        public float value;
    }

    #endregion


    [Publish(PublishGroup.Partners)]
    public static class Partners
    {
        public const string Srv1AnalogActuator = "Srv1AnalogActuatorService";
    }
}
