//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: NxtDirectTypes.cs $ $Revision$
//-----------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;
using System.ComponentModel;

using W3C.Soap;

namespace Microsoft.Robotics.Services.LegoNxt.NxtDirect
{
    
    /// <summary>
    /// NxtDirect Contract
    /// </summary>
    public static class Contract
    {
        /// The Unique Contract Identifier for the NxtDirect service
        public const String Identifier = "http://schemas.microsoft.com/robotics/2006/05/legonxt/nxtdirect.html";
    }

    [DataContract]
    [Description("The state of the NXT after sending a direct command.")]
    public class NxtDirectState
    {
        [DataMember]
        [Browsable(false)]
        [Description("Identifies if the service has been successfully connected with the brick.")]
        public string Status;

        [DataMember(IsRequired = false)]
        [Browsable (false)]
        [Description("When connected, the URI for the brick service.")]
        public string BrickService;
    }
    [Description("Gets the state of the NXT.")]
    public class Get : Get<GetRequestType, PortSet<NxtDirectState, Fault>> { }


    [ServicePort]
    public class NxtDirectOperations : PortSet<DsspDefaultLookup, DsspDefaultDrop, Get,
        SendLegoGetInputValues, SendLegoGetOutputState, SendLegoKeepAlive,
        SendLegoLSGetStatus, SendLegoLSRead, SendLegoLSWrite, SendLegoMessageRead, 
        SendLegoMessageWrite, SendLegoResetInputScaledValue, SendLegoResetMotorPosition,
        SendLegoSetInputMode, SendLegoSetOutputState, SendLegoGetButtonState
        >
    {
    }

    /// <summary>
    /// Request/Response definition for sending a LegoSetOutputState
    /// </summary>
    [Description("Sends the LEGO SetOutputState command to the NXT brick.")]
    public class SendLegoSetOutputState : Submit<LegoSetOutputState, PortSet<LegoResponse, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoResponseGetInputValues
    /// </summary>
    [Description("Sends the LEGO GetInputValues command to the NXT brick.")]
    public class SendLegoGetInputValues : Submit<LegoGetInputValues, PortSet<LegoResponseGetInputValues, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoGetOutputState
    /// </summary>
    [Description("Sends the LEGO GetOutputState command to the NXT brick.")]
    public class SendLegoGetOutputState : Submit<LegoGetOutputState, PortSet<LegoResponseGetOutputState, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoKeepAlive
    /// </summary>
    [Description("Sends the LEGO KeepAlive command to the NXT brick.")]
    public class SendLegoKeepAlive : Submit<LegoKeepAlive, PortSet<LegoResponseKeepAlive, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoLSGetStatus
    /// </summary>
    [Description("Sends the LEGO LSGetStatus command to the NXT brick.")]
    public class SendLegoLSGetStatus : Submit<LegoLSGetStatus, PortSet<LegoResponseLSGetStatus, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoLSRead
    /// </summary>
    [Description("Sends the LEGO LSRight command to the NXT brick.")]
    public class SendLegoLSRead : Submit<LegoLSRead, PortSet<LegoResponseLSRead, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoLSWrite
    /// </summary>
    [Description("Sends the LEGO LSWrite command to the NXT brick.")]
    public class SendLegoLSWrite : Submit<LegoLSWrite, PortSet<LegoResponse, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoMessageRead
    /// </summary>
    [Description("Sends the LEGO MessageRead command to the NXT brick.")]
    public class SendLegoMessageRead : Submit<LegoMessageRead, PortSet<LegoResponseMessageRead, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoMessageWrite
    /// </summary>
    [Description("Sends the LEGO MessageWrite command to the NXT brick.")]
    public class SendLegoMessageWrite : Submit<LegoMessageWrite, PortSet<LegoResponse, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoResetInputScaledValue
    /// </summary>
    [Description("Sends the LEGO ResetInputScaledValue command to the NXT brick.")]
    public class SendLegoResetInputScaledValue : Submit<LegoResetInputScaledValue, PortSet<LegoResponse, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoResponseGetInputValues
    /// </summary>
    [Description("Sends the LEGO GetButtonState command to the NXT brick.\nGets the state of the buttons on the NXT.")]
    public class SendLegoGetButtonState : Submit<LegoGetButtonState, PortSet<LegoResponseGetButtonState, Fault>>
    {
    }
    /// <summary>
    /// Request/Response definition for sending a LegoResetMotorPosition
    /// </summary>
    [Description("Sends the LEGO ResetMotorPosition command to the NXT brick.")]
    public class SendLegoResetMotorPosition : Submit<LegoResetMotorPosition, PortSet<LegoResponse, Fault>>
    {
    }

    /// <summary>
    /// Request/Response definition for sending a LegoSetInputMode
    /// </summary>
    [Description("Sends the LEGO SetInputMode command to the NXT brick.")]
    public class SendLegoSetInputMode : Submit<LegoSetInputMode, PortSet<LegoResponse, Fault>>
    {
    }
}
