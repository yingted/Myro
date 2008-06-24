//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1Types.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;

using W3C.Soap;
using System.ComponentModel;

using dssp = Microsoft.Dss.ServiceModel.Dssp;
using srv1ctrl = SharpLogic.Robotics.Surveyor.Srv1;

namespace SharpLogic.Robotics.Services.Surveyor.Srv1
{

    /// <summary>
    /// DSS Contract for Srv1
    /// </summary>
    static class Contract
    {
        /// <summary>
        /// The DSS Namespace for Srv1
        /// </summary>
        public const string Identifier = "http://schemas.sharplogic.com/robotics/2006/11/srv1.html";
    }

    /// <summary>
    /// The Srv1 Operations Port
    /// </summary>
    public class Srv1Operations : PortSet<DsspDefaultLookup, DsspDefaultDrop, Get, Replace, Subscribe, InitController>
    {
    }

    /// <summary>
    /// DSS Get Definition for Srv1
    /// </summary>
    [DisplayName("Get")]
    [Description("Gets the state of the SRV-1 brick.")]
    public class Get : Get<dssp.GetRequestType, PortSet<Srv1State, W3C.Soap.Fault>>
    {
    }

    /// <summary>
    /// DSS Replace Definition for Srv1
    /// </summary>
    [DisplayName("ChangeState")]
    [Description("Changes or indicates a change for the SRV-1 brick.")]
    public class Replace : Replace<Srv1State, PortSet<dssp.DefaultReplaceResponseType, W3C.Soap.Fault>>
    {
    }

    public class Subscribe : Subscribe<SubscribeRequestType, DsspResponsePort<SubscribeResponseType>, Srv1Operations>
    {
        public Subscribe()
        {
        }

        public Subscribe(SubscribeRequestType body)
            : base(body)
        {
        }
    }

    public class InitController : Submit<InitControllerRequest, DsspResponsePort<DefaultSubmitResponseType>>
    {
        public InitController()
            : base(new InitControllerRequest())
        {
        }
    }

    public class InitControllerRequest
    {
    }

    [Publish(PublishGroup.Partners)]
    public static class Partners
    {
        public const string Srv1 = "Srv1Service";
    }

}
