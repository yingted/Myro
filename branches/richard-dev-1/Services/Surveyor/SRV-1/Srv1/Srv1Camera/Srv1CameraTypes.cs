//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1CameraTypes.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;

using W3C.Soap;
using Microsoft.Dss.Core.DsspHttp;

using camera = SharpLogic.Robotics.Services.Surveyor.Srv1.Camera;
using System.ComponentModel;

namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Camera
{
    
    public static class Contract
    {
        public const string Identifier = "http://schemas.sharplogic.com/robotics/2006/11/srv1camera.html";
    }

    public class Srv1CameraServiceOperations : PortSet<DsspDefaultLookup, DsspDefaultDrop, Get, Replace, QueryFrame, UpdateDevice, UpdateFrame, UpdateFormat, HttpGet, Subscribe, HttpPost>
    {
    }

    [DisplayName("Get")]
    [Description("Retrieves the capture state and a list of connected cameras.")]
    public class Get : Get<GetRequestType, PortSet<Srv1CameraState, Fault>>
    {
    }

    [DisplayName("InitialConfiguration")]
    [Description("Provides the startup state of the camera service.")]
    public class Replace : Replace<Srv1CameraState, PortSet<DefaultReplaceResponseType, Fault>>
    {
    }

    [DisplayName("GetFrame")]
    [Description("Gets the most recently captured frame.")]
    public class QueryFrame : Query<QueryFrameRequest, PortSet<QueryFrameResponse, Fault>>
    {
    }

    [DisplayName("SetCamera")]
    [Description("Selects which connected camera will be used for capture.")]
    public class UpdateDevice : Update<UpdateDeviceRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    [DisplayName("NewFrame")]
    [Description("Indicates when a new frame has been captured.")]
    public class UpdateFrame : Update<UpdateFrameRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    [DisplayName("SetFormat")]
    [Description("Changes which size and compression technology will be used for capture.")]
    public class UpdateFormat : Update<Format, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    public class Subscribe : Subscribe<SubscribeRequestType, PortSet<SubscribeResponseType, Fault>>
    {
    }
}
