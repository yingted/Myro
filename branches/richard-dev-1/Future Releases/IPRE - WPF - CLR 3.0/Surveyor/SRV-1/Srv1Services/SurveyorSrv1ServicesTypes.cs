//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: SurveyorSrv1ServicesTypes.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;

using System;
using System.Collections.Generic;
using System.ComponentModel;
using W3C.Soap;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Drive
{
    
    public static class Contract
    {
        public const string Identifier = "http://schemas.sharplogic.com/robotics/2006/11/surveyorsrv1services.html";
    }

    [DataContract]
    public class DriveState
    {
        [DataMember]
        public double DistanceBetweenWheels;

        [DataMember]
        public double WheelRadius;

        [DataMember]
        public int LeftMotorPortNumber;

        [DataMember]
        public int RightMotorPortNumber;

        [DataMember]
        public int LeftEncoderPortNumber;

        [DataMember]
        public int RightEncoderPortNumber;

        [DataMember]
        public double ForwardSpeed;

        [DataMember]
        public double ReverseSpeed;

        [DataMember]
        public double RotateSpeed;
    }

    public class SurveyorSrv1ServicesOperations : PortSet<DsspDefaultLookup, DsspDefaultDrop, Get, Replace>
    {
    }
    [DisplayName("Get")]
    [Description("Gets the state.")]
    public class Get : Get<GetRequestType, PortSet<DriveState, Fault>>
    {
    }
    [DisplayName("ChangeState")]
    [Description("Changes or indicates a change to the entire state.")]
    public class Replace : Replace<DriveState, PortSet<DefaultReplaceResponseType, Fault>>
    {
    }
}
