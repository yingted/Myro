//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1State.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.ComponentModel;
using Microsoft.Dss.Core.Attributes;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1
{
    /// <summary>
    /// Srv1 State 
    /// </summary>
    [DataContract]
    public class Srv1State
    {
        /// <summary>
        /// Provide sane initial values
        /// </summary>
        public Srv1State()
        {
        }

        [DataMember(IsRequired = true)]
        [Description("The PC serial port used for the connection.")]
        public int ComPort
        {
            get { return this._comport; }
            set { this._comport = value; }
        }
        private int _comport;
    }
}
