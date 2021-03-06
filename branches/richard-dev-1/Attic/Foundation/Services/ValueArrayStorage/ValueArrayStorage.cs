//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     DSS Runtime Version: 2.0.730.3
//     CLR Runtime Version: 2.0.50727.1434
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Xml;
using W3C.Soap;
using valuearraystorage = Robotics.ValueArrayStorage;


namespace Robotics.ValueArrayStorage
{
    
    
    /// <summary>
    /// Implementation class for ValueArrayStorage
    /// </summary>
    [DisplayName("ValueArrayStorage")]
    [Description("The ValueArrayStorage Service")]
    [Contract(Contract.Identifier)]
    public class ValueArrayStorageService : DsspServiceBase
    {
        
        /// <summary>
        /// _state
        /// </summary>
        [ServiceState()]
        private ValueArrayStorageState _state = new ValueArrayStorageState();
        
        /// <summary>
        /// _main Port
        /// </summary>
        [ServicePort("/values", AllowMultipleInstances=false)]
        private ValueArrayStorageOperations _mainPort = new ValueArrayStorageOperations();
        
        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public ValueArrayStorageService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {
        }
        
        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
			base.Start();
			// Add service specific initialization here.
        }
    }
}
