//-----------------------------------------------------------------------
//  IPRE Wander Service
//  Drives around trying not to bump into anything
//  Also speaks
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;
using W3C.Soap;
using Microsoft.Dss.Core.Attributes;


namespace IPRE.Wander
{
    [DataContract]
    public class WanderState
    {
        private string _currentAction = "Null";

        [DataMember]
        public string CurrentAction
        {
            get { return _currentAction; }
            set { _currentAction = value; }
        }
    }

    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/wander.html";
    }

    public class WanderOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop,
        Get>
    {
    }

    public class Get : Get<GetRequestType, PortSet<WanderState, Fault>>
    {
    }

}
