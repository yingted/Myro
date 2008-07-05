//-----------------------------------------------------------------------
//  IPRE
//  
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;

using W3C.Soap;
using Microsoft.Dss.Core.Attributes;


namespace IPRE.LightSeek
{
    [DataContract]
    public class LightSeekState
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
        public const string Identifier = "http://www.roboteducation.org/lightseek.html";
    }

    public class LightSeekOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop,
        Get>
    {
    }

    public class Get : Get<GetRequestType, PortSet<LightSeekState, Fault>>
    {
    }

}
