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


namespace IPRE.DrawSquare
{
    [DataContract]
    public class DrawSquareState
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
        public const string Identifier = "http://www.roboteducation.org/drawsquare.html";
    }


    public class DrawSquareOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop, 
        Get>
    {
    }

    public class Get : Get<GetRequestType, PortSet<DrawSquareState, Fault>>
    {
    }

   
}
