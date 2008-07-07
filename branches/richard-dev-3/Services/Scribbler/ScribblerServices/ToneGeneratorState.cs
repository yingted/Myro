//------------------------------------------------------------------------------
// Tone Generator 
// Generic Service Contract
//
// Interface for a simple speaker which can only play tones
//
//------------------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using System;
using System.Collections.Generic;
using W3C.Soap;

namespace Microsoft.Robotics.Services.ToneGenerator
{
    
    /// <summary>
    /// Tone Generator State
    /// </summary>
    [DataContract()]
    public class ToneGeneratorState
    {
        private bool _playingTone;

        public bool PlayingTone
        {
            get { return this._playingTone; }
            set { this._playingTone = value; }
        }
    }

}
