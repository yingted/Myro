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
using System.ComponentModel;


namespace Microsoft.Robotics.Services.ToneGenerator
{
    /// <summary>
    /// The Dss Contract Definition
    /// </summary>
    [DisplayName("Generic Tone Generator")]
    [Description("Provides access to Tone Generator.")]
    public static class Contract
    {
        /// <summary>
        /// Contract Identifier
        /// </summary>
        public const string Identifier = "http://schemas.microsoft.com/2007/01/tonegenerator.html";
    }

    /// <summary>
    /// Tone Generator Operations
    /// </summary>
    public class ToneGeneratorOperations : PortSet<
        DsspDefaultLookup, 
        DsspDefaultDrop, 
        Get,
        PlayTone,
        PlayTone2>
    {
    }

    /// <summary>
    /// Get
    /// </summary>
    public class Get : Get<GetRequestType, PortSet<ToneGeneratorState, Fault>>
    {
    }


    /// <summary>
    /// Operation PlayTone: Plays a tone for a specified amount of time
    /// </summary>
    [Description("Plays a tone for a specified amount of time.")]
    public class PlayTone : Update<PlayToneRequest, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    /// <summary>
    /// Operation PlayTone2: Plays a dual tone for a specified amount of time
    /// </summary>
    [Description("Plays a dual tone for a specified amount of time.")]
    public class PlayTone2 : Update<PlayTone2Request, PortSet<DefaultUpdateResponseType, Fault>>
    {
    }

    /// <summary>
    /// Body of the PlayTone message
    /// </summary>
    [DataContract]
    [DataMemberConstructor]
    public class PlayToneRequest
    {
        private int _frequency;
        private int _duration;

        /// <summary>
        /// The frequency of the note to play
        /// </summary>
        public int Frequency
        {
            get { return _frequency; }
            set { _frequency = value; }
        }
        
        /// <summary>
        /// How long to play the note
        /// </summary>
        public int Duration
        {
            get { return _duration; }
            set { _duration = value; }
        }
    }

    /// <summary>
    /// Body of the PlayTone2 message
    /// </summary>
    [DataContract]
    [DataMemberConstructor]
    public class PlayTone2Request
    {
        private int _frequency1;
        private int _frequency2;
        private int _duration;

        /// <summary>
        /// The frequency of the note to play
        /// </summary>
        public int Frequency1
        {
            get { return _frequency1; }
            set { _frequency1 = value; }
        }

        /// <summary>
        /// The frequency of the note to play
        /// </summary>
        public int Frequency2
        {
            get { return _frequency2; }
            set { _frequency2 = value; }
        }

        /// <summary>
        /// How long to play the tone
        /// </summary>
        public int Duration
        {
            get { return _duration; }
            set { _duration = value; }
        }
    }

}
