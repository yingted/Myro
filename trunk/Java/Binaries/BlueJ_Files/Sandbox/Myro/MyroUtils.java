/*
 * Myro/Java license - GPL
 * 
 * Myro/Java is a Java implementation of the Myro API, defined by the Institute for Robots in
 * Education (IPRE).  See http://wiki.roboteducation.org for more information.
 * 
 * Copyright 2010-2011 Douglas Harms dharms@depauw.edu
 * 
 * This file is part of Myro/Java.
 * 
 * Myro/Java is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * 
 * Myro/Java is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Myro/Java.  If not, see <http://www.gnu.org/licenses/>.
 */

package Myro;

import java.util.Random;
import com.sun.speech.freetts.*; // for text-to-speech

/**
 * Miscellaneous methods for Myro/Java programs
 * 
 * @author Douglas Harms 
 * @version 1.0
 */
public class MyroUtils
{

    private static Random _randomSeq;
    private static boolean _newCountDown;
    private static long _startTime;
    private static Voice _voice;

    // static constructor
    static
    {
        String VOICE_NAME = "kevin16";

        // initialize random number sequence
        _randomSeq = new Random();

        // initialize timeRemaining 
        _newCountDown = true;

        // initialize text-to-speach
        VoiceManager voiceManager = VoiceManager.getInstance();
        _voice = voiceManager.getVoice( VOICE_NAME );
        _voice.allocate();
    }

    /**
     * Cause the current thread to sleep for numSeconds.
     * 
     * @pre numSeconds >= 0.0
     * 
     * @param numSeconds The length of time to sleep.
     */
    public static void sleep( double numSeconds )
    {
        assert numSeconds >= 0.0 : "numSeconds must be >= 0.0";
        try
        {
            Thread.sleep( (int)(numSeconds * 1000.0) );
        } catch (InterruptedException e) {}

    }

    /**
     * Returns a random integer within a specified range.
     * 
     * @pre low &lt= high
     * 
     * @param low Low end of range
     * @param high High end of range
     * @return A uniformly distributed random int between low (inclusive) and high (inclusive)
     */
    public static int randomInt( int low, int high )
    {
        assert low <= high : "low cannot be greater than high";

        return _randomSeq.nextInt( high-low+1 ) + low ;
    }

    /**
     * Returns a random double in the range 0.0 (inclusive) and 1.0 (exclusive).
     * 
     * @return A uniformly distributed random double between 0.0 (inclusive) and 1.0 (exclusive)
     */
    public static double randomDouble( )
    {
        return _randomSeq.nextDouble();
    }

    /**
     * Controls a while-loop for a specific number of seconds.
     * 
     * @param seconds number of seconds to loop
     * @return true iff the specified number of seconds has not elapsed
     */
    public static boolean timeRemaining( double seconds )
    {
        if( _newCountDown )
        {
            _startTime = System.currentTimeMillis();
            _newCountDown = false;
        }

        if( System.currentTimeMillis() <= _startTime+seconds*1000 )
            return true;
        else
        {
            _newCountDown = true;
            return false;
        }
    }

    /**
     * Speak the passed string using the speach synthesizer.
     * 
     * @param message The string to speak.
     */
    public static void speak( String message )
    {
        _voice.speak( message );
    }

}
