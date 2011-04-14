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

    // static constructor
    static
    {
        _randomSeq = new Random();
        _newCountDown = true;
    }

    /**
     * Cause the current thread to sleep for numSeconds.
     * <p><p>
     * <b>Precondition:</b> numSeconds >= 0.0
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
     * <p><p>
     * <b>Precondition:</b> low <= high
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
     * 
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

}
