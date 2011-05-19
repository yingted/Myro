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

/**
 * Write a description of class MyroBlob here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import java.awt.*;

public class MyroBlobSpec
{
    public int y_low, y_high;
    public int u_low, u_high;
    public int v_low, v_high;
    public int threshold;

    public MyroBlobSpec( int yl, int yh, int ul, int uh, int vl, int vh, int t )
    {
        y_low = yl;
        y_high = yh;
        u_low = ul;
        u_high = uh;
        v_low = vl;
        v_high = vh;
        threshold = t;
    }
    
    public MyroBlobSpec ()
    {
        y_low = 0;
        y_high = 254;
        u_low = 51;
        u_high = 136;
        v_low = 190;
        v_high = 254;
        threshold = 4;
    }

}
