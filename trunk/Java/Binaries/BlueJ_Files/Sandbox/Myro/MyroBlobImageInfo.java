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
 * Write a description of class MyroBlobImageInfo here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class MyroBlobImageInfo
{
    private int pixelCount;
    private int averageX;
    private int averageY;

    public MyroBlobImageInfo( int count, int avgX, int avgY )
    {
        pixelCount = count;
        averageX = avgX;
        averageY = avgY;
    }

    public int getPixelCount()
    {
        return pixelCount;
    }

    public int getAverageX()
    {
        return averageX;
    }

    public int getAverageY()
    {
        return averageY;
    }

}
