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
 * Information about a blob image, returned by robot.getBlob().
 * 
 * @author Douglas Harms
 * @version September 2010
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

    /**
     * Returns the number of pixels that were in the blob.
     * 
     * @return The number of pixels in the blob
     */
    public int getPixelCount()
    {
        return pixelCount;
    }

    /**
     * Returns the average x-coordinate of the pixels that were in the blob.
     * 
     * @return The average x-coordinate of the pixels in the blob
     */
    public int getAverageX()
    {
        return averageX;
    }

    /**
     * Returns the average y-coordinate of the pixels that were in the blob.
     * 
     * @return The average y-coordinate of the pixels in the blob
     */
    public int getAverageY()
    {
        return averageY;
    }

}
