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

import java.awt.*;

/**
 * An instance of this class references a pixel in a MyroImage.  Modifying the MyroPixel will automatically modify
 * the associated pixel in the image.
 * 
 * @author Douglas Harms 
 * @version 1.0 - 30 July 2011
 */
public class MyroPixel
{
    /**
     * Define a MyroPixel attached to pixel (x,y) in the image.
     * 
     * @pre (x,y) is a valid pixel in the image.
     * 
     * @param image The MyroImage this MyroPixel will be associated with
     * @param x The x cooredinate of the pixel in imave
     * @param y The y coordinate of the pixel in image
     */
    public MyroPixel( MyroImage image, int x, int y )
    {
        _image = image;
        _x = x;
        _y = y;
    }
    
    /**
     * Returns the x coordinate of this pixel
     * 
     * @return The x coordinate of this pixel
     */
    public int getX()
    {
        return _x;
    }
    
    /**
     * Returns the y coordinate of this pixel
     * 
     * @return The y coordinate of this pixel
     */
    public int getY()
    {
        return _y;
    }
    
    /**
     * Returns the color of this pixel
     * 
     * @return The color of this pixel
     */
    public Color getColor()
    {
        return _image.getColor( _x, _y );
    }
    
    /**
     * Sets the color of this pixel
     * 
     * @param newColor The color that this pixel will be set to
     */
    public void setColor( Color newColor )
    {
        _image.setColor( _x, _y, newColor );
    }
    
    /**
     * Returns the grayscale intensity of this pixel
     * 
     * @return The grayscale intensity of this pixel
     */
    public int getGray()
    {
        return _image.getGray( _x, _y );
    }
    
    /**
     * Sets the grayscale intensity of this pixel
     * 
     * @pre 0 &lt;= grayLevel and grayLevel &lt;= 255
     * 
     * @param grayLevel The grayscale value that this pixel will be set to
     */
    public void setGray( int grayLevel )
    {
        _image.setGray( _x, _y, grayLevel );
    }
    
    /**
     * Returns the MyroImage this pixel is associated with.
     * 
     * @return The MyroImage that this pixel is associated with this pixel
     */
    public MyroImage getImage()
    {
        return _image;
    }
    
    private MyroImage _image;
    private int _x;
    private int _y;
}
