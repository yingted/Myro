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
import java.awt.geom.*;

/**
 * Class representing a rectangle for display on a MyroCanvas.  The rectangle is initially an outline shape, but can
 * be changed to a filled shape by invoking the makeFilled method.
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
public class MyroRectangle extends MyroShape
{
    /**
     * Construct a rectangle.  Initially the rectangle will be an outline shape, but this can be changed by invoking
     * makeFilled method.  The intial outline color will be black and the fill color will be white.  The rectangle will
     * not be visible initially.
     * 
     * @pre width and height must be > 0
     * 
     * @param whichCanvas Specifies which MyroCanvas this circle will be drawn on
     * @param top The x coordinate of the upperleft corner
     * @param left The y coordinate of the upperleft corner
     * @param width The width of the rectangle
     * @param height The height of the rectangle
     */
    public MyroRectangle( MyroCanvas whichCanvas, int left, int top, int width, int height )
    {
        // intialize MyroShape fields
        super ( whichCanvas );

        assert width > 0:"width must be > 0";
        assert height > 0:"height must be > 0";
        
        // initialize circle-specific fields
        this.top = top;
        this.left = left;
        this.width = width;
        this.height = height;
        
        // make the Shape
        shape = new Rectangle2D.Double( left, top, width, height );
    }

    /**
     * Set a new upperleft corner for this rectangle
     * 
     * @param newX The new x coordinate of the upperleft corner of this rectangle
     * @param newY The new y coordinate of tthe upperleft corner of this rectangle
     */
    public void setCorner( int newX, int newY )
    {
        left = newX;
        top = newY;
        redrawRect();
    }

    /**
     * Set a new width for this rectangle
     * 
     * @pre newWidth > 0
     * 
     * @param newWidth The new width for this rectangle
     */
    public void setWidth( int newWidth )
    {
        assert newWidth > 0:"width must be > 0";
        
        width = newWidth;
        redrawRect();
    }

    /**
     * Set a new height for this rectangle
     * 
     * @pre newHeight > 0
     * 
     * @param newHeight The new height for this rectangle
     */
    public void setHeight( int newHeight )
    {
        assert newHeight > 0:"height must be > 0";
        
        height = newHeight;
        redrawRect();
    }
    
    /**
     * Move this rectangle by (deltaX, deltaY)
     * 
     * @param deltaX The amount to move this rectangle in the x direction
     * @param deltaY The amount to move this rectangle in the y direction
     */
    public void move( int deltaX, int deltaY )
    {
        left += deltaX;
        top += deltaY;
        redrawRect();
    }

    /**
     * Returns the x-coordinate of the point at the center of this rectangle
     */
    public int getCenterX()
    {
        return left + width/2;
    }
    
    /**
     * Returns the y-coordinate of the point at the center of this rectangle
     */
    public int getCenterY()
    {
        return top + height/2;
    }
    
    private void redrawRect()
    {
        shape = new Rectangle2D.Double( left, top,width, height );

        redrawShape();

    }

    // instance fields specific to a rectangle
    private int left, top, width, height;

}
