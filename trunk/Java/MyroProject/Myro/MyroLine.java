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
 * Class representing a line for display on a MyroCanvas.  
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
public class MyroLine extends MyroShape
{
    /**
     * Construct a line.  The color will initially be black and will not be visible. 
     * 
     * @param whichCanvas Specifies which MyroCanvas this circle will be drawn on
     * @param x1 The x coordinate of the first endpoint
     * @param y1 The y coordinate of the first endpoint
     * @param x2 The x coordinate of the second endpoint
     * @param y2 The y coordinate of the second endpoint
     */
    public MyroLine( MyroCanvas whichCanvas, int x1, int y1, int x2, int y2 )
    {
        // intialize MyroShape fields
        super ( whichCanvas );

        // initialize circle-specific fields
        this.x1 = x1;
        this.y1 = y1;
        this.x2 = x2;
        this.y2 = y2;
        
        // make the Shape
        shape = new Line2D.Double( x1, y1, x2, y2 );
    }

    /**
     * Set a new beginning endpoint of this line
     * 
     * @param newX The new x coordinate of the first endpoint of this line
     * @param newY The new y coordinate of the first endpoint of this line
     */
    public void setEndpoint1( int newX, int newY )
    {
        x1 = newX;
        y1 = newY;
        redrawLine();
    }

    /**
     * Set a new ending endpoint of this line
     * 
     * @param newX The new x coordinate of the second endpoint of this line
     * @param newY The new y coordinate of the second endpoint of this line
     */
    public void setEndpoint2( int newX, int newY )
    {
        x2 = newX;
        y2 = newY;
        redrawLine();
    }
    
    /**
     * Return the current x coordinate of the first endpoint of this line.
     * 
     * @return The x coordinate of the first endpoint of this line
     */
    public int getEndpoint1X()
    {
        return x1;
    }

    /**
     * Return the current y coordinate of the first endpoint of this line.
     * 
     * @return The y coordinate of the first endpoint of this line
     */
    public int getEndpoint1Y()
    {
        return y1;
    }

    /**
     * Return the current x coordinate of the second endpoint of this line.
     * 
     * @return The x coordinate of the second endpoint of this line
     */
    public int getEndpoint2X()
    {
        return x2;
    }

    /**
     * Return the current y coordinate of the second endpoint of this line.
     * 
     * @return The y coordinate of the second endpoint of this line
     */
    public int getEndpoint2Y()
    {
        return y2;
    }

    /**
     * Move this line by (deltaX, deltaY)
     * 
     * @param deltaX The amount to move this line in the x direction
     * @param deltaY The amount to move this line in the y direction
     */
    public void move( int deltaX, int deltaY )
    {
        x1 += deltaX;
        y1 += deltaY;
        x2 += deltaX;
        y2 += deltaY;
        redrawLine();
    }

    /**
     * Returns the x-coordinate of the point at the center of this line
     */
    public int getCenterX()
    {
        return (x1 + x2) / 2;
    }
    
    /**
     * Returns the y-coordinate of the point at the center of this line
     */
    public int getCenterY()
    {
        return (y1 + y2) / 2;
    }
    
    private void redrawLine()
    {
        shape = new Line2D.Double( x1, y1, x2, y2 );

        redrawShape();

    }

    // instance fields specific to a line
    private int x1, y1, x2, y2;

}
