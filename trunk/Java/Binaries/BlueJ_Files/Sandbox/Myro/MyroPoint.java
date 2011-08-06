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
 * Class representing a point for display on a MyroCanvas.  
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
public class MyroPoint extends MyroShape
{
    /**
     * Construct a point.  The color will initially be black and will not be visible. 
     * 
     * @param whichCanvas Specifies which MyroCanvas this point will be drawn on
     * @param x The x coordinate of the point
     * @param y The y coordinate of the point
     */
    public MyroPoint( MyroCanvas whichCanvas, int x, int y )
    {
        // intialize MyroShape fields
        super ( whichCanvas );

        // initialize point-specific fields
        this.x = x;
        this.y = y;
        
        // make the Shape
        shape = new Rectangle2D.Double( x, y, 1, 1 );
    }

    /**
     * Set a new coordinate of this point
     * 
     * @param newX The new x coordinate of the this point
     * @param newY The new y coordinate of the fthis point
     */
    public void setCoordinate( int newX, int newY )
    {
        x = newX;
        y = newY;
        redrawPoint();
    }

    /**
     * Return the current x coordinate of this point.
     * 
     * @return The x coordinate of this point
     */
    public int getX()
    {
        return x;
    }

    /**
     * Return the current y coordinate of this point.
     * 
     * @return The y coordinate of this point
     */
    public int getY()
    {
        return y;
    }

    /**
     * Move this point by (deltaX, deltaY)
     * 
     * @param deltaX The amount to move this point in the x direction
     * @param deltaY The amount to move this point in the y direction
     */
    public void move( int deltaX, int deltaY )
    {
        x += deltaX;
        y += deltaY;
        redrawPoint();
    }

    /**
     * Returns the x-coordinate of the point at the center
     */
    public int getCenterX()
    {
        return x;
    }
    
    /**
     * Returns the y-coordinate of the point at the center
     */
    public int getCenterY()
    {
        return y;
    }
    
    private void redrawPoint()
    {
        shape = new Rectangle2D.Double( x, y, 1, 1 );

        redrawShape();

    }

    // instance fields specific to a line
    private int x, y;

}
