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
 * Class representing a circle for display on a MyroCanvas.  The circle is initially an outline shape, but can
 * be changed to a filled shape by invoking the makeFilled method.
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
public class MyroCircle extends MyroShape
{
    /**
     * Construct a circle.  Initially the circle will be an outline shape, but this can be changed by invoking
     * makeFilled method.  The intial outline color will be black and the fill color will be white.  The circle will
     * not be visible initially.
     * 
     * @pre radius > 0
     * 
     * @param whichCanvas Specifies which MyroCanvas this circle will be drawn on
     * @param centerX Initial x coordiante of the center of this circle
     * @param centerY Initial y coordinate of the center of this circle
     * @param radius Initial radius of this circle.
     */
    public MyroCircle( MyroCanvas whichCanvas, int centerX, int centerY, int radius )
    {
        // intialize MyroShape fields
        super ( whichCanvas );

        assert radius > 0: "radius must be > 0";

        // initialize circle-specific fields
        x = centerX;
        y = centerY;
        r = radius;

        // make the Shape
        int left = centerX - radius;
        int top = centerY - radius;
        int diameter = 2 * radius;
        shape = new Ellipse2D.Double( left, top, diameter, diameter );
    }

    /**
     * Set a new center for this circle
     * 
     * @param newX The new x coordinate of this circel
     * @param newY The new y coordinate of this circle
     */
    public void setCenter( int newX, int newY )
    {
        x = newX;
        y = newY;
        redrawCircle();
    }

    /**
     * Set a new radius for this circle
     * 
     * @pre newRadius > 0
     * 
     * @param newRadius The new radius for this circle
     */
    public void setRadius( int newRadius )
    {
        assert newRadius > 0:"radius must be > 0";
        
        r = newRadius;
        redrawCircle();
    }

    /**
     * Return the current radius of this circle.
     * 
     * @return The current radius of this crcle
     */
    public int getRadius()
    {
        return r;
    }

    /**
     * Move this circle by (deltaX, deltaY)
     * 
     * @param deltaX The amount to move this dicrle in the x direction
     * @param deltaY The amount to move this circle in the y direction
     */
    public void move( int deltaX, int deltaY )
    {
        x += deltaX;
        y += deltaY;
        redrawCircle();
    }

    /**
     * Returns the x-coordinate of the center of this circle.
     */
    public int getCenterX()
    {
        return x;
    }
    
    /**
     * Returns the y-coordinate of the center of this circle.
     */
    public int getCenterY()
    {
        return y;
    }
    
    private void redrawCircle()
    {
        int left = x - r;
        int top = y - r;
        int diameter = 2 * r;
        shape = new Ellipse2D.Double( left, top, diameter, diameter );

        redrawShape();

    }

    // instance fields specific to a circle
    private int x, y;
    private int r;

}
