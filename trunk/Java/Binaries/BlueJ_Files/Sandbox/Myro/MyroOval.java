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
 * Class representing an oval for display on a MyroCanvas.  The oval is initially an outline shape, but can
 * be changed to a filled shape by invoking the makeFilled method.
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
public class MyroOval extends MyroShape
{
    /**
     * Construct an oval.  Initially the oval will be an outline shape, but this can be changed by invoking
     * makeFilled method.  The intial outline color will be black and the fill color will be white.  The oval will
     * not be visible initially.
     * 
     * @pre width and height must be > 0
     * 
     * @param whichCanvas Specifies which MyroCanvas this circle will be drawn on
     * @param top The x coordinate of the upperleft corner of a rectangle in which the oval is drawn
     * @param left The y coordinate of the upperleft corner of a rectangle in which the oval is drawn
     * @param width The width of the rectangle in which the oval is drawn
     * @param height The height of the rectangle in which the oval is drawn
     */
    public MyroOval( MyroCanvas whichCanvas, int left, int top, int width, int height )
    {
        // intialize MyroShape fields
        super ( whichCanvas );

        assert width>0:"width must be > 0";
        assert height>0:"height must be > 0";
        
        // initialize circle-specific fields
        this.top = top;
        this.left = left;
        this.width = width;
        this.height = height;

        // make the Shape
        shape = new Ellipse2D.Double( left, top, width, height );
    }

    /**
     * Set a new upperleft corner for the rectangle in which the oval is drawn
     * 
     * @param newX The new x coordinate of the upperleft corner of the rectangle in which the oval is drawn
     * @param newY The new y coordinate of tthe upperleft corner of the rectangle in which the oval is drawn
     */
    public void setCorner( int newX, int newY )
    {
        left = newX;
        top = newY;
        redrawOval();
    }

    /**
     * Set a new width for the rectangle in which the oval is drawn
     * 
     * @pre newWidth > 0
     * 
     * @param newWidth The new width for the rectangle in which the oval is drawn
     */
    public void setWidth( int newWidth )
    {
        assert newWidth > 0: "width must be > 0";
        
        width = newWidth;
        redrawOval();
    }

    /**
     * Set a new height for the rectangle in which the oval is drawn
     * 
     * @pre newHieght > 0
     * 
     * @param newHeight The new height for the rectangle in which the oval is drawn
     */
    public void setHeight( int newHeight )
    {
        assert newHeight > 0:"height must be > 0";
        
        height = newHeight;
        redrawOval();
    }


    /**
     * Returns the x-coordinate of the point at the center of this oval
     */
    public int getCenterX()
    {
        return left + width/2;
    }

    /**
     * Returns the y-coordinate of the point at the center of this oval
     */
    public int getCenterY()
    {
        return top + height/2;
    }

    /**
     * Move the oval by (deltaX, deltaY)
     * 
     * @param deltaX The amount to move this oval in the x direction
     * @param deltaY The amount to move this oval in the y direction
     */
    public void move( int deltaX, int deltaY )
    {
        left += deltaX;
        top += deltaY;
        redrawOval();
    }

    private void redrawOval()
    {
        shape = new Ellipse2D.Double( left, top,width, height );

        redrawShape();

    }

    // instance fields specific to an oval
    private int left, top, width, height;

}
