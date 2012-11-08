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
 * Classes derived from MyroShape will be shapes drawable on a MyroCanvas.
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
abstract public class MyroShape
{
    protected MyroShape( MyroCanvas whichCanvas )
    {
        canvas = whichCanvas;        
        isVisible = false;
        isFilled = false;
        outlineColor = Color.BLACK;
        fillColor = Color.WHITE;
        width = 1;
    }

    /**
     * Make this shape visible
     */
    public void visible()
    {
        if( !isVisible() )
        {
            isVisible = true;
            canvasObject = canvas.addShape( shape, isFilled, fillColor, outlineColor, width );
        }
    }

    /**
     * Make this shape invisible
     */
    public void invisible()
    {
        if( isVisible() )
        {
            isVisible = false;
            canvas.removeShape( canvasObject );
            canvasObject = null;
        }
    }

    /**
     * Returns whether this shape is currently visible or not.
     * 
     * @return true iff the shape is currently visible
     */
    public boolean isVisible()
    {
        return isVisible;
    }

    /**
     * Make this shape an outline shape
     */
    public void makeOutline()
    {
        isFilled = false;
        redrawShape();
    }

    /**
     * Make this shape a filled shape
     */
    public void makeFilled()
    {
        isFilled = true;
        redrawShape();
    }

    /**
     * Returns whether this shape is filled or not (i.e., whether it is a filled or outline shape)
     * 
     * @return true if this shape is a filled shape, false if it is an outline shape
     */
    public boolean isFilled()
    {
        return isFilled;
    }

    /**
     * Sets the fill color of this shape
     */
    public void setFillColor( Color newColor )
    {
        fillColor = newColor;
        redrawShape();
    }

    /**
     * Sets the outline color of this shape
     */
    public void setOutlineColor( Color newColor )
    {
        outlineColor = newColor;
        redrawShape();
    }

    /**
     * Sets the width of the outline
     */
    public void setOutlineWidth( int newWidth )
    {
        width = newWidth;
        redrawShape();
    }

    /**
     * Returns the current fill color of this shape
     * 
     * @return The current fill color of this shape
     */
    public Color getFillColor()
    {
        return fillColor;
    }

    /**
     * Returns the current outline color of this shape.
     * 
     * @return The current outline color of this shape
     */
    public Color getOutlineColor()
    {
        return outlineColor;
    }

    /**
     * Returns the current outline width of this shape
     * 
     * @return The current outline width of this shape
     */
    public int getOutlineWidth()
    {
        return width;
    }

    /**
     * Returns the left edge of the bounding rectangle of this object
     * 
     * @return The left edge of this object
     */
    public int getLeft()
    {
        Rectangle rect = shape.getBounds();

        return rect.x;
    }

    /**
     * Returns the right edge of the bounding rectangle of this object
     * 
     * @return The right edge of this object
     */
    public int getRight()
    {
        Rectangle rect = shape.getBounds();

        return rect.x + rect.width;
    }

    /**
     * Returns the top edge of the bounding rectangle of this object
     * 
     * @return The top edge of this object
     */
    public int getTop()
    {
        Rectangle rect = shape.getBounds();

        return rect.y;
    }

    /**
     * Returns the bottom edge of the bounding rectangle of this object
     * 
     * @return The bottom edge of this object
     */
    public int getBottom()
    {
        Rectangle rect = shape.getBounds();

        return rect.y + rect.height;
    }

    /**
     * Returns the height of the bounding rectangle of this object
     * 
     * @return The height of this object
     */
    public int getHeight()
    {
        Rectangle rect = shape.getBounds();

        return rect.height;
    }

    /**
     * Returns the widthe of the bounding rectangle of this object
     * 
     * @return The width of this object
     */
    public int getWidth()
    {
        Rectangle rect = shape.getBounds();

        return rect.width;
    }

    /**
     * Move the shape by (deltaX, deltaY)
     */
    abstract public void move( int deltaX, int deltaY );

    /**
     * Returns the x-coordinate of the point at the center of this object
     */
    abstract public int getCenterX();

    /**
     * Returns the y-coordinate of the point at the center of this object
     */
    abstract public int getCenterY();

    /**
     * replace the shape on the canvas.  This method should not be invoked by users.  Derived classes should
     * invoke this anytime the shape changes.
     */
    protected void redrawShape()
    {
        if( isVisible )
            canvasObject = canvas.replaceShape( canvasObject, shape, isFilled, fillColor, outlineColor, width );
    }

    // instance fields
    protected MyroCanvas canvas;
    protected Shape shape;
    protected Object canvasObject;

    protected boolean isVisible;
    protected Color fillColor;
    protected Color outlineColor;
    protected boolean isFilled;
    protected int width;
}
