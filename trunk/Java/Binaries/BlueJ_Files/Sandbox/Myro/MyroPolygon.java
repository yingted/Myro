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
import java.util.ArrayList;

/**
 * Class representing a point for display on a MyroCanvas.  
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
public class MyroPolygon extends MyroShape
{
    /**
     * Construct a 0-sided polygon.  The color will initially be black and will not be visible. 
     * 
     * @param whichCanvas Specifies which MyroCanvas this polygon will be drawn on
     */
    public MyroPolygon( MyroCanvas whichCanvas )
    {
        // intialize MyroShape fields
        super ( whichCanvas );

        // initialize circle-specific fields
        
        // make the Shape
        shape = new Polygon();
    }

    /**
     * Adds a new vertex to this polygon.  The new vertex will be placed at the "end"
     * of the polygon.  An edge will connect this vertex to the 0th vertex, and an edge will connect this vertex
     * to the previous vertex (i.e., the vertex that was last before this invocation).
     */
    public void addVertex( int x, int y )
    {
        Polygon p = (Polygon)shape;
        
        p.addPoint( x, y );
        redrawPoly();
    }
    
    /**
     * Return the current x coordinate of the nth vertex in the polygon.
     * 
     * @pre 0 &lt;= n &lt; numVertices
     * 
     * @param n The vertex in the polygon
     * @return The x coordinate of the nth vertex
     */
    public int getX( int n ) 
    {
        Polygon p = (Polygon)shape;
        
        assert 0 <= n && n < p.npoints:"n must be between 0 and numVertices";
        
        return p.xpoints[n];
    }

    /**
     * Return the current y coordinate of the nth vertex in the polygon.
     * 
     * @pre 0 &lt;= n &lt; numVertices
     * 
     * @param n The vertex in the polygon
     * @return The y coordinate of the nth vertexoint
     */
    public int getY( int n ) 
    {
        Polygon p = (Polygon)shape;
        
        assert 0 <= n && n < p.npoints:"n must be >=0 0 and < numVertices";
        
        return p.ypoints[n];
    }

    /**
     * Returns the number of vertices in this polygon
     * 
     * @return The number of vertices that define this polygon
     */
    public int getNumVertices()
    {
        Polygon p = (Polygon)shape;
        
        return p.npoints;
    }

    /**
     * Move this polygon by (deltaX, deltaY)
     * 
     * @param deltaX The amount to move this point in the x direction
     * @param deltaY The amount to move this point in the y direction
     */
    public void move( int deltaX, int deltaY )
    {
        Polygon p = (Polygon)shape;
        
        p.translate( deltaX, deltaY );
        redrawPoly();
    }

    /**
     * Return the x-coordinate of the point at the center of this polygon.  In fact, the center point
     * is the center of the bounding rectangle of this polygon.
     */
    public int getCenterX()
    {
        Rectangle rect = shape.getBounds();
        return rect.x + rect.width/2;
    }
    
     /**
     * Return the y-coordinate of the point at the center of this polygon.  In fact, the center point
     * is the center of the bounding rectangle of this polygon.
     */
    public int getCenterY()
    {
        Rectangle rect = shape.getBounds();
        return rect.y + rect.height/2;
    }
    
   private void redrawPoly()
    {
        redrawShape();
    }

    // instance fields specific to a polygon
}
