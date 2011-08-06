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
import java.awt.image.BufferedImage;
import java.awt.font.GlyphVector;

/**
 * Class representing a point for display on a MyroCanvas.  
 * 
 * @author Douglas Harms 
 * @version 1 August 2011
 */
public class MyroText extends MyroShape
{
    /**
     * Construct a textbox.  The color will initially be black and will not be visible. 
     * 
     * @param whichCanvas Specifies which MyroCanvas this point will be drawn on
     * @param x The x coordinate of the upperleft corner of the textbox
     * @param y The y coordinate of the upperleft corner of the textbox
     * @param str The text to display
     */
    public MyroText( MyroCanvas whichCanvas, int x, int y, String str )
    {
        // intialize MyroShape fields
        super ( whichCanvas );
        
        // store text-specific values
        this.x = x;
        this.y = y;
        this.str = str;
        this.fontFamily = "SansSerif";
        this.fontSize = 32;

        // make the Shape
        shape = generateShapeFromText( );
    }

    /**
     * Set a new upperleft coordinate for this text.
     * 
     * @param newX The new x coordinate
     * @param newY The new y coordinate
     */
    public void setCoordinate( int newX, int newY )
    {
        x = newX;
        y = newY;
        redrawText();
    }

    /**
     * Changes the text displayed.
     * 
     * @param newText The new text to be displayed
     */
    public void setText( String newText )
    {
        str = newText;
        redrawText();
    }
    
    /**
     * Set the font size of the text.
     * 
     * @pre 0 &lt; newSize
     * 
     * @param newSize The font size to use for this text
     */
    public void setFontSize( int newSize )
    {
        fontSize = newSize;
        redrawText();
    }
    
    /**
     * Set the font family of the text.  The acceptable families are Serif, SansSerif, and Monospaced.
     * 
     * @pre newFontFamily is "Serif", "SansSerif", or "Monospaced".
     * 
     * @param newFontFamily The name of the font family to use for this text
     */
    public void setFontFamily( String newFontFamily )
    {
        assert newFontFamily.equals( "Serif" ) ||
            newFontFamily.equals( "SansSerif" ) ||
            newFontFamily.equals( "Monospaced" ): "Illegal font family";
            
        fontFamily = newFontFamily;
        redrawText();
    }
    
    /**
     * Move the position of this text by (deltaX, deltaY)
     * 
     * @param deltaX The amount to move in the x direction
     * @param deltaY The amount to move in the y direction
     */
    public void move( int deltaX, int deltaY )
    {
        x += deltaX;
        y += deltaY;
        redrawText();
    }

    /**
     * Returns the current font size of this text.
     * 
     * @return The current font size of the text
     */
    public int getFontSize()
    {
        return fontSize;
    }
    
    /**
     * Returns the current font family of this text.
     * 
     * @return The current font family of this text.
     */
    public String getFontFamily()
    {
        return fontFamily;
    }
    
    /**
     * Returns the current text.
     * 
     * @return The current text
     */
    public String getText()
    {
        return str;
    }
    
    /**
     * Return the x-coordinate of the point at the center of this text.  In fact, the center point
     * is the center of the bounding rectangle of this text.
     */
    public int getCenterX()
    {
        Rectangle rect = shape.getBounds();
        return rect.x + rect.width/2;
    }
    
     /**
     * Return the y-coordinate of the point at the center of this text.  In fact, the center point
     * is the center of the bounding rectangle of this text.
     */
    public int getCenterY()
    {
        Rectangle rect = shape.getBounds();
        return rect.y + rect.height/2;
    }
    
    private void redrawText()
    {
        shape = generateShapeFromText();

        redrawShape();

    }

    private Shape generateShapeFromText( )
    {
        Font font = new Font( fontFamily, Font.PLAIN, fontSize );
        BufferedImage img = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = img.createGraphics();

        try {
            GlyphVector vect = font.createGlyphVector(g2.getFontRenderContext(), str );
            float dispX = x;
            float dispY = (float) (y - vect.getVisualBounds().getY() );
            Shape shape = vect.getOutline( dispX, dispY );

            return shape;
        } finally {
            g2.dispose();
        }
    }

    // instance fields specific to a text
    private int x, y;
    private String str;
    private int fontSize;
    private String fontFamily;

}
