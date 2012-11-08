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

import javax.swing.*;
import java.awt.*;
import java.util.List;
import java.util.LinkedList;

/**
 * Instances of MyroCanvas provide Myro/Java programs a canvas on which to draw MyroShape objects.  If the canvas
 * is visible, a window appears on the screen.
 * 
 * @author Douglas Harms
 * @version August 2011
 */
public class MyroCanvas
{
    /**
     * Construct a new MyroCanvas with a specified title and size.  The background color will be white and the 
     * canvas will initially be visible.
     * 
     * @pre 0 &lt; width and 0 &lt; height
     */
    public MyroCanvas( String title, int width, int height )
    {
        // It seems that Windows has problems making the first JFrame the active frame.
        // A solution I found is to create a dummy JFrame first, make it visible, create
        // the "real" MyroCanvas frame, make it visible, then hide the dummy.  Also, we
        // need to yield the thread.  It's dark magic, but it seems to work.  Wish I
        // understood why, though.
        JFrame dummy = new JFrame();
        dummy.setVisible( true );

        // create frame and all necessary components
        frame = new JFrame( title );
        canvas = new CanvasPane();
        frame.setContentPane( canvas );
        canvas.setPreferredSize( new Dimension( width, height ) );
        frame.pack();

        // get the graphics context and set the canvas to background color
        Dimension actualSize = canvas.getSize();
        canvasImage = canvas.createImage( actualSize.width, actualSize.height );
        graphic = (Graphics2D)canvasImage.getGraphics();
        graphic.setColor( Color.white );
        graphic.fillRect( 0, 0, actualSize.width, actualSize.height );

        // initialize other structures
        shapes = new LinkedList<ShapeDescription>();
        backgroundColor = Color.white;
        autoRepaint = true;

        // User can detect key/mouse events
        frame.addKeyListener( MyroListener.getKeyListener() );
        frame.addMouseListener( MyroListener.getMouseListener() );
        MyroListener.flushKeys();

        // make the new frame visible
        frame.setVisible( true );

        // now yield the thread, then hide the dummy JFrame.  All part of
        // dark magic.
        Thread.yield();
        dummy.setVisible( false );
    }

    /**
     * Construct a default 200x200 white canvas titled "Myro Canvas".
     */
    public MyroCanvas()
    {
        this( "Myro Canvas", 200, 200 );
    }

    /**
     * Returns the width of this canvas
     * 
     * @return The width of this canvas
     */
    public int getWidth()
    {
        return canvas.getSize().width;
    }

    /**
     * Returns the height of this canvas
     * 
     * @return The height of this canvas
     */
    public int getHeight()
    {
        return canvas.getSize().height;
    }

    /**
     * Set the baackground color of this canvas.
     * 
     * @param newColor The new background color of this canvas
     */
    public void setBackgroundColor( Color newColor )
    {
        backgroundColor = newColor;
        repaint();
    }

    /**
     * Returns the current background color of this canvas
     */
    public Color getBackgroundColor()
    {
        return backgroundColor;
    }

    /**
     * Makes the canvas visible or invisible
     * 
     * @param isVisible true will make the window appear, false will make it disappear
     */
    public void setVisible( boolean isVisible )
    {
        frame.setVisible( isVisible );
        if( isVisible )
            repaint();
    }

    /**
     * Sets whether or not to automatically repaint the canvas everytime an object changes.  If this is set
     * to false, then the user should invoke repaint() in order to have all changes appear on the canvas.
     */
    public void setAutoRepaint( boolean autoRepaint )
    {
        this.autoRepaint = autoRepaint;

        if( autoRepaint )
            repaint();
    }

    /**
     * repaint all objects on this canvas.  Users will only need to call this if they've disabled autoRepaint and
     * want to repaint all objects on the canvas.
     */
    public void repaint()
    {
        // erase the canvas
        graphic.setColor( backgroundColor );
        graphic.fillRect( 0, 0, getWidth(), getHeight() );

        // now draw all of the shpaes on this canvas
        for( ShapeDescription s : shapes )
        {
            s.draw();
        }

        // display everything now
        canvas.repaint();
    }

    /**
     * Add a new shape to this canvas.  Users should not invoke this method.
     * 
     */
    public Object addShape( Shape shape, boolean filled, Color fillColor, Color outlineColor, int outlineWidth )
    {
        // create a new ShapeDescription and add it to shapes
        ShapeDescription s = new ShapeDescription( shape, filled, fillColor, outlineColor, outlineWidth );
        shapes.add( s );

        // display this new shape (and everything else) if autoRepaint is enabled
        if( autoRepaint )
            repaint();

        // return the new ShapeDescription.  This will be passed to removeShape if the shape ever
        // needs to be removed from this canvas.
        return s;
    }

    /**
     * Remove an object from this canvas.  Users should not invoke this method.
     */
    public void removeShape( Object o )
    {
        // o MUST have been returned by a previous invocation of addShape,  cast it to a ShapeDescription.
        ShapeDescription s = (ShapeDescription)o;

        // remove it from the shapes in this canvas
        shapes.remove( s );

        // redraw the canvas to reflect this change if autoRepaint enables
        if( autoRepaint )
            repaint();
    }

    /**
     * Replace a shape with a new one.  Users should not invoke this method.
     */
    public Object replaceShape( Object oldShape, Shape shape, boolean filled, Color fillColor,
    Color outlineColor, int outlineWidth )
    {
        //oldShape MUST have been returned by a previous involcation of addShape or replaceShape.  cast it to
        // a ShapeDescription
        ShapeDescription s = (ShapeDescription)oldShape;

        // remove it from the shapeds in this canvas
        shapes.remove( s );

        // add the new shape and return the object
        return addShape( shape, filled, fillColor, outlineColor, outlineWidth );
    }

    // instance fields
    private JFrame frame;
    private CanvasPane canvas;
    private Graphics2D graphic;
    private Image canvasImage;
    private Color backgroundColor;
    private List<ShapeDescription> shapes;
    private boolean autoRepaint;

    /**
     * private class defining the content panel of a MyroCanvas
     */
    private class CanvasPane extends JPanel
    {
        public void paint(Graphics g)
        {
            g.drawImage(canvasImage, 0, 0, null);
        }
    }

    /**
     * private class defining a shape on this MyroCanvas
     */
    private class ShapeDescription
    {
        public ShapeDescription( Shape s, boolean isFilled, Color fillColor, Color outlineColor, int outlineWidth )
        {
            shape = s;
            this.fillColor = fillColor;
            this.outlineColor = outlineColor;
            this.isFilled = isFilled;
            this.outlineStroke = new BasicStroke( outlineWidth );
        }

        public void draw()
        {
            if( isFilled )
            {
                graphic.setColor( fillColor );
                graphic.fill( shape );
            }

            graphic.setColor( outlineColor );
            graphic.setStroke( outlineStroke );
            graphic.draw( shape );
        }

        private Color fillColor;
        private Color outlineColor;
        private Shape shape;
        private boolean isFilled;
        private Stroke outlineStroke;
    }
}
