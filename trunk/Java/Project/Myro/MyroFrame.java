package Myro;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 * This is a JFrame that has an associated MyroImage.
 */
public class MyroFrame extends JFrame
{
    private boolean isVisible;
    private MyroImage currentImage;
    private JLabel imageStatusLine;
    private JPanel imagePane;
    private boolean definingBlob;
    private int blobx1, bloby1, blobx2, bloby2;

    /**
     * Constructs a JFrame with a specified name, location, and associated MyroImage.
     * 
     * @param frameName Name of this frame
     * @param iamge MyroImage initially associated with this MyroFrame
     * @param x x coordinate of the initial position of this MyroFramee
     * @param y y coordinate of the initial position of this MyroFrame
     */
    public MyroFrame( String frameName, MyroImage image, int x, int y )
    {
        super( frameName );

        // set up the new frame
        imagePane = new imagePanel();
        imagePane.setPreferredSize( new Dimension( image.width(), image.height() ) );

        add( imagePane, BorderLayout.CENTER );            
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        setResizable(false);

        imageStatusLine = _makeLabel( " " );
        add( imageStatusLine, BorderLayout.SOUTH );

        imageMouseEventHandler mouseEvent = new imageMouseEventHandler();
        imagePane.addMouseMotionListener( mouseEvent );
        imagePane.addMouseListener( mouseEvent );

        pack();

        setLocation( x, y );
        setVisible( false );
        isVisible = false;
        currentImage = image;
        definingBlob = false;
    }

    /**
     * Returns the MyroImage currently associated with this MyroFrame
     */
    public MyroImage getCurrentImage()
    {
        return currentImage;
    }

    /**
     * Sets the MyroImage that is associated with this MyroFrame
     */
    public void setImage( MyroImage image )
    {
        currentImage = image;
    }

    /**
     * Makes the MyroFrame visible on the screen.  If the MyroFrame was already visible, nothing
     * changes.
     */
    public void makeVisible()
    {
        setVisible( true );
        repaint();
        isVisible = true;
    }

    /**
     * Makes the MyroFrame invisible.  If the MyroFrame was already invisible, nothing changes.
     */
    public void makeInvisible()
    {
        setVisible( false );
        isVisible = false;
    }

    /**
     * Permits the user to select a rectangle in the MyroFrame.
     * 
     * @param prompt A String that will be displayed at the bottom of the MyroFrame
     * @return A 4-element array of ints containing [xLow, yLow, Width, Height] of the selected rectangle
     */
    public int[] getUserRect( String prompt )
    {
        // go into blob definition mode.  Until the mouse is pressed the defined rectangle is empty.
        definingBlob = true;
        blobx1 = bloby1 = 0;
        blobx2 = bloby2 = 0;
        imageStatusLine.setText( prompt );
        while( definingBlob )
            Thread.yield();

        imageStatusLine.setText( " " );

        // determine upperleft corner and width,height of the selected rectangle
        int xlow = Math.min( blobx1, blobx2 );
        int xhigh = Math.max( blobx1, blobx2 );
        int ylow = Math.min( bloby1, bloby2 );
        int yhigh = Math.max( bloby1, bloby2 );
        int blobWidth = xhigh - xlow + 1;
        int blobHeight = yhigh - ylow + 1;
        
        // pass values back to caller
        int[] vals = new int[4];
        vals[0] = xlow;
        vals[1] = ylow;
        vals[2] = blobWidth;
        vals[3] = blobHeight;
        
        return vals;
    }
    
    /**
     * Class that handles mouse events for the image window.
     */
    private class imageMouseEventHandler implements MouseListener, MouseMotionListener
    {
        /**
         * When the mouse moves over the image display the pixel location and RGB color of the pixel.  Nothing
         * happens if the user is defining a blob, though.
         */
        public void mouseMoved( MouseEvent e )
        {
            // don't do anything if the user is in the process of defining a blob
            if( definingBlob )
                return;

            // get the position of the mouse and the color of that pixel
            int x = e.getX();
            int y = e.getY();
            Color c = currentImage.get( x, y );
            int r = c.getRed();
            int g = c.getGreen();
            int b = c.getBlue();

            // display the location and color in the status line underneath the image.
            imageStatusLine.setText( "("+x+", "+y+"): ( r="+r+", g="+g+", b="+b+" )"  );
        }

        /**
         * Clear the status line when the mouse exits the window and the user is not defining a blob
         */
        public void mouseExited( MouseEvent e )
        {
            if( !definingBlob )
                imageStatusLine.setText( " " );
        }

        /**
         * If the user is not defining a blob then pressing the mouse is the same as moving it.  If the user
         * is defining a blob then the press event specifies the starting corner of the blob rectangle.
         */
        public void mousePressed( MouseEvent e )
        {
            if( !definingBlob )
                mouseMoved( e );
            else
            {
                blobx1 = blobx2 = e.getX();
                bloby1 = bloby2 = e.getY();
            }
        }

        /**
         * If the user is not defining a blob, then do nothing.  If s/he is defining a blob then the current
         * mouse position defines the other corner of the blob rectangle.  Remember this and repaint the window
         * so the rectangle is visible.
         */
        public void mouseDragged( MouseEvent e )
        {
            if( definingBlob )
            {
                blobx2 = Math.max( Math.min( e.getX(), currentImage.width()-1 ), 0 );
                bloby2 = Math.max( Math.min( e.getY(), currentImage.height()-1 ), 0 );
                repaint();
            }
        }

        /**
         * If the user is not defining a blob then do nothing.  Otherwise the mouse position specifies the
         * other corner of the blob rectangle.  Remember this position and indicate that we're not in blob
         * definition mode any longer.
         */
        public void mouseReleased( MouseEvent e )
        {
            if( definingBlob )
            {
                blobx2 = Math.max( Math.min( e.getX(), currentImage.width()-1 ), 0 );
                bloby2 = Math.max( Math.min( e.getY(), currentImage.height()-1 ), 0 );
                definingBlob = false;
                repaint();
            }
        }

        // methods that must be defined for interface MouseListener
        public void mouseClicked( MouseEvent e )
        {}

        public void mouseEntered( MouseEvent e )
        {}

    }

    /**
     * Class that defines the image portion of the window.
     */
    private class imagePanel extends JPanel
    {
        /**
         * Method to paint the contents of the image.  Always display the image, and if the user is defining
         * a blob then also draw a rectangle showing the current blob rectangle.
         */
        public void paintComponent( Graphics g )
        {
            Graphics2D g2 = (Graphics2D)g;

            // draw the image
            g2.drawImage( currentImage.getImage(), 0, 0,
                currentImage.width(), currentImage.height(), this );

            // if the user is in blobdefintion mode then draw the currently defined rectangle
            if( definingBlob )
            {
                // deterine upperleft corner and width,height of the currently defined rectangle
                int xlow = Math.min( blobx1, blobx2 );
                int xhigh = Math.max( blobx1, blobx2 );
                int ylow = Math.min( bloby1, bloby2 );
                int yhigh = Math.max( bloby1, bloby2 );
                int blobWidth = xhigh - xlow + 1;
                int blobHeight = yhigh - ylow + 1;

                // draw the rectangle
                g2.setColor( Color.BLACK );
                g2.drawRect( xlow, ylow, blobWidth, blobHeight );
            }
        }
    }

    /**
     * Returns a JLabel containing the passed String.
     */
    private JLabel _makeLabel(String caption)
    {
        JLabel label = new JLabel();
        //label.setPreferredSize(new Dimension(100, 20));
        label.setText(caption);
        label.setHorizontalAlignment(SwingConstants.LEFT);

        return label;
    }

}