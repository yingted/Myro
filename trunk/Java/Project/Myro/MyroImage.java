package Myro;

import java.io.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;
import javax.imageio.*;

/**
 * Abstract class MyroImage
 * 
 * @author Douglas Harms
 * @version 1.0 - September 2010
 */
public abstract class MyroImage
{
    protected BufferedImage image;               // the rasterized image
    protected JFrame frame;                      // on-screen view
    protected int width, height;                 // width and height
    protected int imageType;
    private JLabel imageStatusLine;
    private JPanel imagePane;
    private boolean definingBlob;
    private int blobx1, bloby1, blobx2, bloby2;
    private boolean isVisible;

    /**
     * Constant returned by {@link #getType getType} indicating that this image is a color image
     */
    public static int MYRO_IMAGE_COLOR = 0;

    /**
     * Constant returned by {@link #getType getType} indicating that this image is a gray scale image
     */
    public static int MYRO_IMAGE_GRAY = 1;

    /**
     * Default constructor, only callable by subclass constructor.
     */
    protected MyroImage()
    {
        definingBlob = false;
        isVisible = false;
    }

    //     public JLabel getJLabel()
    //     {
    //         if (image == null) { return null; }         // no image available
    //         ImageIcon icon = new ImageIcon( image );
    //         return new JLabel( icon );
    //     }

    /**
     * Causes this image to be visible in a window located at a specified location on the screen.  The parameters
     * specify the location of the upperleft corner of the window.
     * 
     * @param x x coordinate of the upperleft corner of the window
     * @param y y coordinate of the upperleft corner of the window
     */
    public void show( int x, int y )
    {
        // create the GUI for viewing the image if needed
        if ( frame == null )
        {
            frame = new JFrame();
            //frame.setContentPane(getJLabel());
            //imagePane = getJLabel();
            imagePane = new imagePanel();
            imagePane.setPreferredSize( new Dimension( width, height ) );
            frame.add( imagePane, BorderLayout.CENTER );            
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            //frame.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
            frame.setResizable(false);

            imageStatusLine = _makeLabel( " " );
            frame.add( imageStatusLine, BorderLayout.SOUTH );

            imageMouseEventHandler mouseEvent = new imageMouseEventHandler();
            imagePane.addMouseMotionListener( mouseEvent );
            imagePane.addMouseListener( mouseEvent );

            frame.pack();
            //frame.setVisible(true);
        }

        // set location of the window and draw it
        frame.setLocation( x, y );
        frame.setVisible( true );
        frame.repaint();
        isVisible = true;
    }

    /**
     * Causes this image to be visible in a window.  If the image had not been displayed previously a window will
     * be created at the upperleft corner of the screen.
     */
    public void show()
    {
        if( frame == null )
            show( 0, 0 );
        else
        {
            frame.setVisible( true );
            frame.repaint();
            isVisible = true;
        }
    }

    /**
     * Causes this image to be invisible (i.e., the window disappears from the screen).  If this image was already
     * invisible, no changes are made to any window.
     */
    public void hide()
    {
        if( frame != null )
        {
            frame.setVisible( false );
            //frame.repaint();
        }

        isVisible = false;
    }

    /**
     * Changes made to the image (e.g., by calling {@link #set set}) are reflected in the displayed window.  
     * If the window is invisible no changes are made to any window.
     */
    public void repaint()
    {
        if( frame != null )
        {
            frame.repaint();
        }
    }

    /**
     * Changes this image to be a copy of the one passed as a parameter.  If the image is visible, then the
     * image is redisplayed to reflect the new image.
     */
    public void setImage( MyroImage newImage )
    {
        // copy instance fields from newImage
        image = newImage.image;
        height = newImage.height;
        width = newImage.width;
        imageType = newImage.imageType;

        // update the frame if there is one
        if( frame != null )
        {
            imagePane.setPreferredSize( new Dimension( width, height ) );
            frame.pack();
        }

        // repaint the frame
        repaint();
    }

    /**
     * Returns the height of the image.
     */
    public int height() {
        return height;
    }

    /**
     * Returns the width of the image.
     */
    public int width() {
        return width;
    }

    /**
     * Returns the type (i.e., Color or Grayscale) of this image.
     * 
     * @return Value MYRO_IMAGE_COLOR or MYRO_IMAGE_GRAY.
     */
    public int getType()
    {
        return imageType;
    }

    /**
     * Allows the user to select a rectangular area of the image used to define a blob.  The image is first
     * made visible, then a message at the bottom of the window instructs the user to drag an area to define
     * a blob.  The blob returned contains the average color of the selected area and can be passed to the
     * scribbler configureBlob method.
     * 
     * @return null if the user selected a zero-size rectangle; otherwise a blob that can be passed to a scribbler
     * configureBlob method.
     */
    public MyroBlobSpec getUserDefinedBlob()
    {
        // make sure the image is visible
        show();

        // go into blob definition mode.  Until the mouse is pressed the defined rectangle is empty.
        definingBlob = true;
        blobx1 = bloby1 = 0;
        blobx2 = bloby2 = 0;
        imageStatusLine.setText( "Drag a rectangle in the image to define a blob" );
        while( definingBlob )
            Thread.yield();

        imageStatusLine.setText( " " );

        // deterine upperleft corner and width,height of the selected rectangle
        int xlow = Math.min( blobx1, blobx2 );
        int xhigh = Math.max( blobx1, blobx2 );
        int ylow = Math.min( bloby1, bloby2 );
        int yhigh = Math.max( bloby1, bloby2 );
        int blobWidth = xhigh - xlow + 1;
        int blobHeight = yhigh - ylow + 1;

        // if either the width or height is 0, then nothing selected so return null, otherwise return
        // a blob defined by the rectangle
        if( blobWidth==0 || blobHeight==0 )
            return null;
        else
            return defineBlob( xlow, ylow, blobWidth, blobHeight );
    }

    /**
     * Calculate a blob based on a rectangular area of the image.  The specified rectangle must be non-empty
     * and be conpletely within the image.  The returned blob contains the average color of the rectangular area
     * of the image and can be passed to the scribbler's configureBlob method.
     * <p><p>
     * <b>Precondition:</b> (xlow,ylow) is the coordinate of the upperleft corner or a rectangle within the image
     * (i.e., >=0), width and height are both > 0, and the lowerright corner of the rectangle is within the image.
     * 
     * @return A blob that can be passed to a scribbler configureBlob method.
     */
    public MyroBlobSpec defineBlob( int xlow, int ylow, int width, int height )
    {
        assert width > 0 && height > 0 : "defineBlob: width or height is <= 0";
        assert xlow + width - 1 < width() : "defineBlob: right side of rectangle not within image";
        assert ylow + height - 1 < height() : "defineBlob: bottom of rectangle not within image";

        // calculate size of the defined rectangle
        int size = width * height;

        // calculate upper coordinates
        int xhigh = xlow + width - 1;
        int yhigh = ylow + height - 1;

        // Convert rgb pixels to yuv, calculate yuv component sums, then yuv component means
        int totalY = 0;
        int totalU = 0;
        int totalV = 0;
        MyroYUVColor yuvPixels[] = new MyroYUVColor[ size ];
        int pos = 0;         // index into yuvPixels array

        for( int x=xlow; x<=xhigh; x++ )
            for( int y=ylow; y<=yhigh; y++)
            {
                // convert rgb pixel to yuv and store in yuvPixels array
                yuvPixels[pos] = new MyroYUVColor( get( x, y ) );

                // add yuv values to component totals
                totalY += yuvPixels[pos].getY();
                totalU += yuvPixels[pos].getU();
                totalV += yuvPixels[pos].getV();

                // increment pos for next iteration
                pos++;
        }

        // calculate yuv component means
        double yMean = (double)totalY / (double)size;
        double uMean = (double)totalU / (double)size;
        double vMean = (double)totalV / (double)size;

        // Calculate the standard deviation of the yuv components
        // The standard deviation of a random variable with a normal distribution is the root-mean-square
        // (RMS) deviation of its values from their mean.

        double rmsSumY = 0.0;
        double rmsSumU = 0.0;
        double rmsSumV = 0.0;

        for( int i=0; i<size; i++ )
        {
            rmsSumY += Math.pow( yuvPixels[i].getY() - yMean, 2 );
            rmsSumU += Math.pow( yuvPixels[i].getU() - uMean, 2 );
            rmsSumV += Math.pow( yuvPixels[i].getV() - vMean, 2 );
        }

        double sY = Math.sqrt( rmsSumY / size );
        double sU = Math.sqrt( rmsSumU / size );
        double sV = Math.sqrt( rmsSumV / size );

        // Select the U/V bounding box based upon stdMod stdDev from the mean, with appropriate min/max
        // values to fit in an 8-bit variable
        final double stdMod = 3.0;
        int minU = Math.max( (int)(uMean - sU*stdMod), 0 );
        int maxU = Math.min( (int)(uMean + sU*stdMod), 255 );
        int minV = Math.max( (int)(vMean - sV*stdMod), 0 );
        int maxV = Math.min( (int)(vMean + sV*stdMod), 255 );

        // return a blob definition, using our calculated U/V bounding box and y bounds of 0..254
        // and a threshold of 4 (which is the default)

        return new MyroBlobSpec( 0, 254, minU, maxU, minV, maxV, 4 );
    }

    /**
     * Returns the RGB color of pixel (x,y).
     * <p><p>
     * <b>Precondition:</b> (x,y) is the coordinate of a pixel in the image.
     * 
     * @param x x xoordinate of the pixel
     * @param y y coordinate of the pixel
     * @return The color of pixel (x,y)
     */
    public abstract Color get( int x, int y );

    /**
     * Returns the grayscale value of pixel (x,y).
     * <p><p>
     * <b>Precondition:</b> (x,y) is the coordinate of a pixel in the image.
     * 
     * @param x x xoordinate of the pixel
     * @param y y coordinate of the pixel
     * @return The grayscale value of pixel (x,y), in the range 0..255
     */
    public abstract int getGray( int x, int y );

    /**
     * Sets the color of pixel (x,y) to an RGB color.  Changes will not appear on the screen until either
     * {@link #show show} or {@link #repaint repaint} is called.
     * <p><p>
     * <b>Precondition:</b> (x,y) is the coordinate of a pixel in the image, color is not null.
     * 
     * @param x x xoordinate of the pixel
     * @param y y coordinate of the pixel
     * @param color An RGB Color that pixel (x,y) is set to.
     */
    public abstract void set( int x, int y, Color color);

    /**
     * Sets the color of pixel (x,y) to a grayscale color.
     * <p><p>
     * <b>Precondition:</b> (x,y) is the coordinate of a pixel in the image, grayLevel is between 0 (inclusive)
     * and 255 (inclusive).  Changes will not appear on the screen until either {@link #show show}
     * or {@link #repaint repaint} is called.
     * 
     * @param x x xoordinate of the pixel
     * @param y y coordinate of the pixel
     * @param grayLevel A grayscale value between 0 and 255 that pixel (x,y) is set to.
     */
    public abstract void setGray( int x, int y, int grayLevel);

    /**
     * Save the image to a file.  The only supported formats are jpg and png.
     * <p><p>
     * <b>Precondition:</b> filename must have an extension of .jpg or .png
     * 
     * @param filename Name of the file to save the image to.
     */
    public void save(String filename) {
        File file = new File(filename);

        String suffix = filename.substring(filename.lastIndexOf('.') + 1);
        suffix = suffix.toLowerCase();
        if (suffix.equals("jpg") || suffix.equals("png")) {
            try { ImageIO.write(image, suffix, file); }
            catch (IOException e) { e.printStackTrace(); }
        }
        else {
            System.out.println("Error: filename must end in .jpg or .png");
        }
    }

    /*
     * Private and Protected methods
     */

    /**
     * Returns the luminosity of an RGB color.
     */
    protected double lum(Color color) {
        int r = color.getRed();
        int g = color.getGreen();
        int b = color.getBlue();
        return .299*r + .587*g + .114*b;
    }

    /**
     * Returns a grayscale color of an RGB color.
     */
    protected Color toGray(Color color) {
        int y = (int) (Math.round( lum( color ) ) );   // round to nearest int        
        return new Color( y, y, y );
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
            Color c = get( x, y );
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
                blobx2 = Math.max( Math.min( e.getX(), width-1 ), 0 );
                bloby2 = Math.max( Math.min( e.getY(), height-1 ), 0 );
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
                blobx2 = Math.max( Math.min( e.getX(), width-1 ), 0 );
                bloby2 = Math.max( Math.min( e.getY(), height-1 ), 0 );
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
            g2.drawImage( image, 0, 0, width, height, this );

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
     * Private class to hold a color specified in YUV space.  Several methods are defined to convert to/from
     * rgb.
     */
    private class MyroYUVColor
    {
        private int y, u, v;

        public MyroYUVColor( int _y, int _u, int _v )
        {
            y = _y;
            u = _u;
            v = _v;
        }

        public MyroYUVColor( Color c )
        {
            set2rgb( c );
        }

        public Color yuv2rgb()
        {
            int r = Math.max( Math.min( (int)(y + (1.4075 * (v - 128))), 255 ), 0 );
            int g = Math.max( Math.min( (int)(y - (0.3455 * (u - 128)) - (0.7169 * (v - 128))), 255 ), 0 );
            int b = Math.max( Math.min( (int)(y + (1.7790 * (u - 128))), 255 ), 0 );
            return new Color( r, g, b );
        }

        public void set2rgb( Color c )
        {
            int r = c.getRed();
            int g = c.getGreen();
            int b = c.getBlue();

            y = Math.max( Math.min( (int)(0.299 * r + 0.587 * g + 0.114 * b), 255 ), 0 );
            u = Math.max( Math.min( (int)(-0.14713 * r - 0.28886 * g + 0.436 * b + 128), 255 ), 0 );
            v = Math.max( Math.min( (int)( 0.615 * r - 0.51499* g - 0.10001 * b + 128), 255 ), 0 );
        }

        public int getY()
        {
            return y;
        }

        public int getU()
        {
            return u;
        }

        public int getV()
        {
            return v;
        }

    }

}
