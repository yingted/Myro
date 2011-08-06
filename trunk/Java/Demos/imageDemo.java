import Myro.*;
import java.awt.Color;

/**
 * This simple program captures an image from the scribbler and does some simple processing of that
 * image.  This program is similar to testImage, except that testImage reads in the image from a file
 * rather than capturing it from the scribbler.
 * 
 * @author Douglas Harms
 */
public class imageDemo
{
    public static void main( String[] args )
    {

        String scribblerPort;
        scribblerPort = MyroGUI.inputString("Please enter the port name");

        Scribbler robot= new Scribbler( scribblerPort );
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to " + scribblerPort, "Bummer" );
            return;
        }

        // take a picture and display it
        MyroImage image = robot.takePicture( Scribbler.IMAGE_COLOR );
        image.show();

        // let the user spend time pondering the wonderful photo
        MyroGUI.tellUser( "Click when ready to see the negative.", "OK" );

        // calculate and display the image negative.  This uses a for-each loop to iterate over
        // all pixels in image
        MyroImage negImage= new MyroColorImage( image.getWidth(), image.getHeight() );
        for( MyroPixel p : image )
        {
            // get the coordinate and color of the current pixel
            int x = p.getX();
            int y = p.getY();
            Color c = p.getColor();

            // create a new color that is the negative
            Color neg = new Color(255-c.getRed(), 255-c.getGreen(), 255-c.getBlue() );

            // set the color of the corresponding pixel in negImage
            MyroPixel negPixel = negImage.getPixel( x, y );
            negPixel.setColor( neg );
        }

        negImage.show();

        // again, wait for the user to finish being impressed
        MyroGUI.tellUser( "Click when ready to see the edge image.", "OK" );

        // now calculate and display an edge-enhanced image
        MyroImage edgeImage = new MyroGrayImage( image.getWidth(), image.getHeight() );

        float[][] edgeKernel = {
                { -1, -1, -1 },
                { -1,  9, -1 },
                { -1, -1, -1 } };

        // Loop through every pixel in the image.  This uses nested for loops with
        // getPixel/setPixel
        for (int y = 1; y < image.getHeight()-1; y++)
        {
            for (int x = 1; x < image.getWidth()-1; x++)
            {
                float sum = 0; // Kernel sum for this pixel
                for (int ky = -1; ky <= 1; ky++)
                {
                    for (int kx = -1; kx <= 1; kx++)
                    {
                        // Calculate the adjacent pixel for this kernel point
                        //int pos = (y + ky)*img.width + (x + kx);
                        // Image is grayscale, red/green/blue are identical
                        MyroPixel pixel = image.getPixel(x+kx, y+ky);
                        int val = pixel.getGray();
                        // Multiply adjacent pixels based on the kernel values
                        sum += edgeKernel[ky+1][kx+1] * val;
                    }
                }
                // For this pixel in the new image, set the gray value
                // based on the sum from the kernel
                //edgeImg.pixels[y*img.width + x] = color(sum);
                sum = Math.min( Math.max( sum, 0 ), 255 );
                MyroPixel newPixel = edgeImage.getPixel( x, y );
                newPixel.setGray( Math.round(sum) );
            }
        }

        edgeImage.show();

        // The user will tell us when s/he is ready to proceed
        MyroGUI.tellUser( "Click when ready to see the blur image.", "OK");

        // calculate and display a blurred image
        MyroImage blurImage = new MyroGrayImage( image.getWidth(), image.getHeight() );

        double v = 1.0 / 9.0;
        double[][] blurKernel = {
                { v, v, v },
                { v, v, v },
                { v, v, v } };

        // Loop through every pixel in the image.  This uses nested for loops and getColor/setColor
        for (int y = 1; y < image.getHeight()-1; y++)
        {
            for (int x = 1; x < image.getWidth()-1; x++)
            {
                double sum = 0; // Kernel sum for this pixel
                for (int ky = -1; ky <= 1; ky++)
                {
                    for (int kx = -1; kx <= 1; kx++)
                    {
                        // Calculate the adjacent pixel for this kernel point
                        //int pos = (y + ky)*img.width + (x + kx);
                        // Image is grayscale, red/green/blue are identical
                        int val = image.getGray( x+kx, y+ky );
                        // Multiply adjacent pixels based on the kernel values
                        sum += blurKernel[ky+1][kx+1] * val;
                    }
                }
                // For this pixel in the new image, set the gray value
                // based on the sum from the kernel
                //edgeImg.pixels[y*img.width + x] = color(sum);
                sum = Math.min( Math.max( sum, 0 ), 255 );
                blurImage.setGray( x, y, (int)Math.round(sum) );
            }
        }

        blurImage.show();

        // Let the user ponder this image
        MyroGUI.tellUser( "Click when ready to see an X in the negative image.", "OK" );

        // Create an X in the negative image and display it
        for( int y=0; y<image.getHeight(); y++ )
        {
            negImage.setColor( y, y, Color.RED );
            negImage.setColor( image.getHeight()-y, y, Color.GREEN );
        }
        negImage.show();

        // Wait for the user to be ready to exit
        MyroGUI.tellUser( "Click when ready to exit.", "Exit" );

        // close the robot and hide the image
        robot.close();
        negImage.hide();
    }
}
