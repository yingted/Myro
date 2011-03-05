import Myro.*;
import java.awt.Color;

/**
 * Simple program that reads in an image and does some simple processing of that image.  THis program
 * does not use the scribbler.
 * 
 * @author Douglas Harms 
 */

public class testImage
{
    public static void main( String[] args )
    {
        MyroImage image = new MyroColorImage("testimage.jpg");

        image.show();

        MyroGUI.askQuestion( "Click when ready to see the negative.", "OK" );
        
        MyroImage negImage= new MyroColorImage( image.width(), image.height() );
        for( int row=0; row<image.height(); row++)
        {
            for( int col=0; col<image.width(); col++)
            {
                Color c=image.get(col, row);
                Color neg = new Color(255-c.getRed(), 255-c.getGreen(), 255-c.getBlue() );
                negImage.set(col, row, neg );
            }
        }

        negImage.show();

        MyroGUI.askQuestion( "Click when ready to see the edge image.", "OK" );
        
        MyroImage edgeImage = new MyroGrayImage( image.width(), image.height() );

        float[][] edgeKernel = {
                { -1, -1, -1 },
                { -1,  9, -1 },
                { -1, -1, -1 } };

        // Loop through every pixel in the image.
        for (int y = 1; y < image.height()-1; y++)
        {
            for (int x = 1; x < image.width()-1; x++)
            {
                float sum = 0; // Kernel sum for this pixel
                for (int ky = -1; ky <= 1; ky++)
                {
                    for (int kx = -1; kx <= 1; kx++)
                    {
                        // Calculate the adjacent pixel for this kernel point
                        //int pos = (y + ky)*img.width + (x + kx);
                        // Image is grayscale, red/green/blue are identical
                        int val = image.getGray( x+kx, y+ky );
                        // Multiply adjacent pixels based on the kernel values
                        sum += edgeKernel[ky+1][kx+1] * val;
                    }
                }
                // For this pixel in the new image, set the gray value
                // based on the sum from the kernel
                //edgeImg.pixels[y*img.width + x] = color(sum);
                sum = Math.min( Math.max( sum, 0 ), 255 );
                edgeImage.setGray( x, y, Math.round(sum) );
            }
        }

        edgeImage.show();

        MyroGUI.askQuestion( "Click when ready to see the blur image.", "OK");
        edgeImage.hide();

        MyroImage blurImage = new MyroGrayImage( image.width(), image.height() );

        double v = 1.0 / 9.0;
        double[][] blurKernel = {
                { v, v, v },
                { v, v, v },
                { v, v, v } };

        // Loop through every pixel in the image.
        for (int y = 1; y < image.height()-1; y++)
        {
            for (int x = 1; x < image.width()-1; x++)
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
        MyroGUI.askQuestion( "Click when ready to continue.", "OK" );
        blurImage.hide();

        negImage.show();

        MyroGUI.askQuestion( "Click again when ready to continue.", "OK");
        for( int y=0; y<image.height(); y++ )
        {
            negImage.set( y, y, Color.RED );
            negImage.set( image.height()-y, y, Color.GREEN );
        }

        MyroGUI.askQuestion( "Click when ready to see an X in the image.", "OK");
        negImage.repaint();

        MyroGUI.askQuestion( "Click when ready to exit.", "Exit" );
        negImage.hide();
    }
}
