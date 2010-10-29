
/**
 * Write a description of class testImage here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import java.awt.*;
import javax.swing.*;

import Myro.*;

public class testImage
{
    public static void main( String[] args )
    {
        MyroImage image = new MyroColorImage("testimage.jpg");
        MyroImage displayedImage = new MyroColorImage(1,1);

        displayedImage.setImage( image );
        displayedImage.show(100,200);
        //image.show();

        JOptionPane.showMessageDialog(null, "Click when ready to see the negative.");
        displayedImage.hide();

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

        displayedImage.setImage( negImage );
        displayedImage.show();
        displayedImage.repaint();
        //negImage.show();

        JOptionPane.showMessageDialog(null, "Click when ready to see the edge image.");
        displayedImage.hide();

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

        JOptionPane.showMessageDialog(null, "Click when ready to see the blur image.");
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

        JOptionPane.showMessageDialog(null, "Click when ready to continue.");
        blurImage.hide();

        negImage.show();

        JOptionPane.showMessageDialog(null, "Click again when ready to continue.");
        for( int y=0; y<image.height(); y++ )
        {
            negImage.set( y, y, Color.RED );
            negImage.set( image.height()-y, y, Color.GREEN );
        }

        JOptionPane.showMessageDialog(null, "Click when ready to see an X in the image.");
        negImage.repaint();

        JOptionPane.showMessageDialog(null, "Click when ready to exit.");
    }
}
