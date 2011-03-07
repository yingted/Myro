
/**
 * Write a description of class testBlobFilter here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import Myro.*;

public class testBlobFilter
{
    private static MyroGrayImage filterBlob( MyroImage image )
    {
        // assume image is a blob image with each pixel either white or black
        double filter [][] = new double[][] {
                { 1.0, 1.0, 1.0 },
                { 1.0, 0.0, 1.0 },
                { 1.0, 1.0, 1.0 } };

        final double THRESHOLD = 5.0;

        // create new image by filtering image
        MyroGrayImage filteredImage = new MyroGrayImage( image.width(), image.height() );
        for( int x=1; x<image.width()-1; x++ )
        {
            for( int y=1; y<image.height()-1; y++ )
            {
                double sum = 0.0;
                for (int ky = -1; ky <= 1; ky++)
                {
                    for (int kx = -1; kx <= 1; kx++)
                    {
                        // Calculate the adjacent pixel for this filter point
                        int val = image.getGray( x+kx, y+ky );
                       
                        // val is 0(for black) or 1 (for white)
                        val = Math.min( val, 1 );
                        
                        // Multiply adjacent pixels based on the filter values
                        sum += filter[ky+1][kx+1] * val;
                    }
                }

                // set the pixel in the filtered image based on whether the sum exceeds the threshold
                if( sum >= THRESHOLD )
                    filteredImage.setGray( x, y, 255 );
                else
                    filteredImage.setGray( x, y, 0 );
            }
        }

        // return the filtered image
        return filteredImage;
    }

    public static void main(String[] args)
    {
        Scribbler robot = new Scribbler("/dev/rfcomm0");

        // let user define a blob in the robot's image
        MyroImage image = robot.takePicture(Scribbler.IMAGE_COLOR);
        image.show();
        MyroBlobSpec blobSpec = image.getUserDefinedBlob();
        robot.configureBlob( blobSpec );

        // get the blob image from the robot and display it
        MyroImage image1 = robot.takePicture(Scribbler.IMAGE_BLOB);
        image1.show();

        // filter the image and display it
        MyroImage image2 = filterBlob( image1 );
        image2.show( );

        // that's all folks
        robot.close();
    }

}
