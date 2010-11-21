
/**
 * Write a description of class FollowBlob here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import Myro.*;

public class FollowBlob
{

    public static void main(String[] args)
    {
        final double TURN_SPEED_TRACKING = 0.1;
        final double TURN_TIME_TRACKING = 0.1;
        final double TURN_SPEED_SEARCHING = 0.2;
        final double TURN_TIME_SEARCHING = 0.2;
        
        int pixelCountThreshold;

        Scribbler robot = new Scribbler("/dev/rfcomm0");

        // let user define a blob in the robot's image
        MyroImage image = robot.takePicture(Scribbler.IMAGE_COLOR);
        image.show();
        MyroBlobSpec blobSpec = image.getUserDefinedBlob();
        robot.configureBlob( blobSpec );
        
        // no reason to keep the image around any more
        image.hide();

        // for the next minute make sure the blob is in the center of the image
        long startTime = System.currentTimeMillis();

        // get the blob and set the count threshold to it (since we assume that when the user
        // set the blob, the number of pixels that are in the blob now are what we expect)
        MyroBlobImageInfo blobInfo = robot.getBlob();
        pixelCountThreshold = (int)(blobInfo.getPixelCount() * 0.9);
        while( System.currentTimeMillis()-startTime <= 60000 )
        {
            blobInfo = robot.getBlob();
            System.out.println("pixel count=" + blobInfo.getPixelCount());
            if( blobInfo.getPixelCount() < pixelCountThreshold )
                robot.turnLeft( TURN_SPEED_SEARCHING, TURN_TIME_SEARCHING );
            else if( blobInfo.getAverageX() < 100 )
                robot.turnLeft( TURN_SPEED_TRACKING, TURN_TIME_TRACKING );
            else if( blobInfo.getAverageX() > 150 )
                robot.turnRight( TURN_SPEED_TRACKING, TURN_TIME_TRACKING );
        }

        robot.close();
    }
}
