
/**
 * Write a description of class testBlob here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import Myro.*;

public class testBlob
{
    public static void main(String[] args)
    {
        Scribbler robot = new Scribbler("/dev/rfcomm2");        
        MyroImage image = robot.takePicture(Scribbler.IMAGE_COLOR);
        MyroBlobSpec blobSpec;
        
        image.show();
        blobSpec = image.getUserDefinedBlob();
        robot.configureBlob(blobSpec);
        
        robot.takePicture(Scribbler.IMAGE_BLOB).show();
        
        robot.close();
        
    }
    
}
