import Myro.*;

/**
 * This program brings up the joystick to allow the user to control the scribbler.  It also displays the
 * camera image once a second.  The program stop when a key is pressed on the keyboard.
 * 
 * @author Douglas Harms 
 */
public class VisualExplorer
{
    public static void main(String[] args)
    {
        final String scribblerPort = "com10";

        Scribbler robot = new Scribbler( scribblerPort );
        // abort if port does not exist
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to "+scribblerPort, "Bummer" );
            return;
        }

        MyroImage image = null;

        // display the joystick
        robot.joyStick();

        // execute until a key is pressed
        while ( !MyroListener.isKeyPressed() )
        {
            // display the camera image
            image = robot.takePicture(Scribbler.IMAGE_COLOR);
            image.show();
            
            // wait 1 second
            MyroUtils.sleep( 1.0 );
        }
        
        // stop the robot, hide the image window, and close the connection
        robot.stop();
        image.hide();
        robot.close();
    }
}
