import Myro.*;

/**
 * Program to play a simple tune on the scribbler.
 * 
 * @author Douglas Harms
 */
public class Tutorial1_SimpleSong
{
    // declare a global variable for the scribbler
    private static Scribbler robot;

    public static void main( String[] args )
    {
        // open connection to robot and abort if this failed
        final String scribblerPort = "com10";
        
        robot = new Scribbler( scribblerPort );
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to " + scribblerPort, "Bummer" );
            return;
        }

        // Play the first part of Mary Had a Little Lamb
        robot.beep( 0.25, 494 ); // B
        robot.beep( 0.25, 440 ); // A
        robot.beep( 0.25, 392 ); // G
        robot.beep( 0.25, 440 ); // A
        robot.beep( 0.25, 494 ); // B
        robot.beep( 0.25, 494 ); // B
        robot.beep( 0.50, 494 ); // B
        robot.beep( 0.25, 440 ); // A
        robot.beep( 0.25, 440 ); // A
        robot.beep( 0.50, 440 ); // A
        robot.beep( 0.25, 494 ); // B
        robot.beep( 0.25, 587 ); // D
        robot.beep( 0.50, 587 ); // D
        
        // close the robot
        robot.close();
    }
 }
