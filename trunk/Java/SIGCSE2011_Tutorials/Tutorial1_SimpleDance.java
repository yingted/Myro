import Myro.*;

/**
 * SIGCSE Tutorial 1 - Simple Dance.  This program has the scribbler do a very simple dance routine.
 * 
 * @author Douglas Harms 
 */
public class Tutorial1_SimpleDance
{
    // declare a global variable for the scribbler
    private static Scribbler robot;

    public static void main( String[] args )
    {
        // open connection to robot and abort if this failed
        final String scribblerPort = "/dev/rfcomm1";

        robot = new Scribbler( scribblerPort );
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to " + scribblerPort, "Bummer" );
            return;
        }

        // do a very simple dance
        for( int i=0; i<3; i++)
        {
            robot.turnLeft( 0.7, 0.5 );
            robot.turnRight( 0.7, 0.5 );
        }

        for( int i=0; i<5; i++ )
        {
            robot.forward( 1.0, 1.5 );
            robot.backward( 1.0, 1.5 );
        }

        // close the robot
        robot.close();
    }
}
