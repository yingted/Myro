import Myro.*;

/**
 * This program uses the Fluke obstacle sensors to avoid collisions.
 * 
 * @author Douglas Harms 
 */
public class Tutorial2_Fluke_IR
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

        final int THRESH = 10;  // An obstacle value greater than this indicates an obstacle

        // You'll need to play with this to find a good value for your Fluke board.  If the
        // robot detects collisions too often, then lower this value by 1 or 2 at a time; if it
        // never detects a collision then increase it by 1 or 2 at a time.
        robot.setIRPower( 130 );

        // run this program until the Q key is hit
        while( MyroListener.getKeyPressed() != 'q' )
        {
            // move forward, stopping when the Fluke's IR sensors indicate an obstacle is
            // in the way
            robot.forward( 0.6 );
            while( robot.getObstacle( Scribbler.SENSOR_IR_LEFT ) < THRESH && 
            robot.getObstacle( Scribbler.SENSOR_IR_RIGHT ) < THRESH &&
            robot.getObstacle( Scribbler.SENSOR_IR_CENTER ) < THRESH )
            {
                MyroUtils.sleep( 0.1 );
            }

            // there's an obstacle in the way so stop
            robot.stop();

            // now rotate until we're obstacle-free
            robot.rotate( 0.2 );
            while( robot.getObstacle( Scribbler.SENSOR_IR_LEFT ) >= THRESH ||
            robot.getObstacle( Scribbler.SENSOR_IR_RIGHT ) >= THRESH ||
            robot.getObstacle( Scribbler.SENSOR_IR_CENTER ) >= THRESH )
            {
                MyroUtils.sleep (0.1 );
            }

            // nothing in the way so start moving again
            robot.forward( 0.6 );

        }

        // stop all movement
        robot.stop();

        // close the robot
        robot.close();
    }
}
