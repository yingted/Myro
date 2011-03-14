import Myro.*;

/**
 * Description of the program
 * 
 * @author (your name) 
 */
public class Tutorial2_Simple_IR_Sensing
{
    // declare a global variable for the scribbler
    private static Scribbler robot;

    public static void main( String[] args )
    {
        // open connection to robot and abort if this failed
        final String scribblerPort = "/dev/rfcomm0";

        robot = new Scribbler( scribblerPort );
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to " + scribblerPort, "Bummer" );
            return;
        }

        long startTime = System.currentTimeMillis();
        long endTime = startTime + 60*1000;
        while( System.currentTimeMillis() < endTime )
        {
            // move backward until an obstacle is detected
            robot.backward( 0.7 );
            while( robot.getIR( Scribbler.SENSOR_IR_LEFT ) && 
            robot.getIR( Scribbler.SENSOR_IR_RIGHT ) )
            {
                MyroUtils.sleep( 0.1 );
            }

            // there's an obstacle in the way so stop
            robot.stop();

            // now rotate until we're obstacle-free
            robot.rotate( 0.3 );
            while( !robot.getIR( Scribbler.SENSOR_IR_LEFT ) ||
            !robot.getIR( Scribbler.SENSOR_IR_RIGHT ) )
            {
                MyroUtils.sleep( 0.1 );
            }
            // nothing in the way so start moving again
            robot.backward( 0.7 );

        }
        // stop all movement
        robot.stop();

        // close the robot
        robot.close();
    }
}

 