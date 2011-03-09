import Myro.*;

/**
 * Find a line, then follow it as long as at least one of the line sensors is still on the line.
 * 
 * @author Douglas Harms 
 */
public class Tutorial2_SimpleLine
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

        // move until a line is encountered
        robot.backward( 0.1 );
        while( !robot.getLine( Scribbler.SENSOR_LINE_LEFT ) && !robot.getLine( Scribbler.SENSOR_LINE_RIGHT ))
        {
            MyroUtils.sleep( 0.1 );
        }
        robot.stop();
        
        boolean left, right;
        double direction;
        long startTime = System.currentTimeMillis();
        long endTime = startTime + 60*1000;
        while( System.currentTimeMillis() < endTime )
        {
            left = robot.getLine( Scribbler.SENSOR_LINE_LEFT );
            right = robot.getLine( Scribbler.SENSOR_LINE_RIGHT );
            if( left && right )
            {
                // we're completely on the line so move straight
                robot.backward( 0.05 );
            }
            else if( right )
            {
                // we're veering toward the left so add some clockwise rotation
                robot.rotate( -0.075 );
            }
            else if( left )
            {
                // we're veering toward the right so add some counterclockwise rotation
                robot.rotate( 0.075 );
            }
            else
            {
                // we're completely off the line so let's stop
                robot.stop();
            }
                
        }
        
        robot.stop();
        
        // close the robot
        robot.close();
    }
 }
