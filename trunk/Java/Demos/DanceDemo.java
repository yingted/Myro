import Myro.*;

/**
 * This program demonstrates simple dancing of the Scribbler.  It is a java implementation of
 * Jennie Kay's demo, described at http://elvis.rowan.edu/~kay/sigcse/kayScribbler/
 * 
 * @author Douglas Harms 
 */
public class DanceDemo
{
    private static Scribbler robot;

    // ---- grapevineRight ----
    //
    // do a zig-zag dance
    private static void grapevineRight()
    {
        // twist left and then go back
        robot.turnLeft( 1.0, 0.1 );
        robot.backward( 0.7, 0.5 );

        // now twist right and go forward
        robot.turnRight( 1.0, 0.2 );
        robot.forward( 0.7, 0.5 );

        // twist left and then go back
        robot.turnLeft( 1.0, 0.2 );
        robot.backward( 0.7, 0.5 );

        // straighten out and pause
        robot.turnRight( 1.0, 0.1 );
        MyroUtils.sleep( 0.5 );
    }

    // ---- backSteps ----
    //
    // Take half second rolling "steps" back at half speed with pauses in between.  Then pause
    // a little bit more for effect
    private static void backSteps( int howMany )
    {
        for( int i=0; i< howMany; i++ )
        {
            robot.backward( 1.0, 0.5 );
            MyroUtils.sleep( 0.1 );
        }

        MyroUtils.sleep( 0.5 );

    }

    // ---- fancyTurn ----
    //
    // Spin to the right for the number of seconds indicated by the first parameter.  If the second
    // parameter is true then also spin to the left.
    private static void fancyTurn( double howLong, boolean turnBothWays )
    {
        // We could have just done robot.turnRight( 1.0, howLong) but this demonstrates
        // the use of directly controlling the motors.
        robot.motors( 1.0, -1.0 );
        MyroUtils.sleep( howLong );
        robot.stop();

        if( turnBothWays )
        {
            robot.turnLeft( 1.0, howLong );
        }

    }

    // ---- coinFlip ----
    //
    // return a boolean value with random probability
    private static boolean coinFlip()
    {
        return MyroUtils.randomInt( 0, 1 ) == 1;
    }
    
    public static void main( String args[] )
    {

        final String scribblerPort = "/dev/rfcomm1";
        
        robot = new Scribbler( scribblerPort );

        // abort if port does not exist
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to "+scribblerPort, "Bummer" );
            return;
        }
        
        // dance for 15 seconds
        long startTime = System.currentTimeMillis();
        long endTime = System.currentTimeMillis() + 15*1000;
        while( System.currentTimeMillis() < endTime )
        {
            // zig zag
            grapevineRight();

            // do a fancier move that depends on a random length of time between 1 and 3 seconds
            // and a randomy chosen decision about whether or not to move both directions
            long turnDuration = MyroUtils.randomInt( 1, 3 );
            boolean turnBothWays = coinFlip();
            fancyTurn( turnDuration, turnBothWays );

            // finish with a simple backstep
            backSteps( 2 );
        }

        robot.close();
    }

}
