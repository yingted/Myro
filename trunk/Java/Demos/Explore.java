import Myro.*;

/**
 * This program causes the robot to "explore" the room.  The robot starts moving forward, and when it
 * hits a wall (i.e., it stalls) it stops, backs up a bit, turns a bit, then continues moving forward.
 * The program stops when a key is pressed or 1 minute is up, whichever occurs first.
 * 
 * @author Douglas Harms 
 */
public class Explore
{
    public static void main( String[] args )
    {
        final String scribblerPort = "/dev/rfcomm1";

        Scribbler robot = new Scribbler( scribblerPort );
        // abort if port does not exist
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to "+scribblerPort, "Bummer" );
            return;
        }

        // start moving forward
        robot.forward( 0.7 );
        
        // keep executing for 1 minute or until a key is pressed
        long startTime = System.currentTimeMillis();
        long endTime = startTime + 60*1000;
        while( System.currentTimeMillis() < endTime && !MyroListener.isKeyPressed() )
        {
            if( robot.getStall() )
            {
                // we must have hit something so stop, backup a bit, turn, and continue
                robot.stop();
                robot.backward( 0.4, 1.0 );
                robot.turnLeft( 0.5, 0.75 );
                robot.forward( 0.7 );
            }
        }
        
        // stop the robot and close
        robot.stop();
        
        robot.close();
    }
}
