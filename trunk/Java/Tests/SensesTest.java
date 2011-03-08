import Myro.*;

/**
 * Description of the program
 * 
 * @author (your name) 
 */
public class SensesTest
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

        robot.senses();
        
//         long startTime = System.currentTimeMillis();
//         long endTime = startTime + 60000;
//         while( System.currentTimeMillis() < endTime ) ;
        
        MyroUtils.sleep( 60 );

        // close the robot
        robot.close();
    }
 }

 