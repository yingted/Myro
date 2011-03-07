import Myro.*;

/**
 * Description of the program
 * 
 * @author (your name) 
 */
public class Tutorial1_SimpleRemoteControl
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
        // loop until the user indicates s/he wants to quit
        boolean finished = false;
        while( !finished )
        {
            char key = MyroListener.getKeyPressed();
            if( key != MyroListener.NO_KEY_PRESSED )
            {
                switch( key )
                {
                    case 'f':
                    case 'F': robot.forward(); break;
                    
                    case 'b':
                    case 'B': robot.backward(); break;
                    
                    case 's':
                    case 'S': robot.stop(); break;
                    
                    case 'q':
                    case 'Q': finished = true; break;
                }
            }
        }
        
        // stop the robot
        robot.stop();

        // close the robot
        robot.close();
    }
}
