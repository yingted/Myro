import Myro.*;

/**
 * Description of the program
 * 
 * @author (your name) 
 */
public class MyroProgramTemplate
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

        // program code goes here

        // close the robot
        robot.close();
    }
 }

 