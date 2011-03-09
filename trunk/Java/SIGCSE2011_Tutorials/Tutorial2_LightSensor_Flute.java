import Myro.*;

/**
 * Description of the program
 * 
 * @author (your name) 
 */
public class Tutorial2_LightSensor_Flute
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

        // define the 8 notees on the flute
        int[] notes = {440, 494, 554, 588, 659, 740, 831, 880};

        // This constant defines the threshold for the light sensors.  A reading larger than
        // this means the sensor is "closed".  You may need to adjust this depending on the ambient
        // light levels in the room,
        final int THRESHOLD = 2000;

        int note;               // this is the selected note to play (0-7)
        int[] lights;           // array used to hold the values of the 3 light sensors

        // Play the flute until a keyboard key is pressed
        while( !MyroListener.isKeyPressed() )
        {
            // get the values in the three light sensors
            lights = robot.getLight();

            // calculate the note selected
            note = 0;
            if( lights[0] > THRESHOLD )
                note += 1;
            if( lights[1] > THRESHOLD )
                note += 2;
            if( lights[2] > THRESHOLD )
                note += 4;

            // have the scribbler play the note
            robot.beep( 0.2, notes[note] );
        }

        // close the robot
        robot.close();
    }
}

 