import Myro.*;

/**
 * This program makes the scribbler into a simple flute, using the three light sensors as the
 * "keys" to the flute.
 * 
 * @author Douglas Harms
 */
public class Flute
{
    public static void main(String[] args)
    {
        // change this for your scribbler port
        final String scribblerPort = "/dev/rfcomm0";
        
        // define the 8 notees on the flute
        int[] notes = {440, 494, 554, 588, 659, 740, 831, 880};

        // This constant defines the threshold for the light sensors.  A reading smaller than
        // this means the sensor is "closed".  You may need to adjust this depending on the ambient
        // light levels in the room,
        final double THRESHOLD = 0.01;

        int note;               // this is the selected note to play (0-7)
        double[] lights;        // array used to hold the values of the 3 light sensors

        // instantiate the scribbler
        Scribbler robot = new Scribbler(scribblerPort);

        // Play the flute until a keyboard key is pressed
        while( !MyroListener.isKeyPressed() )
        {
            // get the values in the three light sensors
            lights = robot.getLight();

            // calculate the note selected
            note = 0;
            if( lights[0] < THRESHOLD )
                note += 1;
            if( lights[1] < THRESHOLD )
                note += 2;
            if( lights[2] < THRESHOLD )
                note += 4;

            // have the scribbler play the note
            robot.beep( notes[note], 0.2 );
        }

        robot.close();
    }
}
