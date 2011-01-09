import Myro.*;

/**
 * Write a description of class Flute here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Flute
{
    public static void main()
    {
        Scribbler robot = new Scribbler("");
        int[] notes = {440, 494, 554, 588, 659, 740, 831, 880};
        
        int note;
        int[] lights;
        
        final int THRESHOLD = 2000;
        
        long startTime = System.currentTimeMillis();
        while (System.currentTimeMillis() < startTime + 10000 )
        {
            lights = robot.getLight();
            note = 0;
            if( lights[0] > THRESHOLD )
                note += 1;
            if( lights[1] > THRESHOLD )
                note += 2;
            if( lights[2] > THRESHOLD )
                note += 4;
                
            robot.beep( 0.2, notes[note] );
        }
        
        robot.close();
    }
}
