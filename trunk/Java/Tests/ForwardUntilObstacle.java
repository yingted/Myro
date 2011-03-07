import Myro.*;

/**
 * Write a description of class ForwaardUntilObstacle here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class ForwardUntilObstacle
{

    public static void main()
    {
        Scribbler robot = new Scribbler("/dev/rfcomm2");
        
        robot.backward();
        
        long startTime = System.currentTimeMillis();
        while( System.currentTimeMillis() < startTime+10000 )
        {
            if( !robot.getIR( Scribbler.SENSOR_IR_LEFT ) )
            {
                robot.stop();
                robot.beep( 1.0, 440 );
            }
            else
            {
                robot.backward();
            }
        }
        
        robot.stop();
        
        robot.close();
    }
}
