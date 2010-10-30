import Myro.*;

public class Explore
{
    public static void main( String[] args )
    {
        Scribbler robot;

        robot = new Scribbler("/dev/rfcomm1");

        robot.forward( 0.7 );

        while( true )
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
    }
}
