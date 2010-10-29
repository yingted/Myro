import Myro.*;

public class Explore
{
    Scribbler robot;

    public Explore ()
    {
        robot = new Scribbler("/dev/rfcomm1");
    }
    
    public void doIt()
    {
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
