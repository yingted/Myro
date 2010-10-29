import Myro.*;

public class dance
{
    Scribbler robot;
    
    public dance()
    {
        robot = new Scribbler("/dev/rfcomm1");
    }
    
    public void doIt()
    {
        robot.forward(1.0, 1.0);
        robot.turnRight(0.75, 2.0 );
        robot.forward(-1.0, 1.0);
        
        robot.close();
    }
}
    
