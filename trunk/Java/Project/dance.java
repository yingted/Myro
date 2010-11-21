import Myro.*;

public class dance
{
    public static void main(String[] args)
    {
        Scribbler robot;

        robot = new Scribbler("/dev/rfcomm0");
        
        robot.forward(1.0, 1.0);
        robot.turnRight(0.75, 2.0 );
        robot.forward(-1.0, 1.0);

        robot.close();
    }
}

