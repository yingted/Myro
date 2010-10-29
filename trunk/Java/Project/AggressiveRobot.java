import Myro.*;

public class AggressiveRobot
{
    Scribbler robot;

    AggressiveRobot(String portName)
    {
        robot = new Scribbler(portName);
    }

    public void doIt()
    {
        int leftLight, rightLight;
        int leftAmbient, rightAmbient;
        
        leftAmbient = robot.getLight(Scribbler.SENSOR_LIGHT_LEFT);
        rightAmbient = robot.getLight(Scribbler.SENSOR_LIGHT_RIGHT);
        while( true )
        {
            leftLight = robot.getLight(Scribbler.SENSOR_LIGHT_LEFT);
            rightLight = robot.getLight(Scribbler.SENSOR_LIGHT_RIGHT);
            robot.motors(normalize(rightLight,rightAmbient), normalize(leftLight,leftAmbient));
        }
    }

    private double normalize( int v, int ambient )
    {
        if( v > ambient )
            v = ambient;
            
            return 1.0 - (double)v / (double)ambient;
    }

}