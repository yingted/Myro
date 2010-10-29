import Myro.Scribbler;

public class AliveRobot
{
    Scribbler robot;

    AliveRobot(String portName)
    {
        robot = new Scribbler(portName);
    }

    public void doIt()
    {
        int light;
        int ambient;
        
        ambient = robot.getLight(Scribbler.SENSOR_LIGHT_CENTER);
        
        while( true )
        {
            light = robot.getLight(Scribbler.SENSOR_LIGHT_CENTER);
            robot.forward(normalize(light,ambient));
        }
    }

    private double normalize( int v, int ambient )
    {
        if( v > ambient )
            v = ambient;
            
            return 1.0 - (double)v / (double)ambient;
    }

}