import Myro.Scribbler;

public class AliveRobot
{
    public static void main( String[] args )
    {
        Scribbler robot;

        robot = new Scribbler("/dev/rfcomm0");
        int light;
        int ambient;

        ambient = robot.getLight(Scribbler.SENSOR_LIGHT_CENTER);

        while( true )
        {
            light = robot.getLight(Scribbler.SENSOR_LIGHT_CENTER);
            robot.forward( -normalize(light,ambient) );
        }
    }

    private static double normalize( int v, int ambient )
    {
        if( v > ambient )
            v = ambient;

        return 1.0 - (double)v / (double)ambient;
    }

}