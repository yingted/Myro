import Myro.*;

public class AggressiveRobot
{
    public static void main( String[] args )
    {
        Scribbler robot;

        robot = new Scribbler("/dev/rfcomm0");
        int leftLight, rightLight;
        int leftAmbient, rightAmbient;

        leftAmbient = robot.getLight(Scribbler.SENSOR_LIGHT_LEFT);
        rightAmbient = robot.getLight(Scribbler.SENSOR_LIGHT_RIGHT);
        while( true )
        {
            leftLight = robot.getLight(Scribbler.SENSOR_LIGHT_LEFT);
            rightLight = robot.getLight(Scribbler.SENSOR_LIGHT_RIGHT);
            robot.motors( normalize(rightLight,rightAmbient), normalize(leftLight,leftAmbient) );
        }
    }

    private static double normalize( int v, int ambient )
    {
        if( v > ambient )
            v = ambient;

        return 1.0 - (double)v / (double)ambient;
    }

}