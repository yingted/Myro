import Myro.*;

public class LoveRobot
{
    Scribbler robot;

    LoveRobot(String portName)
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
            robot.motors(normalize(leftLight,leftAmbient), normalize(rightLight,rightAmbient));
            //System.out.println("left="+normalize(leftLight,leftAmbient)+",right="+normalize(rightLight,rightAmbient));
        }
    }

    private double normalize( int v, int ambient )
    {
        double mean = ambient/2.0;
        double stddev = ambient/6.0;
        
        if( v > ambient )
            v = ambient;
            
        return Math.exp( -Math.pow( v-mean, 2) / (2.0 * Math.pow(stddev, 2) ) );
    }
//     private double normalize( int v, int ambient )
//     {
//         if( v > ambient )
//             v = ambient;
//             
//             return (double)v / (double)ambient;
//     }

}