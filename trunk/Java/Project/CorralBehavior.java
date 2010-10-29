import Myro.*;

public class CorralBehavior
{
    // implements the corral problem using behavior approach described in Chapter 7

    Scribbler robot;

    public CorralBehavior( String portName )
    {
        robot = new Scribbler( portName );
    }

    final double cruiseSpeed = 0.8;
    final double turnSpeed = 0.8;
    final int lightThresh = 80;

    private void cruise( boolean output, double translate, double rotate )
    {
        output = true;
        translate = cruiseSpeed;
        rotate = turnSpeed;
    }

    private void avoid( boolean output, double translate, double rotate )
    {
        boolean obstacleOnLeft = !robot.getIR(Scribbler.SENSOR_IR_LEFT);
        boolean obstacleOnRight = !robot.getIR(Scribbler.SENSOR_IR_RIGHT);

        if( obstacleOnLeft )
        {
            output = true;
            translate = 0.0;
            rotate = -turnSpeed;
        }
        else if (obstacleOnRight )
        {
            output = true;
            translate = 0.0;
            rotate = turnSpeed;
        }
        else
        {
            output = false;
        }
    }

    private void seekLight( boolean output, double translate, double rotate )
    {
        int leftLight = robot.getLight(Scribbler.SENSOR_LIGHT_LEFT);
        int rightLight = robot.getLight(Scribbler.SENSOR_LIGHT_RIGHT);

        if( leftLight < lightThresh )
        {
            output = true;
            translate = cruiseSpeed/2.0;
            rotate = turnSpeed;
        }
        else if (rightLight < lightThresh )
        {
            output = true;
            translate = cruiseSpeed/2.0;
            rotate = -turnSpeed;
        }
        else
        {
            output = false;
        }
    }

    private void arbitrate( double translate, double rotate )
    {
        boolean behaviorOutput=false;

        // high priority
        //seekLight( behaviorOutput, translate, rotate );
        if( behaviorOutput )
            return;

        avoid( behaviorOutput, translate, rotate );
        if( behaviorOutput )
            return;

        cruise( behaviorOutput, translate, rotate );
    }

    
    public void doIt()
    {
        double translate = 0.0;
        double rotate = 0.0;
        
        while (true )
        {
            arbitrate( translate, rotate );
            robot.move( translate, rotate );
        }
    }
    
}