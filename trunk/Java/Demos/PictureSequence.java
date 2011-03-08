import Myro.*;

/**
 * Takes a sequence of pictures then displays them in rapid succession
 * 
 * @author Douglas Harms 
 */
public class PictureSequence
{
    public static void main()
    {
        final String scribblerPort = "/dev/rfcomm1";
        
        Scribbler robot= new Scribbler( scribblerPort );
        if( !robot.portOpened() )
        {
            MyroGUI.tellUser( "Scribbler not connected to " + scribblerPort, "Bummer" );
            return;
        }

        MyroImage images[] = new MyroImage[10];
        String ans;

        // take 10 pictures and store them in the images array
        for( int i=0; i<10; i++ )
        {
            images[i] = robot.takePicture( Scribbler.IMAGE_COLOR );
            robot.turnLeft( 0.3, 0.1 );
        }

        // now display the 10 images
        do
        {
            for( int i=0; i<10; i++ )
            {
                images[i].show();
                MyroUtils.sleep( 0.1 );
            }

            ans = MyroGUI.askQuestion( "Would you like to see it again?" );
        }
        while( ans.equals("Yes") );

        images[9].hide();
        robot.close();
    }
}
