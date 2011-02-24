import Myro.*;

/**
 * Write a description of class PictureSequence here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class PictureSequence
{
    public static void main()
    {
        Scribbler robot= new Scribbler("/dev/rfcomm2");

        MyroImage images[] = new MyroImage[10];
        String[] options = {"Yes", "No" };
        String ans;

        // take 10 pictures and store them in the images array
        for( int i=0; i<10; i++ )
        {
            images[i] = robot.takePicture( Scribbler.IMAGE_COLOR );
            robot.turnLeft( 0.3, 0.1 );
        }

        // now display the 10 images
        //MyroImage displayImage = images[0];
        //displayImage.show();
        do
        {
            for( int i=0; i<10; i++ )
            {
                //displayImage.setImage( images[i] );
                images[i].show();
                Scribbler.wait( 0.5 );
            }

            ans = MyroGUI.askQuestion( "Would you like to see it again?", options );
        }
        while( ans.equals("Yes") );

        //displayImage.hide();
        images[9].hide();
        robot.close();
    }
}
