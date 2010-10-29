
/**
 * Write a description of class VisualExplorer here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import Myro.*;

public class VisualExplorer
{
   public static void main(String[] args)
   {
       Scribbler robot = new Scribbler("/dev/rfcomm0");
       MyroImage image = new MyroColorImage(100,100);
       
       robot.joyStick();
       
       while (true)
       {
           image.setImage( robot.takePicture(Scribbler.IMAGE_GRAY) );
           image.show();
           try
           {
               Thread.sleep(1000);
            } catch (InterruptedException e) {};
        }
    }
}
