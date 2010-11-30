
// ListControllers.java
// Andrew Davison, October 2006, ad@fivedots.coe.psu.ac.th

/* Use JInput to list the names and types of all the
available input devices (controllers).

An index number precedes each controller, which is used
in the ControlerDetailss and TestController applications.

Usage pattern:
runJI ListControllers
 */

import java.io.*;
import net.java.games.input.*;

public class ListControllers 
{

    public static void main(String[] args) 
    {
        System.out.println("JInput version: " + Version.getVersion());

        ControllerEnvironment ce =
            ControllerEnvironment.getDefaultEnvironment();
        Controller[] cs = ce.getControllers();

        if (cs.length == 0) {
            System.out.println("No controllers found");
            System.exit(0);
        }

        // print the name and type for each controller
        for (int i = 0; i < cs.length; i++)
        {
            System.out.println(i + ". " + cs[i].getName() + ", " + cs[i].getType() );
            Component[] comp = cs[i].getComponents();
            for (int j=0; j<comp.length; j++)
            {
                System.out.println( j + ": Name=" + comp[j].getName() + 
                ", Type=" + comp[j].getIdentifier().getClass().toString());
            }
        }
    } // end of main()

} // end of ListControllers class
