
// TestController.java
// Andrew Davison, October 2006, ad@fivedots.coe.psu.ac.th

/* Test a particular component of a specified controller.
   TestController allows us to see what float value is 
   produced by a component when the user 'presses' it.

   Usage pattern:
      runJI TestController <controller index> <component index>
   e.g.
      runJI TestController 2 0

   The component index comes from the output of the ListController
   application, and the component index from ControllerDetails.
     
   When a component is pressed, the float value is printed to the
   screen. The value is only printed once, so holding the component
   down will not trigger multiple outputs. Also, releasing the
   component does not trigger an output.

   Type ctrl-c to terminate the application.
*/


import java.io.*;
import net.java.games.input.*;


public class TestController
{
  private static final int DELAY = 40;  // ms  (polling interval)
  private static final float EPSILON = 0.0001f;   
         // to deal with values near to 0.0


  public static void main(String[] args)
  {
    if (args.length < 2) {
      System.out.println("Usage: TestController <index> <component index>");
      System.exit(0);
    }

    // get the specified controller using the first index value
    Controller c = getController(args[0]);

    // get the specified component using the second index value
    Component component = getComponent(args[1], c);

    pollComponent(c, component);    // keep polling the component
  } // end of main()


  private static Controller getController(String idxStr)
  // return the controller at index position idxStr
  {
    ControllerEnvironment ce =
         ControllerEnvironment.getDefaultEnvironment();
    Controller[] cs = ce.getControllers();
    if (cs.length == 0) {
      System.out.println("No controllers found");
      System.exit(0);
    }
    else
      System.out.println("No. of controllers: " + cs.length);

    // get the specified controller using the index value
    int index = extractIndex(idxStr, cs.length);  // string --> integer
    Controller c = cs[index];
    System.out.println("Polling controller: " + c.getName() + ", " + c.getType());

    return c;
  }  // end of getController()


  private static int extractIndex(String s, int len)
  /* Extract the index integer from the string, and check that
     it's valid, i.e. between 0 and len-1.
  */
  {
    int index = 0;
    try {
      index = Integer.parseInt(s);
    }
    catch (NumberFormatException e) 
    {  System.out.println("Incorrect index format; using 0");  }

    if (index < 0 || index >= len) {
      System.out.println("Index not between 0 and " + (len-1) +
              "; using 0");
      index = 0;
    }
    return index;
  }  // end of extractIndex()


  private static Component getComponent(String idxStr, Controller c)
  // return the component at index position idxStr in the controller c
  {
    // get all the components for the controller
    Component[] comps = c.getComponents();
    if (comps.length == 0) {
      System.out.println("No components found");
      System.exit(0);
    }
    else
      System.out.println("No. of components: " + comps.length);

    // get the component using the arg integer value
    int index = extractIndex(idxStr, comps.length);  // string --> integer
    Component component = comps[index];
    System.out.println("Component: " + component.getName());

    return component;
  }  // end of getComponent()


  private static void pollComponent(Controller c, Component component)
  /* Repeatedly poll the specified component
     of the controller, sleeping for DELAY ms between each polling.

     Only print the polled value if it has changed since the last
     polling, and it's not 0. This means that continually 'pressing'
     the component will not generate multiple values, and releasing
     the component, which generates a 0, will not trigger an output.
  */
  {
    float prevValue = 0.0f;
    float currValue;
    boolean pollIsValid;  // new

    int i = 1;   // used to format the output
    while (true) {
      try {
        Thread.sleep(DELAY);      // wait a while
      } 
      catch (Exception ex) {}

      pollIsValid = c.poll(); // update the controller's components
      if (pollIsValid) {
        currValue = component.getPollData();
        if (currValue != prevValue) {  // value has changed
          if (Math.abs(currValue) > EPSILON) {   
              // don't show component 'releases' that are 0 or near to 0.0f
            System.out.print(currValue + "; ");
            i++;
          }
          prevValue = currValue;
        }
      }
      else
        System.out.println("Controller no longer valid");

      if (i%10 == 0) {   // after several outputs (9), put in a newline
        System.out.println();
        i = 1;
      }
    }
  }  // end of pollComponent()

} // end of TestController class
