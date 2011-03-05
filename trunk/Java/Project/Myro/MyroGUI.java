package Myro;

/**
 * This class provides Myro/Java programs with some very simple GUI methods
 * 
 * @author Douglas Harms 
 * @version 1.0
 */

import javax.swing.*;

public class MyroGUI
{

    /**
     * Presents the user with a modal dialog box with 1 or more responses.
     * <p><p>
     * precondition: length of options > 0
     * 
     * @param message The text presented to the user
     * @param options An array of Strings containing the text of the buttons(s)
     * @return The text of the button the user clicked
     */
    public static String askQuestion( String message, String[] options )
    {

        int n = JOptionPane.showOptionDialog(null,
                message,
                "Myro",
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE,
                null,
                options,
                null);

        return options[n];
    }

    /**
     * Presents the user with a modal dialog box for a yes/no response.
     * 
     * @param message The text message presented to the user
     * @return "Yes" or "no"
     */
    public static String askQuestion( String message )
    {
        String[] options = new String[] { "Yes", "No" };
        return askQuestion( message, options );
    }

    /**
     * Presents the user with a modal dialog box with only 1 response.
     * 
     * @param message The text message presented to the user
     * @param option1 The text of the single button presented to the user
     * @return The text of the button (i.e., the value in parameter option1)
     */
    public static String askQuestion( String message, String option1 )
    {
        String[] options = new String[] { option1 };
        return askQuestion( message, options );
    }

}
