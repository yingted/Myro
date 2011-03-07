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
     * Presents the user with a question modal dialog box with 1 or more responses.
     * <p><p>
     * precondition: length of options > 0
     * 
     * @param message The text presented to the user
     * @param options An array of Strings containing the text of the buttons(s)
     * @return The text of the button the user clicked
     */
    public static String askQuestion( String message, String[] options )
    {
        return showDialogBox( message, options, JOptionPane.QUESTION_MESSAGE );
    }

    /**
     * Presents the user with a question modal dialog box for a yes/no response.
     * 
     * @param message The text message presented to the user
     * @return "Yes" or "no"
     */
    public static String askQuestion( String message )
    {
        String[] options = new String[] { "Yes", "No" };
        return showDialogBox( message, options, JOptionPane.QUESTION_MESSAGE );
    }

    /**
     * Presents the user with an information modal dialog box with only 1 response.
     * 
     * @param message The text message presented to the user
     * @param option1 The text of the single button presented to the user
     */
    public static void tellUser( String message, String option1 )
    {
        String[] options = new String[] { option1 };
        showDialogBox( message, options, JOptionPane.INFORMATION_MESSAGE );
    }

    /**
     * Presents the user with a modal dialog box with 1 or more responses.
     * <p><p>
     * precondition: length of options > 0
     * 
     * @param message The text presented to the user
     * @param options An array of Strings containing the text of the buttons(s)
     * @return The text of the button the user clicked
     */
    private static String showDialogBox( String message, String[] options, int messageType )
    {
        int n = JOptionPane.showOptionDialog(null,
                message,
                "Myro",
                JOptionPane.YES_NO_CANCEL_OPTION,
                //JOptionPane.INFORMATION_MESSAGE,
                messageType,
                null,
                options,
                null);

        return options[n];
    }

}
