package Myro;

/**
 * Write a description of class UserInterface here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import javax.swing.*;

public class MyroGUI
{

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
    
    public static String askQuestion( String message )
    {
        String[] options = new String[] { "Yes", "No" };
        return askQuestion( message, options );
    }
    
    public static String askQuestion( String message, String option1 )
    {
        String[] options = new String[] { option1 };
        return askQuestion( message, options );
    }
    
    
}
