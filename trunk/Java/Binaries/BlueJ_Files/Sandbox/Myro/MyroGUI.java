/*
 * Myro/Java license - GPL
 * 
 * Myro/Java is a Java implementation of the Myro API, defined by the Institute for Robots in
 * Education (IPRE).  See http://wiki.roboteducation.org for more information.
 * 
 * Copyright 2010-2011 Douglas Harms dharms@depauw.edu
 * 
 * This file is part of Myro/Java.
 * 
 * Myro/Java is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * 
 * Myro/Java is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Myro/Java.  If not, see <http://www.gnu.org/licenses/>.
 */

package Myro;

import javax.swing.*;
import java.awt.*;

/**
 * This class provides Myro/Java programs with some very simple GUI methods
 * 
 * @author Douglas Harms 
 * @version 1.0
 */
public class MyroGUI
{

    /**
     * Constant passed to pickAFile to indicate opening an existing file
     */
    final public static int FILE_OPEN =   0;

    /**
     * Constant passed to pickAFile to indicate opening a file for saving
     */
    final public static int FILE_SAVE =   1;

    /**
     * Presents the user with a question modal dialog box with 1 or more responses.
     * 
     * @pre length of options > 0
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
     * Presents the user with an information modal dialog box with only an OK response.
     * 
     * @param message The text message presented to the user
     */
    public static void tellUser( String message )
    {
        String[] options = new String[] { "OK" };
        showDialogBox( message, options, JOptionPane.INFORMATION_MESSAGE );
    }

    /**
     * Presents user with an input dialog box, allowing her/him to enter a String.
     * 
     * @param message The text message presented to the user
     * @return The String entered by the user, or null if user cancels the input
     */
    public static String inputString( String message )
    {
        return JOptionPane.showInputDialog( message );
    }

    /**
     * Presents user with an input dialog box, allowing her/him to enter an integer.  If the user
     * enters an illegal value, the user is told about this and presented with the original
     * dialog box asking for input.  This continues until the user enters a valid integer or
     * hits the Cancel button, in which case 0 is returned.
     * 
     * @param message The text message presented to the user
     * @return The integer entered by the user.  0 is returned if the user cancels the input.
     */
    public static int inputInteger( String message )
    {
        String response;
        int retVal;

        while (true)
        {
            // present user with input dialog box, prompting with message
            response = inputString( message );

            // process the response
            // user cancelled the input, so just return 0
            if( response == null )
                return 0;

            // try to convert response to int.  If this fails, have user re-enter
            try {
                retVal = Integer.valueOf( response );
                return retVal;
            } catch (NumberFormatException e)
            {
                tellUser( "Improper integer entered.  Please try again." );
            }
        }
    }

    /**
     * Presents user with an input dialog box, allowing her/him to enter a double.  If the user
     * enters an illegal value, the user is told about this and presented with the original
     * dialog box asking for input.  This continues until the user enters a valid double or
     * hits the Cancel button, in which case 0.0 is returned.
     * 
     * @param message The text message presented to the user
     * @return The integer entered by the user.  0.0 is returned if the user cancels the input.
     */
    public static double inputDouble( String message )
    {
        String response;
        double retVal;

        while (true)
        {
            // present user with input dialog box, prompting with message
            response = inputString( message );

            // process the response
            // user cancelled the input, so just return 0
            if( response == null )
                return 0.0;

            // try to convert response to double.  If this fails, have user re-enter
            try {
                retVal = Double.valueOf( response );
                return retVal;
            } catch (NumberFormatException e)
            {
                tellUser( "Improper double entered.  Please try again." );
            }
        }
    }

    /**
     * Display a dialog box allowing the user to select a file for opening or saving.
     * 
     * @pre openType must be MyroGUI.FILE_OPEN (or 0) or MyroGUI.FILE_SAVE (or 1)
     * 
     * @param openType Indicates whether an existing file is being opened (MyroGUI.FILE_OPEN) or
     * a file is being saved (MyroGUI.FILE_SAVE).
     * @return The name of the selected file including the complete path.  If the user cancelled the
     * open or an error occurred, the return value is the empty String.
     */
    public static String pickAFile( int openType )
    {
        assert openType==FILE_OPEN || openType==FILE_SAVE:"openType not valid";

        int status;

        // Create a file chooser
        JFileChooser fc = new JFileChooser();

        // determine which type of open to perform
        if( openType == FILE_OPEN )
        // open an existing file
            status = fc.showOpenDialog( null );
        else
        // choose file to save
            status = fc.showSaveDialog( null );

        // check the status and return an appropriate String
        if( status == JFileChooser.APPROVE_OPTION )
            return fc.getSelectedFile().getPath();
        else
            return "";
    }

    /**
     * Display a dialog box letting the user choose a color.
     * 
     * @return The color selected by the user.  If the user cancelled, black is returned.
     */
    public static Color pickAColor()
    {
        JColorChooser jc = new JColorChooser();

        Color c = jc.showDialog( null, "Myro/Java Color Chooser", Color.white );
        if( c==null )
            return Color.black;
        else
            return c;
    }

    /**
     * Presents the user with a modal dialog box with 1 or more responses.
     * 
     * @pre length of options > 0
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
                options[0]);

        return options[n];
    }
}
