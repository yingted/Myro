package Myro;

import java.awt.event.*;

/**
 * Provides a simple interface to determine whether or not key and mouse events have occurred.  The
 * programmer will associate an instance of MyroListener with each window whose events need monitoring.
 * Static methods isKeyPressed, whichKeyPressed, and isMousePressed will then return an indication
 * of events that have occurred in any associated window.
 * <p>
 * Usage: The following code will set up JFrame j so key/mouse events will be monitored:<p>
 *      MyroListener listener = new MyroListener();<p>
 *      j.addKeyListener( listener.getKeyListener() );<p>
 *      j.addMouseListener( listener.getMouseListener() );<p>
 * 
 * Static methods MyroListener.isKeyPressed(), MyroListener.whicKeyPressed(), and MyroListener.isMousePressed()
 * can be used to determine whether an appropriate event occurred in j.  (Note that it is not possible to 
 * determine in which frame the event occurred.)
 * 
 * @author Douglas Harms 
 * @version 1.0
 */

public class MyroListener
{ 
    /**
     * Constant returned by whichKeyPressed to indicate that no key was pressed.
     */
    public static char NO_KEY_PRESSED = (char)0;

    private static char _lastChar;
    private static boolean _keyPressed = false;
    private static boolean _mousePressed = false;

    private simpleKeyListener _keyListener;
    private simpleMouseListener _mouseListener;

    public MyroListener()
    {
        _keyListener = new simpleKeyListener();
        _mouseListener = new simpleMouseListener();
    }

    /**
     * Returns the KeyAdapter instance created for this instance of MyroListener.
     */
    public KeyAdapter getKeyListener()
    {
        return _keyListener;
    }

    /**
     * Returns the MouseAdapter instance created for this instance of MyroListener.
     */
    public MouseAdapter getMouseListener()
    {
        return _mouseListener;
    }

    private class simpleKeyListener extends KeyAdapter
    {
        public void keyTyped( KeyEvent e )
        {
            _keyPressed = true;
            _lastChar = e.getKeyChar();
        }
    }

    /**
     * Returns true iff a key press event has occurred since the last time isKeyPressed was called.
     */
    public static boolean isKeyPressed()
    {
        if( _keyPressed )
        {
            _keyPressed = false;
            _lastChar = NO_KEY_PRESSED;
            return true;
        }
        return false;
    }

    /** 
     * If a key was pressed since the last time isKeyPressed or whichKeyPressed was called, then
     * the key that was most recently pressed is returned, otherwise NO_KEY_PRESSED is returned.
     */
    public static char whichKeyPressed()
    {
        char retVal = _lastChar;
        _keyPressed = false;
        _lastChar = NO_KEY_PRESSED;
        return retVal;
    }

    private class simpleMouseListener extends MouseAdapter
    {
        public void mousePressed( MouseEvent e )
        {
            _mousePressed = true;
        }
    }

    /**
     * Returns true iff the mouse has been pressed since the last time isMousePressed was called.
     */
    public static boolean isMousePressed()
    {
        boolean retVal = _mousePressed;
        _mousePressed = false;
        return retVal;
    }

}
