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
    public static final char NO_KEY_PRESSED = (char)0;

    private static char _lastChar;
    private static boolean _keyPressed = false;
    private static boolean _mousePressed = false;

    private simpleKeyListener _keyListener;
    private simpleMouseListener _mouseListener;

    // locks for synchronization of keyboard events and mouse events
    private static Object keyLock;
    private static Object mouseLock;

    // allocate global synchronization locks
    static {
        keyLock = new Object();
        mouseLock= new Object();
    }

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
            synchronized (keyLock)
            {
            _keyPressed = true;
            _lastChar = e.getKeyChar();
            }
        }
    }

    /**
     * Returns true iff a key press event has occurred since the last time isKeyPressed or
     * getKeyPressed was called.  It is not possible to to determine which key was actually pressed;
     * this functionality is only available in method getKeyPressed.
     * 
     */
    public static boolean isKeyPressed()
    {
        synchronized (keyLock)
        {
        if( _keyPressed )
        {
            _keyPressed = false;
            _lastChar = NO_KEY_PRESSED;
            return true;
        }
        return false;
        }
    }

    /** 
     * Determines whether a key has been pressed since the last time isKeyPressed or getKeyPressed
     * was called.  If a key was pressed, then the key that was most recently pressed is returned,
     * otherwise NO_KEY_PRESSED is returned.
     */
    public static char getKeyPressed()
    {
        synchronized (keyLock)
        {
        char retVal = _lastChar;
        _keyPressed = false;
        _lastChar = NO_KEY_PRESSED;
        return retVal;
        }
    }

    private class simpleMouseListener extends MouseAdapter
    {
        public void mousePressed( MouseEvent e )
        {
            synchronized (mouseLock)
            {
            _mousePressed = true;
            }
        }
    }

    /**
     * Returns true iff the mouse has been pressed since the last time isMousePressed was called.
     */
    public static boolean isMousePressed()
    {
        synchronized (mouseLock)
        {
        boolean retVal = _mousePressed;
        _mousePressed = false;
        return retVal;
        }
    }

}
