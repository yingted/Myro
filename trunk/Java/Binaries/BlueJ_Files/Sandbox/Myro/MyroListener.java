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

/**
 * USAGE:
 * The
 * programmer will associate an instance of MyroListener with each window whose events need monitoring.
 * Static methods isKeyPressed, whichKeyPressed, and isMousePressed will then return an indication
 * of events that have occurred in any associated window.
 * <p>
 * Usage: The following code will set up JFrame j so key/mouse events will be monitored:<p>
 *      MyroListener listener = new MyroListener();<p>
 *      j.addKeyListener( listener.getKeyListener() );<p>
 *      j.addMouseListener( listener.getMouseListener() );<p>
 */
package Myro;

import java.awt.event.*;
import java.util.LinkedList;
import java.util.Queue;

/**
 * Provides a simple interface to determine whether or not key and mouse events have occurred.  
 * The "robot connected" and MyroCanvas windows will automatically listen for key/mouse events, and
 * other windows can be included too.  (Instructions are included in comments.)
 * 
 * Static methods MyroListener.isKeyPressed(), MyroListener.whichKey(),
 * MyroListener.isMousePressed(), and MyroListener.whichButton()
 * can be used to determine whether an appropriate event occurred in j.  (Note that it is not possible to 
 * determine in which frame the event occurred.)
 * 
 * @author Douglas Harms 
 * @version 1.0
 */

public class MyroListener
{ 
    // constants returned by whichButton
    /**
     * Constant returned by whichButton to indicate the left mouse button was pressed.
     */
    public static final int LEFT_BUTTON     = MouseEvent.BUTTON1;

    /**
     * Constant returned by whichButton to indicate the middle mouse button was pressed.
     */
    public static final int MIDDLE_BUTTON   = MouseEvent.BUTTON2;

    /**
     * Constant returned by whichButton to indicate the right mouse button was pressed.
     */
    public static final int RIGHT_BUTTON    = MouseEvent.BUTTON3;

    private static Queue<Character> _bufferedChar;
    private static char _lastChar;
    private static int _bufferedButton;
    private static int _lastButton;
    private static boolean _mousePressed = false;

    private static simpleKeyListener _keyListener;
    private static simpleMouseListener _mouseListener;

    // locks for synchronization of keyboard events and mouse events
    private static Object _keyLock;
    private static Object _mouseLock;

    // allocate global synchronization locks
    static {
        _keyLock = new Object();
        _mouseLock= new Object();
        _bufferedChar = new LinkedList<Character>();
        _keyListener = new simpleKeyListener();
        _mouseListener = new simpleMouseListener();

    }

    /**
     * Returns the KeyAdapter instance created for this instance of MyroListener.
     */
    public static KeyAdapter getKeyListener()
    {
        return _keyListener;
    }

    /**
     * Returns the MouseAdapter instance created for this instance of MyroListener.
     */
    public static MouseAdapter getMouseListener()
    {
        return _mouseListener;
    }

    /**
     * keyListener to process keyboard events
     */
    private static class simpleKeyListener extends KeyAdapter
    {
        public void keyTyped( KeyEvent e )
        {
            synchronized (_keyLock)
            {
                _bufferedChar.add( e.getKeyChar() );
            }
        }
    }

    /**
     * Returns true iff a key press event has occurred since the last time isKeyPressed was called.
     * When isKeyPressed returns true, whichKey() will return the key that was pressed.
     * 
     * @return True iff a key was pressed since the last time isKeyPressed was invoked.
     * 
     */
    public static boolean isKeyPressed()
    {
        synchronized (_keyLock)
        {
            if( !_bufferedChar.isEmpty() )
            {
                _lastChar = _bufferedChar.remove();
                return true;
            }
            return false;
        }
    }

    /** 
     * Returns the key that was pressed when isKeyPressed() returned true.  If the last invocation of
     * isKeyPressed returned false, the return value of whichKey() is undefined.
     * 
     * @return The key that was pressed the last time isKeyPressed returned true.
     */
    public static char whichKey()
    {
        synchronized (_keyLock)
        {
            return _lastChar;
        }
    }

    /**
     * Flushes any buffered keystrokes that may have been pressed since the last time isKeyPressed()
     * was invoked.
     */
    public static void flushKeys()
    {
        synchronized( _keyLock )
        {
            _bufferedChar.clear();
        }
    }
    
    private static class simpleMouseListener extends MouseAdapter
    {
        public void mousePressed( MouseEvent e )
        {
            synchronized (_mouseLock)
            {
                _mousePressed = true;
                _bufferedButton = e.getButton();
            }
        }
    }

    /**
     * Returns true iff the mouse has been pressed since the last time isMousePressed was called.
     * 
     * @return True iff the mouse has been pressed since the last time isMousePressed was invoked.
     */
    public static boolean isMousePressed()
    {
        synchronized (_mouseLock)
        {
            if( _mousePressed )
            {
                _mousePressed = false;
                _lastButton = _bufferedButton;
                return true;
            }
            else
                return false;
        }
    }

    /**
     * Returns an indication of which button was pressed the last time isMousePressed returned true.
     * If the most recent invocation of isMousePressed returned false, the return value of 
     * whichButton is undefined.
     * 
     * @return The most recent button pressed.  This will be MyroListener.LEFT_BUTTON,
     * MyroListener.MIDDLE_BUTTON, or MyroListener.RIGHT_BUTTON.
     */
    public static int whichButton()
    {
        synchronized( _mouseLock )
        {
            return _lastButton;
        }
    }

}
