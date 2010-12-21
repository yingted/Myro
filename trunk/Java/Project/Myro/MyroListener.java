package Myro;

/**
 * Write a description of class simpleListeners here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import java.awt.event.*;

public class MyroListener
{
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
    
    public KeyAdapter getKeyListener()
    {
        return _keyListener;
    }
    
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

    public static boolean isKeyPressed()
    {
        return _keyPressed;
    }

    public static char whichKeyPressed()
    {
        assert _keyPressed : "key hasn't been pressed";

        _keyPressed = false;
        return _lastChar;
    }
    
    private class simpleMouseListener extends MouseAdapter
    {
        public void mousePressed( MouseEvent e )
        {
            _mousePressed = true;
        }
    }
    
    public static boolean isMousePressed()
    {
        boolean retVal = _mousePressed;
        _mousePressed = false;
        return retVal;
    }
   
}
