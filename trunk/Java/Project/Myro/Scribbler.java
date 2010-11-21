package Myro;

import java.io.*;
import java.util.*;
import java.text.*;  // for decimal formatting (debugging)

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.border.*;

import scribbler.io.*; // for rxtxSerial library
//import javax.comm.*;

/**
 * Class Scribbler defines methods to control and query a Scribbler robot.  The methods  defined follow
 * the IPRE Myro specification as described in "Learning Computing with Robots" by Deepak Kumar.
 * (see www.roboteducation.org )
 * 
 * @author Douglas Harms
 * @version 1.0
 * 
 */
public class Scribbler  {

    // public constants

    /**
     * Constant passed to {@link #getLight getLight} or {@link #getBright getBright} to select the left sensor.
     */
    public static final int SENSOR_LIGHT_LEFT       = 0;

    /**
     * Constant passed to {@link #getLight getLight} or {@link #getBright getBright} to select the center sensor.
     */
    public static final int SENSOR_LIGHT_CENTER     = 1;

    /**
     * Constant passed to {@link #getLight getLight} or {@link #getBright getBright} to select the right sensor.
     */
    public static final int SENSOR_LIGHT_RIGHT      = 2;

    /**
     * Constant passed to {@link #getIR getIR} or {@link #getObstacle getObstacle} to select the left IR sensor.
     */
    public static final int SENSOR_IR_LEFT          = 0;

    /**
     * Constant passed to {@link #getIR getIR} or {@link #getObstacle getObstacle} to select the right IR sensor.
     */
    public static final int SENSOR_IR_RIGHT         = 1;

    /**
     * Constant passed to {@link #getObstacle getObstacle} to select the center IR sensor.
     */
    public static final int SENSOR_IR_CENTER        = 2;

    /**
     * Constant passed to {@link #getLine getLine} to select the left line sensor.
     */
    public static final int SENSOR_LINE_LEFT        = 0;

    /**
     * Constant passed to {@link #getLine getLine} to select the right line sensor.
     */
    public static final int SENSOR_LINE_RIGHT       = 1;

    /**
     * Constant passed to {@link #takePicture takePicture} to select a color image
     */
    public static final int IMAGE_COLOR             = 0;

    /**
     * Constant passed to {@link #takePicture takePicture} to select a gray scale image
     */
    public static final int IMAGE_GRAY              = 1;

    /**
     * Constant passed to {@link #takePicture takePicture} to select a blob image
     */
    public static final int IMAGE_BLOB              = 2;

    /**
     * Constant passed to {@link #setLED setLED} to select the left Scribbler LED
     */
    public static final int LED_LEFT                = 0;

    /**
     * Constant passed to {@link #setLED setLED} to select the center Scribble LED
     */
    public static final int LED_CENTER              = 1;

    /**
     * Constant passed to {@link #setLED setLED} to select the right Scribbler LED
     */
    public static final int LED_RIGHT               = 2;

    /**
     * Constant passed to {@link #setLED setLED} to select all Scribbler LEDs
     */
    public static final int LED_ALL                 = 3;

    /**
     * Constant passed to {@link #setLED setLED} to turn selected LED Off
     */
    public static final int LED_OFF                 = 0;

    /**
     * Constant passed to {@link #setLED setLED} to turn selected LED On
     */
    public static final int LED_ON                  = 1;

    /**
     * Constant passed to {@link #setVolume setVolume} to turn the Scribbler's speaker off
     */
    public static final int VOLUME_OFF              = 0;

    /**
     * Constant passed to {@link #setVolume setVolume} to turn the Scribbler's speaker on
     */
    public static final int VOLUME_ON               = 1;

    /**
     * Construct a Scribbler object and connect it to port portName.  If the connection was successfully made then it
     * is legal to invoke methods that require
     * the scribbler be connected; if the connection was not successful then it is not legal to invoke 
     * methods that require the scribbler to be connected.  Method {@link #scribblerConnected scribblerConnected} can be used to
     * determine if the connection was successfully made.
     * 
     * @param portName  the name of the port the Scribbler is attached to (e.g., "COM1", "/dev/ttyS0")
     */
    public Scribbler(String portName) {

        _scribblerConnected = false;
        _flukeConnected = false;
        _serialPort = null;

        // print a warning if asserts are disabled
        boolean testAssert = false;
        assert testAssert=true;
        if( !testAssert )
        {
            System.out.println("WARNING: asserts are disabled!  You might want to add:");
            System.out.println("\"bluej.vm.args=-ea\" to your bluej.properties file to enable");
            System.out.println("assertion checking.");
        }

        // try to connect to the robot
        connect( portName );
    }

    /**
     * Connect the Scribbler to port portName.  If the Scribbler is already connected to a port it is
     * first closed.  If the connection was successfully made then it is legal to invoke methods that require
     * the scribbler be connected; if the connection was not successful then it is not legal to invoke 
     * methods that require the scribbler to be connected.  Methods {@link #scribblerConnected scribblerConnected}
     * and {@link #flukeConnected flukeConnected} can be used to determine if the connection was successfully made.
     * 
     * @param portName The name of the port the Scribbler is connected to (e.g., "COM1", "/dev/ttyS0")
     * @return true returned iff the connection to the Scribbler was successful
     * 
     */
    public boolean connect( String portName )
    {
        // close the connection if it is currently opened
        if( _scribblerConnected || _flukeConnected )
            close();

        // initalize serial port
        _serialPort = null;
        try {
            _serialPort = new RXTXScribblerPort( portName );
        } catch (PortInUseException e)
        {
            System.out.println("PortInUseException:" + e.getMessage());
            return false;}

        if( _serialPort == null )
        {
            System.out.println("Open on port " + portName + " timed out.");
            return false;
        }

        try {
            // set port parameters
            _serialPort.setSerialPortParams( 57600, RXTXScribblerPort.DATABITS_8, 
                RXTXScribblerPort.STOPBITS_1, 
                RXTXScribblerPort.PARITY_NONE );
            _serialPort.setFlowControlMode( RXTXScribblerPort.FLOWCONTROL_NONE );
        } catch (UnsupportedCommOperationException e)
        {
            System.out.println("UnsupportedCommOperationException:" + e.getMessage());
            return false;
        }

        // get the input and output streams so we can access this port
        _inputStream = _serialPort.getInputStream();
        _outputStream = _serialPort.getOutputStream();

        // flush any garbage left in input buffer
        _flushInput();

        // get info about the robot
        String info = getInfo();
        info = info.toLowerCase();
        if( info.contains("fluke") )
            _flukeConnected = true;
        if( info.contains("scribbler" ) )
            _scribblerConnected = true;

        // Print information messages
        //System.out.println( getName() + " is Ready!!" );
        System.out.println("Info=\"" + getInfo() + "\"" );

        return true;
    }

    /**
     * Close the connection between the computer and the Scribbler.  Any threads associated with this robot
     * (e.g., senses, joystick) will be killed.  After calling close the Scribbler cannot be accessed again unless
     * {@link #connect connect} is called to reestablish the connection.
     * <p><p>
     * It is important to invoke this method at the end of the program.  Failure to do so may cause problems when
     * connecting to the Scribbler in the future.
     */
    public  void close()
    {
        if( _scribblerConnected || _flukeConnected )
        {
            // kill any threads associated with this robot
            if( currentSensesThread != null )
            {
                currentSensesThread.interrupt();
                try
                {
                    currentSensesThread.join();
                } catch (InterruptedException e)
                {
                    System.out.println("While waiting for senses to die, we were interrupted.");
                }
                //System.out.println("Senses thread has died.");
                currentSensesThread = null;
            }

            if( currentJoyStickThread != null )
            {
                currentJoyStickThread.interrupt();
                try
                {
                    currentJoyStickThread.join();
                } catch (InterruptedException e)
                {
                    System.out.println("While waiting for joystick to die, we were interrupted.");
                }
                //System.out.println("JoyStick thread has died.");
                currentJoyStickThread = null;
            }
        }

        // close the port if it's opened
        if( _serialPort != null )
            _serialPort.close();

        // indicate that nothing is connected
        _scribblerConnected = false;
        _flukeConnected = false;
        _serialPort = null;
    }

    /**
     * Returns whether the scribbler is currently connected.
     * 
     * @return true iff the Scribbler is currently connected
     * 
     */
    public boolean scribblerConnected()
    {
        return _scribblerConnected;
    }

    /**
     * Indicates whether the robot has a Fluke board attached to it.
     * 
     * @return true iff a Fluke board is present on the robot
     */
    public boolean flukeConnected()
    {
        return _flukeConnected;
    }

    /**
     * Indicates whether the port connecting to the robot has been opened.  Note that it if true
     * is returned it isn't necessarily know what kind of robot is connected (e.g., scribler or fluke);
     * {@link #flukeConnected flukeConnected} or {@link #scribblerConnected scribblerConnected} should
     * be used to determine this.
     * 
     * @return true iff the connection to the robot has been established.
     */
    public boolean portOpened()
    {
        return _serialPort != null;
    }

    /**
     * resets the Scribbler.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected()
     */
    public  void reset()
    {
        assert scribblerConnected() : "Scribbler not connected";

        _get( SOFT_RESET, 0 );
        //         int[] message = new int[] {SOFT_RESET};
        //         _writePadded( message );
        //         int[] echo = _read( 9 );
        try
        {
            Thread.sleep(1000);  // give scribbler time to reset
        } catch (InterruptedException e) {}
    }

    /**
     * Returns whether the Scribbler has stalled (i.e., stopped moving).  Returns true iff the Scribbler has stalled.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected()
     */
    public  boolean getStall()
    {
        assert scribblerConnected() : "Scribbler not connected";

        return _getAll()[10] != 0;
    }

    /**
     * Returns the state of one of the Scribbler's light sensors.  whichLight specifies the light sensor to query.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected() and whichLight is SENSOR_LIGHT_LEFT (or 0), SENSOR_LIGHT_CENTER (or 1),
     * or SENSOR_LIGHT_RIGHT (or 2).
     * 
     * @param whichLight Specifies the light sensor to query.  Must be SENSOR_LIGHT_LEFT (or 0),
     * SENSOR_LIGHT_CENTER (or 1), or SENSOR_LIGHT_RIGHT (or 2).
     * 
     * @return The value of the selected light sensor.  The value will be non-negative, and a low value indicates
     * bright light, a high value indicates low light.
     */
    public  int getLight( int whichLight ) 
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert SENSOR_LIGHT_LEFT<=whichLight && whichLight<=SENSOR_LIGHT_RIGHT : "Illegal light sensor";

        // set command to be the appropriate Scribbler command
        int command = 0;
        switch( whichLight )
        {
            case 0: command = GET_LIGHT_LEFT; break;
            case 1: command = GET_LIGHT_CENTER; break;
            case 2: command = GET_LIGHT_RIGHT; break;
        }

        // issue the command to the Scribbler and read the response
        int[] data = _get( command, 2 );
        return (data[0] << 8) | data[1];
    }

    /**
     * Returns the state of all three Scribbler light sensors.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected()
     * 
     * @return A three element array.  element 0 contains the value of the left sensor, element 1 contains the value
     * of the center sensor, and element 2 contains the value of the right sensor.  All values are non-negative,
     * and low values indicate bright light, high values indicate low light.
     */
    public  int[] getLight()
    {
        assert scribblerConnected() : "Scribbler not connected";

        int[] retVal = new int[3];
        int[] data = _get( GET_LIGHT_ALL, 6 );
        retVal[0] = (data[0] << 8) | data[1];
        retVal[1] = (data[2] << 8) | data[3];
        retVal[2] = (data[4] << 8) | data[5];
        return retVal;
    }

    /**
     * Returns the state of one of the Scribbler's IR sensors.  whichIR specifies the IR sensor to query.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected() and whichIR is {@link #SENSOR_IR_LEFT SENSOR_IR_LEFT} (or 0)
     *  or {@link #SENSOR_IR_RIGHT SENSOR_IR_RIGHT} (or 1).
     * 
     * @param whichIR Specifies the IR sensor to query.  Should be {@link #SENSOR_IR_LEFT SENSOR_IR_LEFT} (or 0)
     *  or {@link #SENSOR_IR_RIGHT SENSOR_IR_RIGHT} (or 1).
     * 
     * @return The value of the selected IR sensor. True means that an obstacle is NOT detected by the selected
     * IR sensor, and false means that an obstacle IS detected by the sensor.
     */
    public  boolean getIR( int whichIR ) 
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert whichIR>=SENSOR_IR_LEFT && whichIR<=SENSOR_IR_RIGHT;

        int command;
        if( whichIR == SENSOR_IR_LEFT )
            command = GET_IR_LEFT;
        else
            command = GET_IR_RIGHT;

        int[] data = _get( command, 1 );
        return data[0] == 1;
    }

    /**
     * Returns the state of both of the Scribbler's IR sensors.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected()
     * 
     * @return A two element boolean array containing the values of the IR sensort. True means that an obstacle is
     * NOT detected by the selected IR sensor, and false means that an obstacle IS detected by the sensor.
     */
    public  boolean[] getIR()
    {
        assert scribblerConnected() : "Scribbler not connected";

        boolean[] retVal = new boolean[2];
        int[] data = _get( GET_IR_ALL, 2 );
        retVal[0] = data[0] == 1;
        retVal[1] = data[1] == 1;
        return retVal;
    }

    /**
     * Returns the state of one of the Scribbler's line sensors.  whichSensor specifies the line sensor to query.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected() and whichSensor is {@link #SENSOR_LINE_LEFT SENSOR_LINE_LEFT} (or 0)
     *  or {@link #SENSOR_LINE_RIGHT SENSOR_LINE_RIGHT} (or 1).
     * 
     * @param whichSensor Specifies the line sensor to query.  Should be {@link #SENSOR_LINE_LEFT SENSOR_LINE_LEFT} (or 0)
     *  or {@link #SENSOR_LINE_RIGHT SENSOR_LINE_RIGHT} (or 1).
     * 
     * @return The value of the selected line sensor. True means that a (dark) line is detected by the selected
     * line sensor, and false means that a (dark) line is not detected by the sensor.
     */
    public  boolean getLine( int whichSensor ) 
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert whichSensor>=SENSOR_LINE_LEFT && whichSensor<=SENSOR_LINE_RIGHT;

        int command;
        if( whichSensor == SENSOR_LINE_LEFT )
            command = GET_LINE_LEFT;
        else
            command = GET_LINE_RIGHT;

        int[] data = _get( command, 1 );
        return data[0] == 1;
    }

    /**
     * Returns the state of both of the Scribbler's line sensors.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected()
     * 
     * @return A two element boolean array containing the values of the line sensort. True means that a (dark) line
     * is detected by the selected line sensor, and false means that a (dark) line is not detected by the sensor.
     */
    public  boolean[] getLine()
    {
        assert scribblerConnected() : "Scribbler not connected";

        boolean[] retVal = new boolean[2];
        int[] data = _get( GET_LINE_ALL, 2 );
        retVal[0] = data[0] == 1;
        retVal[1] = data[1] == 1;
        return retVal;
    }

    /**
     * Returns the info string provided by the Scribbler.  The specific information contains such things as the 
     * firmware version, the type of robot (i.e., Scribbler) and the communication mode (e.g., Serial).
     * <p><p>
     * <b>Precondition:</b> portOpened
     * 
     * @return A String containing information about the connected robot, such as robot type (e.g., Scribbler),
     * firmware version number, and communication mode (e.g., Serial).
     */
    public String getInfo()
    {
        int[] info = _getLine( GET_INFO );

        assert portOpened() : "The port is not opened";

        // create String from the data, using a temp byte array
        byte[] temp = new byte[ info.length ];
        for( int i=0;i<info.length; i++ )
            temp[i] = (byte)info[i];
        String retVal = new String(temp);

        // if the scribbler is connected then the first 9 characters are the echo of the command, so we'll
        // remove them
        if( scribblerConnected() )
            retVal = retVal.substring( 9 );

        return retVal;
    }

    /**
     * Returns the four "fudge factors" used to tweak the motors.  Each value is between 0.0 (inclusive) and 2.0
     * (inclusive). A value of 1.0 indicates no tweaking, values between 0.0 and 1.0 indicate a leftward adjustment,
     * and values between 1.0 and 2.0 indicate a rightward adjustment.  The further a value is away from 1.0, the
     * larger the adjustment.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected()
     * 
     * @return A four element array.  Element 0 is the adjustment for high forward speeds (i.e., > 0.5), element 1
     * is the adjustment for slow forward speeds (i.e., &lt;= 0.5), element 2 is the adjustment for high backward
     * speeds, element 3 is the adjustment for slow backward speeds.
     */
    public double[] getFudge()
    {
        int[] data;
        double[] retVal = new double[4];

        assert scribblerConnected() : "Scribbler not connected";

        // fundge factors are in locations 0-4
        data = _get( GET_DATA, 8 );
        for(int i=0; i<4; i++ )
        {
            if( data[i] == 0 )
                data[i] = 127;
            retVal[i] = data[i] / 127.0;
        }

        return retVal;
    }

    /**
     * Sets the four "fudge factors" for tweaking the motors.  Each value is between 0.0 (inclusive) and 2.0
     * (inclusive). A value of 1.0 indicates no tweaking, values between 0.0 and 1.0 indicate a leftward adjustment,
     * and values between 1.0 and 2.0 indicate a rightward adjustment.  The further a value is away from 1.0, the
     * larger the adjustment.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected and all four parameters between 0.0 (inclusive) and 2.0 (inclusive)
     * 
     * @param fastForward Tweak value for fast forward speeds (i.e., speed > 0.5 )
     * @param slowForward Tweak value for slow forward speeds (i.e., speed &lt;= 0.5 )
     * @param fastBackward Tweak value for fast backward speeds (i.e., speed > 0.5 )
     * @param slowBackward Tweak value for slow backward speeds (i.e., speed &lt;= 0.5 )
     */
    public void setFudge( double fastForward, double slowForward, double fastBackward, double slowBackward )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert (fastForward >= 0.0) && (fastForward <= 2.0) : "fastForward tweak value out of range";
        assert (slowForward >= 0.0) && (slowForward <= 2.0) : "slowForward tweak value out of range";
        assert (fastBackward >= 0.0) && (fastBackward <= 2.0) : "fastBackward tweak value out of range";
        assert (slowBackward >= 0.0) && (slowBackward <= 2.0) : "slowBackward tweak value out of range";

        _set( SET_SINGLE_DATA, 0, (int)(fastForward*127.0) );
        _set( SET_SINGLE_DATA, 1, (int)(slowForward*127.0) );
        _set( SET_SINGLE_DATA, 2, (int)(fastBackward*127.0) );
        _set( SET_SINGLE_DATA, 3, (int)(slowBackward*127.0) );
    }

    /**
     * Returns the name of the Scribbler.  The name is set with {@link #setName setName}.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     * @return The name of the Scribbler.
     */
    public  String getName()
    {
        assert scribblerConnected() : "Scribbler not connected";

        // get the 16 character name for the robot
        int[] temp1 = _get( GET_NAME1, 8 );
        int[] temp2 = _get( GET_NAME2, 8 );

        // create a 16 character String from the 16 values, using a 16 byte array as a temp
        byte[] temp = new byte[16];
        for( int i=0; i<8; i++ )
        {
            temp[i] = (byte)temp1[i];
            temp[i+8] = (byte)temp2[i];
        }
        String retVal = new String(temp);
        return retVal.trim();
    }

    /**
     * Sets the name of the Scribbler.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     * @param newName String containing the new name of the Scribbler.  Only the first 16 characters of newName are
     * used.
     */
    public  void setName( String newName )
    {
        assert scribblerConnected() : "Scribbler not connected";

        // copy new name to two int arrays
        int[] name1 = new int[8];
        int[] name2 = new int[8];
        for (int i=0; i<8; i++)
        {
            if( i < newName.length() )
                name1[i] = (int)newName.charAt(i);
            if( (i+8) < newName.length() )
                name2[i] = (int)newName.charAt(i+8);
        }

        // send to robot
        _set( SET_NAME1, name1 );
        _set( SET_NAME2, name2 );
    }

    /**
     * Causes the Scribbler to emit a melodic single frequency tone.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     * @param duration The length of the tone to be emitted, in seconds.
     * @param frequency The frequency of the tone to emit.
     */
    public  void beep( double duration, int frequency )
    {
        assert scribblerConnected() : "Scribbler not connected";

        int durationInt = ((int)(duration*1000.0)) & 0xffff;
        int durationHigh = (int)(durationInt >> 8);
        int durationLow  = (int)(durationInt & 0xff);
        int frequencyHigh = (int)((frequency >> 8) & 0xff);
        int frequencyLow  = (int)(frequency & 0xff);
        int[] data = new int[] {durationHigh, durationLow, frequencyHigh, frequencyLow};
        _set( SET_SPEAKER, data );
    }

    /**
     * Causes the Scribbler to emit a melodic dual frequency tone.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     * @param duration The length of the tone to be emitted, in seconds.
     * @param frequency1 The frequency of one of the tones to emit.
     * @param frequency2 The frequency of the other tone to emit.
     */
    public  void beep ( double duration, int frequency1, int frequency2 )
    {
        assert scribblerConnected() : "Scribbler not connected";

        int durationInt = ((int)(duration*1000.0)) & 0xffff;
        int durationHigh = (int)(durationInt >> 8);
        int durationLow  = (int)(durationInt & 0xff);
        int freq1High = (int)((frequency1 >> 8) & 0xff);
        int freq1Low  = (int)(frequency1 & 0xff);
        int freq2High = (int)((frequency2 >> 8) & 0xff);
        int freq2Low  = (int)(frequency2 & 0xff);
        int[] data = new int[] {durationHigh, durationLow, freq1High, freq1Low, freq2High, freq2Low};
        _set( SET_SPEAKER_2, data );

    }

    public void setVolume( int onOff )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert onOff==VOLUME_OFF || onOff==VOLUME_ON: "setVolume: invalid parameter";

        if( onOff == VOLUME_OFF )
            _set( SET_QUIET );
        else
            _set( SET_LOUD );
    }

    /**
     * Opens a window that continually displays the sensor values of the Scribbler and/or Fluke.  The values are
     * updated every .25 seconds.
     * <p><p>
     * Only one senses window is permitted to be opened for a particular Scribbler/Fluke; no action occurs if this
     * method is invoked when a senses window is already opened.  The window will stay opened until the user closes
     * it (by clicking the window's close icon) or the {@link #close close} method is invoked.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected or flukeConnected
     */
    public  void senses()
    {
        assert scribblerConnected() || flukeConnected(): "Neither Scribbler nor Fluke connected";

        // can only have one senses window open for this robot
        if( currentSensesThread != null && currentSensesThread.isAlive() )
        {
            return;
        }

        // create a thread for the senses window and start it.
        currentSensesThread = new Thread( new sensesThread(this) );
        currentSensesThread.start();
    }

    /**
     * Opens a window that permits the user to control the movement of the Scribbler.  The window allows the user
     * to control the Scribbler using a joystick-like interface, permitting forward, backward, right, and left
     * movement.
     * <p><p>
     * Only one joystick window is permitted to be opened for a particular Scribbler; no action occurs if
     * this method is invoked when a joystick window is already opened.  The window will stay opened until the user
     * closes it (by clicking the window's close icon) or the {@link #close close} method is invoked.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     */
    public void joyStick()
    {
        assert scribblerConnected() : "Scribbler not connected";

        //can only have one joystick thread open for this robot
        if( currentJoyStickThread != null && currentJoyStickThread.isAlive() )
        {
            return;
        }

        // create a thread for this joystick and start it
        currentJoyStickThread = new Thread( new joyStickThread(this) );
        currentJoyStickThread.start();
        //currentJoyStickFrame = new joyStickFrame( this );
    }

    /**
     * Sets one (or all) of the Scribbler's LEDs on or off.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected; position is LED_LEFT (or 0), LED_CENTER (or 1), LED_RIGHT (or 2), or 
     * LED_ALL (or 3); onOff is LED_OFF (or 0) or LED_ON (or 1)
     */
    public void setLED( int position, int onOff )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert position>=LED_LEFT && position<=LED_ALL:"setLED: Postion invalid";
        assert onOff==LED_ON || onOff==LED_OFF : "setLED: onOff value invalid";

        if( onOff == LED_ON )
        {
            switch( position )
            {
                case LED_LEFT: _set( SET_LED_LEFT_ON ); break;
                case LED_CENTER: _set( SET_LED_CENTER_ON ); break;
                case LED_RIGHT: _set( SET_LED_RIGHT_ON ); break;
                case LED_ALL: _set( SET_LED_ALL_ON ); break;
            }
        }
        else
        {
            switch( position )
            {
                case LED_LEFT: _set( SET_LED_LEFT_OFF ); break;
                case LED_CENTER: _set( SET_LED_CENTER_OFF ); break;
                case LED_RIGHT: _set( SET_LED_RIGHT_OFF ); break;
                case LED_ALL: _set( SET_LED_ALL_OFF ); break;
            }
        }
    }

    /**
     * Sets the front LED on the Fluke board on or off.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, onOff either LED_OFF (or 0) or LED_ON (or 1)
     * 
     * @param onOff Specifies whether to turn on the LED (LED_ON) or turn it off (LED_OFF)
     */
    public void setLEDFront ( int onOff )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert onOff==LED_ON || onOff==LED_OFF : "setLEDFront: onOff value invalid";

        if( onOff == LED_OFF )
            _setFluke( SET_DONGLE_LED_OFF );
        else
            _setFluke( SET_DONGLE_LED_ON );
    }

    /**
     * Sets the back LED on the Fluke board to a specified brightness.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, and brightness between 0.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param brightness A value between 0.0 and 1.0 that specifies the brightness of the LED
     */
    public void setLEDBack( double brightness )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert 0.0 <= brightness && brightness <= 1.0 : "setLEDBack: brightness not between 0 and 1";

        // scale brighness value to be an int between 170 and 255
        int level = (int)Math.round( brightness*(255-170) + 170);

        // set the LED's brightness
        _setFluke( SET_DIMMER_LED, level );
    }

    /**
     * Read one of the Fluke's IR obstacle sensors.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, whichSensor is SENSOR_IR_LEFT (or 0), SENSOR_IR_CENTER (or 2), or
     * SENSOR_IR_RIGHT (or 1)
     * 
     * @param whichSensor Selects the Fluke IR sensor.  Should be SENSOR_IR_LEFT (or 0), SENSOR_IR_CENTER (or 2), or
     * SENSIR_IR_RIGHT (or 1)
     * @return The value of the selected sensor.  A low value means there are no obstacles detected, a high value
     * means there is an obstacle detected.  The return value is in the range 0..6400.
     */
    public int getObstacle( int whichSensor )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert whichSensor==SENSOR_IR_LEFT || whichSensor==SENSOR_IR_CENTER || whichSensor==SENSOR_IR_RIGHT :
        "getObstacle: whichSensor not valid";

        // read the appropriate Fluke sensor
        int[] data=null;
        switch( whichSensor ) {
            case SENSOR_IR_LEFT: data = _getFluke( GET_DONGLE_L_IR, 2 ); break;
            case SENSOR_IR_CENTER: data = _getFluke( GET_DONGLE_C_IR, 2 ); break;
            case SENSOR_IR_RIGHT: data = _getFluke( GET_DONGLE_R_IR, 2 ); break;
        }

        // convert the returned byte values to a 16-bit int and return it
        return (data[0]<<8) + data[1];

    }

    /**
     * Sets the power level of the Fluke's IR obstacle sensors.  The default value is 135.  If
     * {@link #getObstacle getObstacle} always reports high values, try lowering the power level; if it always
     * reports very low values, try increasing the power level.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, powerLevel between 0 (inclusive) and 255 (inclusive)
     * 
     * @param powerLevel Specifies the power level
     */
    public void setIRPower( int powerLevel )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert 0<=powerLevel && powerLevel<=255 : "setIRPower: powerLevel not between 0 and 255";

        _setFluke( SET_DONGLE_IR, powerLevel );
    }

    /**
     * Turn off the Fluke camera's auto-exposure, auto-gain, and lower the gain to the specified value.  This is
     * useful when using {@link #getBright getBright} virtual sensors.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, level between 0 (inclusive) and 255 (inclusive)
     * 
     * @param level The camera's gain level, between 0 and 255.
     * 
     */
    public void darkenCamera( int level )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert 0<=level && level<=255 : "darkenCamera: level not between 0 and 255";

        _set_cam_param( CAM_COMA, CAM_COMA_WHITE_BALANCE_OFF );
        _set_cam_param( CAM_COMB, (CAM_COMB_GAIN_CONTROL_OFF & CAM_COMB_EXPOSURE_CONTROL_OFF) );
        _set_cam_param( CAM_GAIN, level);
        _set_cam_param( 1, 0 );
        _set_cam_param( 2, 0 );
        _set_cam_param( CAM_BRT, 0 );  
        _set_cam_param( CAM_EXP, 0 );
    }

    /**
     * Turn on the Fluke camera's auto-exposure, auto-gain, and auto-color-balance.
     * <p><p>
     * <b>Precondition:</b> flukeConnected
     * 
     */
    public void autoCamera()
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";

        _set_cam_param( CAM_GAIN, CAM_GAIN_DEFAULT );
        _set_cam_param( 1, 0x80);
        _set_cam_param( 2, 0x80);
        _set_cam_param( CAM_BRT, CAM_BRT_DEFAULT );
        _set_cam_param( CAM_EXP, CAM_EXP_DEFAULT );
        _set_cam_param( CAM_COMA, CAM_COMA_DEFAULT );
        _set_cam_param( CAM_COMB, CAM_COMB_DEFAULT );

    }

    /**
     * Set the Fluke camera's gain, brightness, and exposore control to specific values.  The default values
     * are: gain:0, brightness: 120, exposure:65.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, all parameters between 0 (inclusive) and 255 (inclusive)
     */
    public void manualCamera( int gain, int brightness, int exposure )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert 0<=gain && gain<=255 : "manualCamera: gain not in 0..255";
        assert 0<=brightness && brightness<=255 : "manualCamera: brightness not in 0..255";
        assert 0<=exposure && exposure<=255 : "manualCamera: exposure not in 0..255";

        _set_cam_param( CAM_COMA, CAM_COMA_WHITE_BALANCE_OFF );
        _set_cam_param( CAM_COMB, (CAM_COMB_GAIN_CONTROL_OFF & CAM_COMB_EXPOSURE_CONTROL_OFF) );
        _set_cam_param( CAM_GAIN, gain );
        _set_cam_param( CAM_BRT, brightness );
        _set_cam_param( CAM_EXP, exposure );
    }

    /**
     * Defines the blob used by {@link #takePicture takePicture}(IMAGE_BLOB).  A blob specifies a range of colors
     * and is usually defined by calling defineBlob or getUserDefinedBlob in a MyroImage.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, blob not null
     * 
     * @param blob Specifies the color range of the blob
     */
    public void configureBlob( MyroBlobSpec blob )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert blob != null : "configureBlob: blob is null";

        final int delay = 90;  // delay must be 90.  Don't know why, but Bad Thing(tm) happen otherwise

        // create array with parameters to the SET_RLE Fluke command
        int[] params = new int[] { delay, blob.threshold, blob.y_low, blob.y_high, blob.u_low, blob.u_high, 
                blob.v_low, blob.v_high };

        // send the SET_RLE command to the FLuke
        _setFluke( SET_RLE, params );
    }

    /**
     * Gets the voltage of the Scribbler's battery.  If the battery voltage drops below ~6.1V the Fluke's back
     * LED will flash to alert you to change (or preferably recharge) the batteries.
     * <p><p>
     * <b>Precondition:</b> flukeConnected
     * 
     * @return The voltage of the Scribbler's battery
     */
    public double getBattery()
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";

        // get the battery voltage data from the Fluke
        int data[] = _getFluke( GET_BATTERY, 2 );

        // convert the raw data to a 16-bit int, then to a voltage.  The constant is from scribbler.py.
        return ((data[0]<<8) + data[1]) / 20.9813;
    }

    /**
     * Read one of the Fluke's virtual light sensors.  The Fluke's virtual light sensors report the total
     * intensity on the left, center, and right sides of the Fluke's camera.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, and whichSensor is SENSOR_LIGHT_LEFT (or 0),
     * SENSOR_LIGHT_CENTER (or 1), or SENSOR_LIGHT_RIGHT (or 2).
     * 
     * @param whichSensor Specifies the sensor to use.  Must be SENSOR_LIGHT_LEFT (or 0), SENSOR_LIGHT_CENTER (or 1),
     * or SENSOR_LIGHT_RIGHT (or 2).
     * 
     * @return The intensity of the light in the selected sensor area.
     */
    public int getBright( int whichSensor )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert whichSensor>=SENSOR_LIGHT_LEFT && whichSensor<=SENSOR_LIGHT_RIGHT: "getBright: whichSensor not valid";

        // define the window of interest.  Note that we're only looking at the intensity (i.e., Y component).
        // We're also always using Fluke image window 0.
        switch( whichSensor ) {
            case 0: _setImageWindow( 0, 1,   0,  84, 191, 2, 1 ); break;
            case 1: _setImageWindow( 0, 84,  0, 170, 191, 2, 1 ); break;
            case 2: _setImageWindow( 0, 170, 0, 254, 191, 2, 1 ); break;
        }

        // read the total intensity of the defined window (i.e., window 0)
        int[] data = _getFluke( GET_WINDOW_LIGHT, 0, 3 );

        // convert the returned 3-byte value to an int and return it
        return (data[0]<<16) | (data[1]<<8) | (data[2]);        
    }
    //---------------------------------------------------------------------------------------------
    //
    // Movement methods
    //
    //---------------------------------------------------------------------------------------------

    /**
     * Starts the Scribbler moving in the specified direction. 
     * The Scribbler will continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, and translate and rotate are both between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param translate Specifies the forward movement speed.  Values > 0 specify forward speed (with 1.0 specifying
     * full forward speed), values &lt; 0 specify backward speed (with -1.0 specifying full backward speed).  0
     * specifies no forward or backward speed.
     * 
     * @param rotate Specifies rotational speed.  Values > 0 specify counterclockwise rotation (with 1.0 specifying
     * full counterclockwise rotation), values &lt; 0 specify clockwise rotation (with -1.0 specifying full
     * clockwise rotation).  0 specifies no rotation at all.
     */
    public  void move( double translate, double rotate )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= translate && translate <= 1.0 : "translate not between -1.0 and 1.0";
        assert -1.0 <= rotate && rotate <= 1.0 : "rotate not between -1.0 and 1.0";

        _adjustSpeed( translate, rotate );
    }

    /**
     * Moves the Scribbler in a forward direction at a specified speed with no rotational movement for a specified
     * amount of time.  The Scribbler will stop moving at the end of the specified time period.  This method will
     * not return until the specified time period has occurred.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the forward speed.  Positive values specify forward movement (1.0 is full forward speed),
     * negative values specify backward movement (-1.0 is full backward speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void forward( double speed, double numSeconds)
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";

        move( speed, 0.0 );
        _wait( numSeconds );
        stop();
    }

    /**
     * Starts the Scribbler moving forward at a specified speed with no rotational movement.  The Scribbler will
     * continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the speed.  Positive values specify forward movement (1.0 is full forward speed),
     * negative values specify backward movement (-1.0 is full backward speed).
     * 
     */
    public  void forward( double speed )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";

        move( speed, 0.0 );
    }

    /**
     * Starts the Scribbler moving forward at full speed with no rotational movement.  The Scribbler will continue to
     * move until another movement method is invoked (e.g., {@link #stop stop}, {@link #move move},
     * {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight},
     * {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     */
    public  void forward()
    {
        assert scribblerConnected() : "Scribbler not connected";

        forward( 1.0 );
    }

    /**
     * Causes the Scribbler to stop moving.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     */
    public  void stop()
    {
        assert scribblerConnected() : "Scribbler not connected";

        _lastRotate = 0.0;
        _lastTranslate = 0.0;
        _set( SET_MOTORS_OFF );
    }

    /**
     * Moves the Scribbler in a backward direction at a specified speed with no rotational movement for a specified
     * amount of time.  The Scribbler will stop moving at the end of the specified time period.  This method will
     * not return until the specified time period has occurred.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the backward speed.  Positive values specify backward movement (1.0 is full backward
     * speed), negative values specify forward movement (-1.0 is full forward speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void backward( double speed, double numSeconds)
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";

        move( -speed, 0.0 );
        _wait( numSeconds );
        stop();
    }

    /**
     * Starts the Scribbler moving backward at a specified speed with no rotational movement.  The Scribbler will
     * continue to move until another movement method is invoked (e.g., {@link #stop stop}, {@link #move move},
     * {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the speed.  Positive values specify backward movement (1.0 is full backward speed),
     * negative values specify forward movement (-1.0 is full forward speed).
     * 
     */
    public  void backward( double speed )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";

        move( -speed, 0.0 );
    }

    /**
     * Starts the Scribbler moving backward at full speed with no rotational movement.  The Scribbler will continue to
     * move until another movement method is invoked (e.g., {@link #stop stop}, {@link #move move},
     * {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     */
    public  void backward()
    {
        assert scribblerConnected() : "Scribbler not connected";

        backward( 1.0 );
    }

    /**
     * Moves the Scribbler in a counterclockwise rotation at a specified speed with no forward or backward movement
     * for a specified amount of time.  The Scribbler will stop moving at the end of the specified time period.  This
     * method will not return until the specified time period has occurred.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the rotational speed.  Positive values specify counterclockwise rotation 
     * (1.0 is full counterclockwise speed),
     * negative values specify clockwise rotation (-1.0 is full clockwise speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void turnLeft( double speed, double numSeconds )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";

        move( 0.0, speed );
        _wait( numSeconds );
        stop();
    }

    /**
     * Moves the Scribbler in a counterclockwise rotation at a specified speed with no forward or backward movement.
     * The Scribbler will continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the rotational speed.  Positive values specify counterclockwise rotation (1.0 is full
     * counterclockwise speed), negative values specify clockwise rotation (-1.0 is full clockwise speed).
     * 
     */
    public  void turnLeft( double speed )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";

        move (0.0, speed );
    }

    /**
     * Moves the Scribbler in a counterclockwise rotation at full speed with no forward or backward movement.
     * The Scribbler will continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     */
    public  void turnLeft()
    {
        assert scribblerConnected() : "Scribbler not connected";

        turnLeft( 1.0 );
    }

    /**
     * Moves the Scribbler in a clockwise rotation at a specified speed with no forward or backward movement
     * for a specified amount of time.  The Scribbler will stop moving at the end of the specified time period.  This
     * method will not return until the specified time period has occurred.
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the rotational speed.  Positive values specify clockwise rotation (1.0 is full
     * clockwise speed), negative values specify counterclockwise rotation (-1.0 is full counterclockwise speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void turnRight( double speed, double numSeconds )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";

        move( 0.0, -speed );
        _wait( numSeconds );
        stop();
    }

    /**
     * Moves the Scribbler in a clockwise rotation at a specified speed with no forward or backward movement. The
     * Scribbler will continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the rotational speed.  Positive values specify clockwise rotation 
     * (1.0 is full clockwise speed),
     * negative values specify counterclockwise rotation (-1.0 is full counterclockwise speed).
     * 
     */
    public  void turnRight( double speed )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";

        move (0.0, -speed );
    }

    /**
     * Moves the Scribbler in a clockwise rotation at full speed with no forward or backward movement.  The Scribbler
     * will continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected
     * 
     */
    public  void turnRight()
    {
        assert scribblerConnected() : "Scribbler not connected";

        turnRight( 1.0 );
    }

    /**
     * Starts the Scribbler moving by specifying the amount of power going to each wheel. The Scribbler will continue
     * to move until another movement method is invoked (e.g., {@link #stop stop}, {@link #move move},
     * {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected, and left and right are both between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param left Specifies the speed of the left wheel.  Values > 0 specify forward speed (with 1.0 specifying
     * full forward speed), values &lt; 0 specify backward speed (with -1.0 specifying full backward speed).  0
     * specifies no forward or backward speed.
     * 
     * @param right Specifies the speed of the right wheel.  Values > 0 specify forward speed (with 1.0 specifying
     * full forward speed), values &lt; 0 specify backward speed (with -1.0 specifying full backward speed).  0
     * specifies no forward or backward speed.
     */
    public  void motors( double left, double right )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= left && left <= 1.0 : "left not between -1.0 and 1.0";
        assert -1.0 <= right && right <= 1.0 : "right not between -1.0 and 1.0";

        double trans = (right + left) / 2.0;
        double rotate = (right - left) / 2.0;
        move( trans, rotate );
    }

    /**
     * Starts the Scribbler moving forward or backward at a specified speed without changing the Scribbler's current 
     * rotational movement.  The Scribbler will continue to move until another movement method is invoked
     * (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, {@link #backward backward},
     * {@link #turnLeft turnLeft}, {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate},
     * {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the speed.  Positive values specify forward movement (1.0 is full forward speed),
     * negative values specify backward movement (-1.0 is full backward speed).
     * 
     */
    public  void translate( double speed )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";

        _adjustSpeed( speed, _lastRotate );
    }

    /**
     * Starts the Scribbler rotating at a specified speed without changing the Scribbler's current forward or backward
     * movement.  The Scribbler will continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight}, {@link #motors motors}, {@link #translate translate}, {@link #rotate rotate} ).
     * <p><p>
     * <b>Precondition:</b> scribblerConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the rotational speed.  Positive values specify counterclockwise rotation (1.0 is full
     * counterclockwise speed), negative values specify clockwise rotation (-1.0 is full clockwise speed).
     * 
     */
    public  void rotate( double speed )
    {
        assert scribblerConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";

        _adjustSpeed( _lastTranslate, speed );
    }

    /**
     * Takes a picture with Fluke's camera.  The imageType parameter determines what kind of picture is taken.
     * <p><p>
     * <b>Precondition:</b> flukeConnected, imageType is IMAGE_COLOR (or 0), IMAGE_GRAY (or 1), or
     * IMAGE_BLOB (or 2)
     * 
     * @param imageType Specifies the type of picture to take.  IMAGE_COLOR, IMAGE_GRAY, or IMAGE_BLOB
     */
    public MyroImage takePicture( int imageType )
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";
        assert imageType>=IMAGE_COLOR && imageType<=IMAGE_BLOB: "takePicture: invalid imageType";

        MyroImage retImage=null;

        switch (imageType) {
            case IMAGE_COLOR: retImage = _readColorImage(); break;
            case IMAGE_GRAY:  retImage = _readGrayImage(); break;
            case IMAGE_BLOB:  retImage = _readBlobImage(); break;
        }

        return retImage;
    }

    /**
     * Get info about the blob image.  The returned MyroBlobImageInfo contains the number of pixels in the blob,
     * the average X coordinate and the average Y coordinate.
     * <p><p>
     * <b>Precondition:</b> flukeConnected
     * 
     * @return A MyroBlobImageInfo instance that has information about the blob image (number of pixels in the
     * blob, and average location of the blob)
     */
    public MyroBlobImageInfo getBlob()
    {
        assert flukeConnected() : "Scribbler does not have a Fluke board";

        // get blob information from the Fluke
        int[] data = _getFluke( GET_BLOB, 4 );

        // extract info from the response
        int pixelCount = ( data[0] << 8 ) | data[1];
        int averageX = data[2];
        int averageY = data[3];

        // return the info
        return new MyroBlobImageInfo( pixelCount, averageX, averageY );
    }
    //---------------------------------------------------------------------------------------------
    //
    // private fields and methods
    //
    //---------------------------------------------------------------------------------------------

    private InputStream                             _inputStream;
    private scribbler.io.RXTXScribblerPort          _serialPort;
    private OutputStream                            _outputStream;

    // bytecode constants
    // Scribbler codes
    private static final int SOFT_RESET             = 33;

    private static final int GET_ALL                = 65;
    private static final int GET_LIGHT_LEFT         = 67;
    private static final int GET_LIGHT_CENTER       = 68;
    private static final int GET_LIGHT_RIGHT        = 69;
    private static final int GET_LIGHT_ALL          = 70;
    private static final int GET_IR_LEFT            = 71;
    private static final int GET_IR_RIGHT           = 72;
    private static final int GET_IR_ALL             = 73;
    private static final int GET_LINE_LEFT          = 74;
    private static final int GET_LINE_RIGHT         = 75;
    private static final int GET_LINE_ALL           = 76;
    private static final int GET_NAME1              = 78;
    private static final int GET_NAME2              = 64;
    private static final int GET_STALL              = 79;
    private static final int GET_INFO               = 80;
    private static final int GET_DATA               = 81;
    private static final int GET_PASS1              = 50;
    private static final int GET_PASS2              = 51;

    // Fluke codes
    private static final int GET_RLE                = 82;
    private static final int GET_IMAGE              = 83;
    private static final int GET_WINDOW             = 84;
    private static final int GET_DONGLE_L_IR        = 85;
    private static final int GET_DONGLE_C_IR        = 86;
    private static final int GET_DONGLE_R_IR        = 87;
    private static final int GET_WINDOW_LIGHT       = 88;
    private static final int GET_BATTERY            = 89;
    private static final int GET_SERIAL_MEM         = 90;
    private static final int GET_SCRIB_PROGRAM      = 91;
    private static final int GET_CAM_PARAM          = 92;
    private static final int GET_BLOB               = 95;
    private static final int GET_JPEG_GRAY_HEADER   = 135;
    private static final int GET_JPEG_GRAY_SCAN     = 136;
    private static final int GET_JPEG_COLOR_HEADER  = 137;
    private static final int GET_JPEG_COLOR_SCAN    = 138;

    // Scribbler codes
    private static final int SET_PASS1              = 55;
    private static final int SET_PASS2              = 56;
    private static final int SET_SINGLE_DATA        = 96;
    private static final int SET_DATA               = 97;
    private static final int SET_ECHO_MODE          = 98;
    private static final int SET_LED_LEFT_ON        = 99;
    private static final int SET_LED_LEFT_OFF       = 100;
    private static final int SET_LED_CENTER_ON      = 101;
    private static final int SET_LED_CENTER_OFF     = 102;
    private static final int SET_LED_RIGHT_ON       = 103;
    private static final int SET_LED_RIGHT_OFF      = 104;
    private static final int SET_LED_ALL_ON         = 105;
    private static final int SET_LED_ALL_OFF        = 106;
    private static final int SET_LED_ALL            = 107;
    private static final int SET_MOTORS_OFF         = 108;
    private static final int SET_MOTORS             = 109;
    private static final int SET_NAME1              = 110;
    private static final int SET_NAME2              = 119;
    private static final int SET_LOUD               = 111;
    private static final int SET_QUIET              = 112;
    private static final int SET_SPEAKER            = 113;
    private static final int SET_SPEAKER_2          = 114;

    // Fluke codes
    private static final int SET_DONGLE_LED_ON      = 116;
    private static final int SET_DONGLE_LED_OFF     = 117;
    private static final int SET_RLE                = 118;
    private static final int SET_DONGLE_IR          = 120;
    private static final int SET_SERIAL_MEM         = 121;
    private static final int SET_SCRIB_PROGRAM      = 122;
    private static final int SET_START_PROGRAM      = 123;
    private static final int SET_RESET_SCRIBBLER    = 124;
    private static final int SET_SERIAL_ERASE       = 125;
    private static final int SET_DIMMER_LED         = 126;
    private static final int SET_WINDOW             = 127;
    private static final int SET_FORWARDNESS        = 128;
    private static final int SET_WHITE_BALANCE      = 129;
    private static final int SET_NO_WHITE_BALANCE   = 130;
    private static final int SET_CAM_PARAM          = 131;

    // Fluke camera addresses and associated constants
    private static final int CAM_PID                = 0x0A;
    private static final int CAM_PID_DEFAULT        = 0x76;

    private static final int CAM_VER                = 0x0B;
    private static final int CAM_VER_DEFAULT        = 0x48;

    private static final int CAM_GAIN               = 0x00;
    private static final int CAM_GAIN_DEFAULT       = 0x00;

    private static final int CAM_BRT                = 0x06;
    private static final int CAM_BRT_DEFAULT        = 0x80;

    private static final int CAM_EXP                = 0x10;
    private static final int CAM_EXP_DEFAULT        = 0x41;

    private static final int CAM_COMA               = 0x12;
    private static final int CAM_COMA_DEFAULT       = 0x14;
    private static final int CAM_COMA_WHITE_BALANCE_ON = (CAM_COMA_DEFAULT |  (1 << 2));
    private static final int CAM_COMA_WHITE_BALANCE_OFF = (CAM_COMA_DEFAULT & ~(1 << 2));

    private static final int CAM_COMB               = 0x13;
    private static final int CAM_COMB_DEFAULT       = 0xA3;
    private static final int CAM_COMB_GAIN_CONTROL_ON = (CAM_COMB_DEFAULT |  (1 << 1));
    private static final int CAM_COMB_GAIN_CONTROL_OFF = (CAM_COMB_DEFAULT & ~(1 << 1));
    private static final int CAM_COMB_EXPOSURE_CONTROL_ON = (CAM_COMB_DEFAULT |  (1 << 0));
    private static final int CAM_COMB_EXPOSURE_CONTROL_OFF = (CAM_COMB_DEFAULT & ~(1 << 0));

    // robot state
    private boolean _flukeConnected = false;
    private double _lastTranslate = 0.0;
    private double _lastRotate = 0.0;
    private int[] _lastSensors;     // Included because Myro-Pytyhon had this.  Not sure it's necessary
    private boolean _scribblerConnected = false;       // true=>robot is connected to a scribbler
    private Thread currentSensesThread;
    private Thread currentJoyStickThread;

    /**
     * Write a sequence of ints to the robot.  Note that the difference between _write and _writePadded
     * is that _write will not pad the message to 9 characters.
     */
    private void _write( int[] message )
    {
        //         System.out.print("_write:" + message[0] );
        //         for( int i=1; i<message.length; i++ )
        //             System.out.print( "," + message[i] );
        //         System.out.println();
        // copy ints from message to an array of bytes
        byte[] byteString = new byte[ message.length ];
        for( int i=0; i<message.length; i++ )
            byteString[i] = (byte)(message[i] & 0xff);

        // send message to the robot
        try {                   
            // write string to serial port
            _outputStream.write(byteString);
        } catch (IOException e) {System.out.println("_write:IOException");}
    }

    /**
     * Send the values contained in messageString to the Scribbler.  If the message has less than 9 values then
     * it is padded with zeros so that a 9-byte message is sent.  It is assumed that the values in messageString
     * are all between 0 and 255.
     */
    private void _writePadded(int[] messageString)
    {
        //         System.out.print("_writePadded:" + messageString[0] );
        //         for( int i=1; i<messageString.length; i++ )
        //             System.out.print( "," + messageString[i] );
        //         System.out.println();
        // 
        byte[] byteString = new byte[9];
        // copy messageString to byteString
        for( int i=0; i<messageString.length; i++ )
            byteString[i] = (byte)(messageString[i] & 0xff);

        //         System.out.print("Writing:");    
        //         for (int i=0; i<byteString.length; i++ )
        //             System.out.print( byteString[i] + " ");
        //         System.out.println();

        try {
            // write string to serial port
            _outputStream.write(byteString);
        } catch (IOException e) {System.out.println("_writePadded:IOException");}
    }

    /**
     * Read numBytes from the Scribbler and return an array containing these values.  The returned values will all
     * be between 0 and 255.
     */
    private int[] _read( int numBytes )
    {
        byte[] readBuffer = new byte[numBytes];
        int numBytesRead = 0;

        //         System.out.println("_read, numBytes=" + numBytes);
        try {

            while( numBytesRead < numBytes )
            {
                if (_inputStream.available() > 0)
                {
                    //System.out.println(":");
                    numBytesRead += _inputStream.read(readBuffer,numBytesRead,numBytes-numBytesRead);
                }
                else
                {
                    Thread.yield();
                    //System.out.print(".");
                }
            }

        } catch (IOException e) {System.out.println("IO Exception Raised");}

        // create a int array and move the bytes there
        int[] retVal = new int[numBytes];
        for( int i=0; i<numBytes; i++ )
            retVal[i] = (int)( (int)readBuffer[i]  & 0xff);

        //         System.out.print("Read " + numBytesRead + " of " + numBytes + ": ");
        //         for( int i=0; i<numBytesRead; i++ )
        //             System.out.print(readBuffer[i] + " ");
        //         System.out.println();

        return retVal;
    }

    /**
     * Read bytes from the Scribbler until an eol (ASCII 10, line feed) character is encountered.  The values
     * read (not including the eol) will be returned in the array.  All values in the array will be between
     * 0 and 255.
     */
    private int[] _readLine( )
    {
        final int arrSize = 1000;   // assume no more than this many characters in response
        byte[] readBuffer = new byte[arrSize];
        int numBytesRead = 0;
        final byte newline = 10;  // linefeed
        boolean finished = false;

        //         System.out.println("_read, numBytes=" + numBytes);
        try {

            while( !finished )
            {
                if (_inputStream.available() > 0)
                {
                    //System.out.println(":");
                    numBytesRead += _inputStream.read(readBuffer,numBytesRead,arrSize-numBytesRead);
                    if( readBuffer[numBytesRead-1] == newline )
                        finished = true;
                }
                else
                {
                    Thread.yield();
                    //System.out.print(".");
                }
            }

        } catch (IOException e) {System.out.println("IO Exception Raised");}

        // create a int array and move the bytes there.  (Do not include newline)
        int[] retVal = new int[numBytesRead-1];
        for( int i=0; i<numBytesRead-1; i++ )
            retVal[i] = (int)( (int)readBuffer[i]  & 0xff);

        //         System.out.print("Read " + (numBytesRead-1) + ": ");
        //         for( int i=0; i<numBytesRead-1; i++ )
        //             System.out.print(readBuffer[i] + " ");
        //         System.out.println();

        return retVal;
    }

    /**
     * Read any remaining data from the Scribbler and throw them out.
     */
    private void _flushInput()
    {
        final int arrSize = 1000;   // assume no more than this many characters in response
        byte[] readBuffer = new byte[arrSize];

        try
        {
            if( _inputStream.available() > 0 )
                _inputStream.read( readBuffer );
        } catch (IOException e) {System.out.println("IO Exception Raised");}

    }

    /**
     * Set the Scribbler motors so the robot is moving in the specified direction.
     */
    private void _adjustSpeed(double translate, double rotate )
    {
        _lastTranslate = translate;
        _lastRotate = rotate;
        double left  = Math.min( Math.max( _lastTranslate - _lastRotate, -1.0 ), 1.0 );
        double right = Math.min( Math.max( _lastTranslate + _lastRotate, -1.0 ), 1.0 );
        int leftPower  = (int)((left + 1.0) * 100.0);
        int rightPower = (int)((right + 1.0) * 100.0);
        //System.out.println("leftPower=" + leftPower + ", rightPower=" + rightPower);
        _set( SET_MOTORS, rightPower, leftPower );

    }

    /**
     * Send one of the parameterless set commands to the Scribbler.  Parameter command contains the opcode.  The
     * command echo from the Scribbler is read and a sanity check is performed on the echo.The 11 byte response is
     * also read from the Scribbler and stored in instance field _lastSensors.  Only one thread can can communicate 
     * with the Scribbler at a time.
     */
    private synchronized void _set( int command )
    {
        int[] data = new int[8];
        _set( command, data );
    }

    /**
     * Send one of the two parameter set commands to the Scribbler.  The command opcode and the two parameters
     * are passed to this method.  The command echo is read from the Scribbler and a sanity check is performed on
     * the echo.  The 11 byte response is also read from the Scribbler and stored in instance field _lastSensors.
     * Only one thread can can communicate with the Scribbler at a time.
     */
    private synchronized void _set( int command, int value1, int value2 )
    {
        int[] data = new int[] { value1, value2 };
        _set( command, data );
    }

    /**
     * Send one of the set commands to the Scribbler.  The command opcode is passed to the method, and parameters to
     * the command, if any, are contained in an array also passed to the method.  The command echo is read from the
     * Scribbler and a sanity check is performed on the echo.  The 11 byte response is also read from the Scribbler
     * and stored in instance field _lastSensors.  Only one thread can can communicate with the Scribbler at a time.
     */
    private synchronized void _set( int command, int[] values )
    {
        // construct message to scribbler
        int[] message = new int[ values.length + 1];
        message[0] = command;
        for (int i=0; i<values.length && i<8; i++ )
            message[i+1] = values[i];

        // send it, then get echo and response
        _writePadded( message );
        int[] echo = _read( 9 );
        _checkEcho( message, echo );
        _lastSensors = _read( 11 );
    }

    /**
     * Send a get_all command to the Scribbler and store the data received in instance field _lastSensors.  Only one
     * thread at a time can communicate with the Scribbler.
     */
    private synchronized int[] _getAll()
    {
        _lastSensors = _get( GET_ALL, 11 );
        return _lastSensors;
    }

    /**
     * Send a get command to the Scribbler that expects a fixed-size response.  The command opcode is passed to this
     * method as well as the number of bytes expected in the Scribbler's response.  The command echo is read from the
     * Scribbler and a sanity check is performed on the echo.  If there are response bytes expected they are read and
     * stored in an array that is returned to the invoker.  Only one thread at a time can communicate with the
     * Scribbler.
     */
    private synchronized int[] _get( int command, int numResponseBytes )
    {
        int[] retVal = null;
        int[] message = new int[] { command };
        _writePadded( message );
        int[] echo = _read( 9 );
        _checkEcho( message, echo );
        if( numResponseBytes > 0 )
            retVal = _read( numResponseBytes );
        return retVal;
    }

    /**
     * Send a get command to the Scribbler that expects a response terminted by eol.  The command opcode is passed
     * to this method.  The command echo is read from the Scribbler and a sanity check performed on the echo.  The
     * eol-terminate response is read from the Scribbler and returned (without the eol) to the caller.  Only one
     * thread at a time can communicate with the Scribbler.
     */
    private synchronized int[] _getLine( int command )
    {
        int[] retVal = null;
        int[] message = new int[] { command };
        _writePadded( message );
        //int[] echo = _read( 9 );
        //_checkEcho( message, echo );
        retVal = _readLine();
        return retVal;
    }

    /**
     * Send a set command to the Fluke that does not expect a response.  The command opcode is passed to the
     * method.  Only one thread at a time can communicate with the Fluke/Scribbler.
     */
    private synchronized void _setFluke( int command )
    {
        int message[] = new int[] { command };
        _write( message );
    }

    /**
     * Send a set command to the Fluke that does not expect a response.  The command opcode and one byte of data
     * are passed as parameters to this method.  Only one thread at a time can communicate with the Fluke/
     * Scribbler.
     */
    private synchronized void _setFluke( int command, int data )
    {
        int[] message = new int[] { command, data };
        _write( message );
    }

    /**
     * Send a set command to the Fluke that does not expect a response.  The command opcode and the data for the 
     * command are passed as parameters to this method.  Only one thread at a time can communicate with the
     * Fluke/Scribbler.
     */
    private synchronized void _setFluke( int command, int[] data )
    {
        // create the message for the fluke
        int [] message = new int[data.length + 1];
        message[0] = command;
        for( int i=0; i<data.length; i++ )
            message[i+1] = data[i];

        // send the message
        _write( message );
    }

    /**
     * Send a SET_CAM_PARAM command to the Fluke.  The parameter address and value are sent as parameters.
     * The Fluke takes some time (150 msec) to reconfigure itself so we'll be sure to wait after sending the
     * command.
     */
    private void _set_cam_param( int addr, int value )
    {
        // send the command to the Fluke
        int[] data = new int[] { addr, value };
        _setFluke( SET_CAM_PARAM );

        // wait for the Fluke to reconfigure
        _wait( 0.150 );  
    }

    /**
     * Send a get command to the Fluke.  The command opcode is passed to the method as well as the number of
     * response bytes expected.  Only one thread at a time can communicate with the Fluke/Scribbler.
     */
    private synchronized int[] _getFluke( int command, int numResponseBytes )
    {
        int[] message = new int[] { command };
        _write( message );
        return _read( numResponseBytes );
    }

    private synchronized int[] _getFluke( int command, int data, int numResponseBytes)
    {
        int[] message = new int[] { command, data };
        _write( message );
        return _read( numResponseBytes );
    }

    /**
     * Compare the echo with the original message.  If there are differences print a message.
     * 
     * @return true iff the original message and echo are the same.
     */
    private boolean _checkEcho( int[] message, int[]echo )
    {
        // returns true iff message == echo
        boolean echoOK = true;

        // echo should have 9 values
        if( echo.length != 9 )
            echoOK = false;

        // the first bytes should be the same
        int i = 0;
        while( echoOK && (i < message.length) )
        {
            if( message[i] != echo[i] )
                echoOK = false;
            i++;
        }

        // the rest of echo should all be 0's
        while( echoOK && (i < 9) )
        {
            if( echo[i] != 0 )
                echoOK = false;
            i++;
        }

        // print message if there are problems
//         if( !echoOK )
//         {
//             System.out.println("There seems to be problems with the echo :-(");
//             System.out.print("Expected:" );
//             for(int k=0; k< 9; k++ )
//                 if( k < message.length )
//                     System.out.print( message[k] + " ");
//                 else
//                     System.out.print( "0 ");
//             System.out.println();
//             System.out.print("Received:");
//             for( int k=0; k<echo.length; k++ )
//                 System.out.print(echo[k] + " ");
//             System.out.println();
//         }

        return echoOK;
    }

    /**
     * Cause the current thread to sleep for numSeconds.  This method was originally a Myro method, but since the
     * actions of this are implemented by the computer (as opposed to the Scribbler) I decided to make it a 
     * private method used only by the implementation of myro.  Programmers should use java's Thread.sleep method
     * directly.
     */
    private  void _wait( double numSeconds )
    {
        try
        {
            Thread.sleep( (int)(numSeconds * 1000.0) );
        } catch (InterruptedException e) {}

    }

    /**
     * Defines an image window on the fluke.  The image is from (xlow,ylow) to (xhigh,yhigh).  xstep and ystep
     * specify which pixels are included (1=all pixels, 2=every other pixel, etc.).  parameter win specifies which
     * window to define (0..2)
     */
    private void _setImageWindow( int win, int xlow, int ylow, int xhigh, int yhigh, int xstep, int ystep )
    {
        assert 0<=win && win<=2 : "_setImageWindow: win out of range";
        assert flukeConnected() : "_setImageWindow: no fluke on robot";

        int data[] = new int[] { win, xlow, ylow, xhigh, yhigh, xstep, ystep };
        _setFluke( SET_WINDOW, data );
    }

    /**
     * Calcaulate a rgb color based on yuv specification.
     */
    private Color _calcColor( int y, int u, int v )
    {
        int r, g, b;

        // calculate the rgb color based on the current yuv values
        r = Math.max( Math.min( (int)(y + 1.13983 * v), 255 ), 0 );
        g = Math.max( Math.min( (int)(y - 0.39466 * u - 0.58060 * v), 255 ), 0 );
        b = Math.max( Math.min( (int)(y + 2.03211 * u), 255 ), 0 );

        // create a rgb color and return it
        return new Color( r, g, b);
    }

    /**
     * Read the 256x192 color image from the fluke board and return it as a MyroColorImage instance.
     */
    private MyroColorImage _readColorImage( )
    {
        assert flukeConnected() : "_readColorImage: no Fluke on robot";

        int width = 256;
        int height = 192;
        int size = width * height;
        MyroColorImage image = new MyroColorImage( width, height );

        // get the image from the fluke
        int[] pixels = _getFluke( GET_IMAGE, size );

        // translate pixels from yuv to rgb and store in the image
        int cyclePos;  // position in "vyuy vyuy vyuy" cycle
        int y, u, v;  // pixel color in yuv format
        int pos = 0;  // position in pixels array
        for( int row=0; row<height; row++ )
        {
            // the first 4 pixels on each line are calcluated using some pixel values that appear later
            // on the line.  We'll calculate these pixels explicitly, then use the loop to calculate the rest.
            //
            // The first pixels on the line are:
            //    VYUY VYUY
            //    0123 4567
            //
            // pixel 0 (V0 Y1 U2)
            v = pixels[pos] - 128;
            y = pixels[pos+1];
            u = pixels[pos+2] - 128;
            image.set( 0, row, _calcColor( y, u, v ) );

            // pixel 1 (V4 Y1 U2)
            v = pixels[pos+4] - 128;
            image.set( 1, row, _calcColor( y, u, v ) );

            // pixel 2 (V4 Y3 U2)
            y = pixels[pos+3];
            image.set( 2, row, _calcColor( y, u, v ) );

            // pixel 3 (V0 Y3 U2)
            v = pixels[pos] - 128;
            image.set( 3, row, _calcColor( y, u, v ) );

            // After processing the 1st four pixels in this line, we're back at the beginning of the cycle
            cyclePos = 0;

            // We've processed 4 pixels
            pos += 4;

            // process the remaining pixels in this row
            for( int col=4; col<width; col++ )
            {
                // incorporate the current pixel into the yuv specification.
                switch( cyclePos ) {
                    case 0: v = pixels[pos] - 128; break;
                    case 2: u = pixels[pos] - 128; break;
                    case 1:
                    case 3: y = pixels[pos]; break;
                }

                // calculate the rgb color from the yuv and store the pixel in the image
                image.set( col, row, _calcColor( y, u, v) );

                // increment pos and cyclePos
                pos++;
                cyclePos = (cyclePos + 1) % 4;
            }
        }

        return image;
    }

    /**
     * Read a 128x96 gray image from the Fluke and return it as a 256x192 MyroGrayImage.
     */
    private MyroGrayImage _readGrayImage()
    {
        int width = 128;
        int height = 96;
        int size = width * height;

        // set image window 0 to include only Y pixels, every other line
        _setImageWindow( 0, 1, 0, 255, 191, 2, 2 );

        // read pixels from Fluke window 0
        int[] pixels = _getFluke( GET_WINDOW, 0, size );

        // place pixels in a 256x192 MyroGrayImage
        MyroGrayImage image = new MyroGrayImage( 256, 192 );
        int pos = 0;  //current pixel in pixels array

        for( int y=0; y<192; y+=2 )
            for( int x=0; x<256; x+=2 )
            {
                // store current pixel in 4 image pixels
                int value = pixels[pos];
                image.setGray( x,   y,   value );
                image.setGray( x,   y+1, value );
                image.setGray( x+1, y,   value );
                image.setGray( x+1, y+1, value );

                // move to next pixel
                pos++;
        }

        return image;
    }

    /**
     * Read a blob (i.e., RLE) image from the Fluke and return it as a 256x192 MyroGrayImage.  Because we'll
     * need to call i/o routines twice we need to be synchronized.
     */
    private synchronized MyroImage _readBlobImage()
    {
        int width = 256;
        int height = 192;

        // get the RLE image from the Fluke.  The first two response bytes are the reply size
        int[] sizeResponse = _getFluke( GET_RLE, 2 );

        // convert the two bytes to an int
        int size = (sizeResponse[0]<<8) | sizeResponse[1];

        // read the rest of the response
        int[] rle = _read( size );

        // define a grayscale image based on the RLE blob image
        MyroImage image = new MyroGrayImage( width, height );

        int runLength = 0;          // num pixels left in current run
        int pos = 0;                // position in rle
        int color = 255;            // grayscale color of pixel.  Start with white.
        for( int y=0; y<height; y++ )
        {
            for( int x=0; x<width; x+=4 ) // it seems that the RLE is in groups of 4 pixels.  Don't know why.
            {
                if( runLength == 0 )
                {
                    // last run is finished so get length of next run
                    runLength = ( rle[pos] << 8 ) | rle[pos+1];
                    pos += 2;

                    // set color of this run to be the opposite of the previous run
                    color = color ^ 0xff;
                }

                // Each item in the RLE is for a 4-pixel block, so set the next 4 pixels to color
                for( int i=0; i<4;i++ )
                    image.setGray( x+i, y, color );

                // One less item in the current run needs processing now
                runLength--;
            }
        }

        return image;

    }

    /**
     * An instance of this class creates a senses window and a thread that queries the Scribbler every 0.25 seconds
     * and displays the values of all sensors in the window.  The thread will be killed (and the window closed) when
     * the user clicks the window's close icon or the Scribbler's close method is invoked.
     */
    private class sensesThread implements Runnable 
    {
        private Scribbler robot;
        private boolean finished;
        private JFrame frame;

        // fields for Scribbler status
        private JLabel stallValue;
        private JLabel IRLeftValue, IRRightValue;
        private JLabel LineLeftValue, LineRightValue;
        private JLabel LightLeftValue, LightCenterValue, LightRightValue;

        // fields for Fluke status (if there is one)
        private JLabel BrightLeftValue, BrightCenterValue, BrightRightValue;
        private JLabel ObstacleLeftValue, ObstacleCenterValue, ObstacleRightValue;

        /**
         * Construct a JFrame containing fields for the Scribbler's sensor values and set a windowListener that
         * will respond to the window close event.
         */
        public sensesThread(Scribbler _robot)
        {
            Container frameContentPane;
            GridBagLayout gridbag = new GridBagLayout();
            GridBagConstraints c = new GridBagConstraints();
            Border border = LineBorder.createBlackLineBorder();
            JLabel temp;
            int row = 0;

            finished = false;

            robot = _robot;

            frame = new JFrame("Scribbler Sensors");
            frameContentPane = frame.getContentPane();
            frameContentPane.setLayout( gridbag );

            frameContentPane.setFont(new Font("SansSerif", Font.PLAIN, 14));

            c.fill = GridBagConstraints.HORIZONTAL;

            // define Scribbler sensors (if there is a scribbler)
            c.weightx = 1.0;
            if( scribblerConnected() )
            {
                // Stall
                c.gridx = 0;
                c.gridy = row;
                c.gridwidth = 1;
                temp = makeLabel("Stall", border);
                gridbag.setConstraints( temp, c );
                frameContentPane.add( temp );

                stallValue = makeLabel("false", border);
                c.gridx = 1;
                c.gridy = row;
                c.gridwidth = 6;
                gridbag.setConstraints( stallValue, c );
                frameContentPane.add( stallValue );
                row++;

                // IR
                c.weightx = 0.0;                //reset to the default

                c.gridx = 0;
                c.gridy = row;
                c.gridwidth = 1;
                temp = makeLabel("IR", border);
                gridbag.setConstraints( temp, c );
                frameContentPane.add( temp );

                c.gridx = 1;
                c.gridy = row;
                c.gridwidth = 3;
                IRLeftValue = makeLabel("false", border);
                gridbag.setConstraints( IRLeftValue, c );
                frameContentPane.add( IRLeftValue );

                c.gridx = 4;
                c.gridy = row;
                c.gridwidth = 3;
                IRRightValue = makeLabel("false", border);
                gridbag.setConstraints( IRRightValue, c );
                frameContentPane.add( IRRightValue );
                row++;

                // Line
                c.gridx = 0;
                c.gridy = row;
                c.gridwidth = 1;
                temp = makeLabel("Line", border);
                gridbag.setConstraints( temp, c );
                frameContentPane.add( temp );

                c.gridx = 1;
                c.gridy = row;
                c.gridwidth = 3;
                LineLeftValue = makeLabel("false", border);
                gridbag.setConstraints( LineLeftValue, c );
                frameContentPane.add( LineLeftValue );

                c.gridx = 4;
                c.gridy = row;
                c.gridwidth = 3;
                LineRightValue = makeLabel("false", border);
                gridbag.setConstraints( LineRightValue, c );
                frameContentPane.add( LineRightValue );
                row++;

                // Light
                c.gridx = 0;
                c.gridy = row;
                c.gridwidth = 1;
                temp = makeLabel("Light", border);
                gridbag.setConstraints( temp, c );
                frameContentPane.add( temp );

                c.gridx = 1;
                c.gridy = row;
                c.gridwidth = 2;
                LightLeftValue = makeLabel("0", border);
                gridbag.setConstraints( LightLeftValue, c );
                frameContentPane.add( LightLeftValue );

                c.gridx = 3;
                c.gridy = row;
                c.gridwidth = 2;
                LightCenterValue = makeLabel("0", border);
                gridbag.setConstraints( LightCenterValue, c );
                frameContentPane.add( LightCenterValue );

                c.gridx = 5;
                c.gridy = row;
                c.gridwidth = 2;
                LightRightValue = makeLabel("0", border);
                gridbag.setConstraints( LightRightValue, c );
                frameContentPane.add( LightRightValue );
                row++;
            }

            // define Fluke sensors if one is attached
            if( flukeConnected() )
            {
                // Bright
                c.gridx = 0;
                c.gridy = row;
                c.gridwidth = 1;
                temp = makeLabel("Bright", border);
                gridbag.setConstraints( temp, c );
                frameContentPane.add( temp );

                c.gridx = 1;
                c.gridy = row;
                c.gridwidth = 2;
                BrightLeftValue = makeLabel("0", border);
                gridbag.setConstraints( BrightLeftValue, c );
                frameContentPane.add( BrightLeftValue );

                c.gridx = 3;
                c.gridy = row;
                c.gridwidth = 2;
                BrightCenterValue = makeLabel("0", border);
                gridbag.setConstraints( BrightCenterValue, c );
                frameContentPane.add( BrightCenterValue );

                c.gridx = 5;
                c.gridy = row;
                c.gridwidth = 2;
                BrightRightValue = makeLabel("0", border);
                gridbag.setConstraints( BrightRightValue, c );
                frameContentPane.add( BrightRightValue );
                row++;

                // reset weightx to the default if we just filled the first row
                if( row == 1 )
                    c.weightx = 0.0;

                // Obstacle
                c.gridx = 0;
                c.gridy = row;
                c.gridwidth = 1;
                temp = makeLabel("Obstacle", border);
                gridbag.setConstraints( temp, c );
                frameContentPane.add( temp );

                c.gridx = 1;
                c.gridy = row;
                c.gridwidth = 2;
                ObstacleLeftValue = makeLabel("0", border);
                gridbag.setConstraints( ObstacleLeftValue, c );
                frameContentPane.add( ObstacleLeftValue );

                c.gridx = 3;
                c.gridy = row;
                c.gridwidth = 2;
                ObstacleCenterValue = makeLabel("0", border);
                gridbag.setConstraints( ObstacleCenterValue, c );
                frameContentPane.add( ObstacleCenterValue );

                c.gridx = 5;
                c.gridy = row;
                c.gridwidth = 2;
                ObstacleRightValue = makeLabel("0", border);
                gridbag.setConstraints( ObstacleRightValue, c );
                frameContentPane.add( ObstacleRightValue );
                row++;
            }
            frame.pack();
            frame.setVisible( true );

            // create an event handler that will handle the window's close event
            frame.addWindowListener( new windowEventHandler() );

        }

        /**
         * Utility routine to make a label
         */
        private JLabel makeLabel(String caption, Border border)
        {
            JLabel label = new JLabel();
            label.setPreferredSize(new Dimension(100, 20));
            label.setText(caption);
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setBorder( border );

            return label;
        }

        /**
         * Main method to execute in the senses thread.  Query the Scribbler every 0.25 seconds and set the fields
         * in the window accordingly.  If the Sleep method is interrupted it means that the Scribbler's close
         * method has been called; in this case we create a window close event which will be handled exacly as
         * if the user closed the window.
         */
        public void run()
        {
            int[] data;
            Boolean Stall;
            Boolean IRLeft, IRRight;
            Boolean LineLeft,LineRight;
            Integer LightLeft, LightCenter, LightRight;
            Integer BrightLeft, BrightCenter, BrightRight;
            Integer ObstacleLeft, ObstacleCenter, ObstacleRight;

            while( !finished )
            {
                // get the state of the scribbler (if present)and display it
                if( scribblerConnected() )
                {
                    data = robot._getAll();
                    Stall = new Boolean( data[10]!=0 );
                    IRLeft = new Boolean( data[0]!=0 );
                    IRRight = new Boolean( data[1]!=0 );
                    LineLeft = new Boolean( data[8]!=0 );
                    LineRight = new Boolean( data[9]!=0 );
                    LightLeft = new Integer( (data[2]<<8) | data[3] );
                    LightCenter = new Integer( (data[4]<<8) | data[5] );
                    LightRight = new Integer( (data[6]<<8) | data[7] );

                    stallValue.setText( Stall.toString() );
                    IRLeftValue.setText( IRLeft.toString() );
                    IRRightValue.setText( IRRight.toString() );
                    LineLeftValue.setText( LineLeft.toString() );
                    LineRightValue.setText( LineRight.toString() );
                    LightLeftValue.setText( LightLeft.toString() );
                    LightCenterValue.setText( LightCenter.toString() );
                    LightRightValue.setText( LightRight.toString() );
                }

                // get the state of the Fluke sensors (if there is one) and display
                if( flukeConnected() )
                {
                    BrightLeft = new Integer( robot.getBright( 0 ) );
                    BrightCenter = new Integer( robot.getBright( 1 ) );
                    BrightRight = new Integer( robot.getBright( 2 ) );
                    ObstacleLeft = new Integer( robot.getObstacle( 0 ) );
                    ObstacleCenter = new Integer( robot.getObstacle( 1 ) );
                    ObstacleRight = new Integer( robot.getObstacle( 2 ) );
                    BrightLeftValue.setText( BrightLeft.toString() );
                    BrightCenterValue.setText( BrightCenter.toString() );
                    BrightRightValue.setText( BrightRight.toString() );
                    ObstacleLeftValue.setText( ObstacleLeft.toString() );
                    ObstacleCenterValue.setText( ObstacleCenter.toString() );
                    ObstacleRightValue.setText( ObstacleRight.toString() );
                }
                // wait .25 seconds.  If our parent thread interrupts us then we're finished
                try
                {
                    Thread.sleep( 250 );
                } catch (InterruptedException e)
                {
                    // We've been interrupted, so force an event as if the user closed the window
                    frame.getToolkit().getSystemEventQueue().postEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING)); 
                }
            }
        }

        /**
         * The user has closed the window so simply set finished to true, and the next time the loop executes in
         * method run, the thread will terminate.
         */
        private class windowEventHandler extends WindowAdapter
        {
            public void windowClosing(WindowEvent e) 
            {
                finished = true;
            }
        }

    }

    /**
     * An instance of this class creates a joystick window and a thread that uses mouse events to control the Scribbler.
     * The thread will be killed (and the window closed) when the user clicks the window's close icon or the
     * Scribbler's close method is invoked.
     */
    private class joyStickThread implements Runnable
    {
        private Scribbler robot;
        boolean finished;
        joyStickPanel panel;
        int xPos, yPos;
        int panelHeight, panelWidth;
        int panelHalfHeight, panelHalfWidth;
        boolean robotMoving;
        JFrame frame;

        /**
         * Create a window and set up a window listener (to handle the window close event), mouse listeners (to 
         * handle mouse click and drag events), and a component listener (to handle window resize events).
         */
        public joyStickThread( Scribbler _robot )
        {
            Container frameContentPane;
            mouseEventHandler mouseEvent = new mouseEventHandler();
            robot = _robot;
            finished = false;
            robotMoving = false;

            // create the frame and contents
            frame = new JFrame("Scribbler JoyStick");
            frameContentPane = frame.getContentPane();
            frameContentPane.setLayout( new BorderLayout() );

            frameContentPane.add( makeLabel("forward"), BorderLayout.NORTH );
            frameContentPane.add( makeLabel("backward"), BorderLayout.SOUTH );
            frameContentPane.add( makeLabel("right"), BorderLayout.EAST );
            frameContentPane.add( makeLabel("left"), BorderLayout.WEST );

            panel = new joyStickPanel();
            panel.setBorder(  new LineBorder( Color.BLACK, 3) );
            panel.setPreferredSize(new Dimension(300, 300));
            frameContentPane.add( panel, BorderLayout.CENTER );
            // add mouse listener to handle mouse press and release events
            panel.addMouseListener ( mouseEvent );
            // add mouse motion listener to handle mouse drag events
            panel.addMouseMotionListener ( mouseEvent );
            // add a component listener to handle resize events
            panel.addComponentListener( new panelEventHandler() );

            frame.pack();
            frame.setVisible( true );

            // add window listener to handle window close events
            frame.addWindowListener( new windowEventHandler() );

        }

        /**
         * Method executed in the joystick thread.  The listeners do all the work so all we do is sleep all day!
         * If we're interrupted that means the Scribbler's close method has been called, so we force a window
         * close event that will eventually close the window and tell us to stop.
         */
        public void run()
        {
            // loop until the window is closed (which sets finished to true) or our parent thread interrupts us
            while( !finished )
                try
                {
                    Thread.sleep(1000);
                } catch (InterruptedException e)
                { 
                    // We've been interrupted, so handle this as if the user closed the window
                    frame.getToolkit().getSystemEventQueue().postEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING)); 
            }

            //panel.stopThread();
        }

        /**
         * Utility method to make a label.
         */
        private JLabel makeLabel(String caption)
        {
            JLabel label = new JLabel();
            label.setPreferredSize(new Dimension(100, 20));
            label.setText(caption);
            label.setHorizontalAlignment(SwingConstants.CENTER);

            return label;
        }

        private class joyStickPanel extends JPanel
        {
            public void paintChildren( Graphics g )
            {
                Graphics2D g2 = (Graphics2D) g;

                // draw axes
                g2.setColor( Color.BLACK );
                g2.drawLine( panelHalfWidth, 0, panelHalfHeight, panelHeight );
                g2.drawLine( 0, panelHalfHeight, panelWidth, panelHalfHeight );

                // draw line to coordinate if the robot is moving
                if( robotMoving )
                {
                    g2.setColor( Color.RED );
                    g2.setStroke(new BasicStroke(3));
                    g2.drawLine( panelHalfWidth, panelHalfHeight, xPos+panelHalfWidth, panelHalfHeight-yPos );
                }
            }
        }

        /**
         * Process window (actually panel) resize events, which simply means setting instance fields for the 
         * panel dimensions that are used in the repaint method.
         */
        private class panelEventHandler extends ComponentAdapter
        {
            public void componentResized( ComponentEvent e)
            {
                Component c = e.getComponent();
                panelWidth = c.getWidth();
                panelHeight = c.getHeight();
                panelHalfWidth = panelWidth / 2;
                panelHalfHeight = panelHeight / 2;
            }
        }

        /**
         * Handle window close events.  All we do is set instance field finished to true, and the run routine will
         * then terminate the thread.
         */
        private class windowEventHandler extends WindowAdapter
        {
            public void windowClosing(WindowEvent e) 
            {
                finished = true;
                //panel.stopThread();
            }
        }

        private class mouseEventHandler implements MouseListener, MouseMotionListener
        {
            public void mousePressed( MouseEvent e )
            {
                // process the mouse event
                processMouseEvent( e );

                // the robot is now moving
                robotMoving = true;

                // repaint the joystick window to include the mouse press
                panel.repaint();
            }

            public void mouseReleased( MouseEvent e )
            {
                // stop the robot
                robot.stop();

                // the robot is no longer moving
                robotMoving = false;

                // update the joystick window display
                panel.repaint();
            }

            public void mouseDragged( MouseEvent e )
            {
                // process the mouse event
                processMouseEvent( e );

                // redraw the joystick window to show the current position
                panel.repaint();
            }

            private void processMouseEvent( MouseEvent e )
            {
                double translate, rotate;

                //System.out.println("mouseX="+mouseX+", mouseY="+mouseY+", width="+width+", height="+height);

                // get the coordinate
                xPos = e.getX();
                yPos = e.getY();

                // keep it in the panel
                xPos = Math.max( Math.min( xPos, panelWidth ), 0 );
                yPos = Math.max( Math.min( yPos, panelHeight), 0 );

                // transform y so that origin is lower left corner
                yPos = panelHeight - yPos;

                // transform so origin is at center of panel
                xPos = xPos - panelHalfWidth;
                yPos = yPos - panelHalfHeight;       

                // calculate robot translation and rotation
                translate = (double)yPos / (double)panelHalfHeight;
                rotate = (double)xPos / (double)panelHalfWidth;

                // make sure floating point rounding hasn't messed things up
                translate = Math.max( Math.min( translate, 1.0), -1.0 );
                rotate = Math.max( Math.min( rotate, 1.0), -1.0 );

                // move the robot
                //System.out.println("translate="+translate+", rotate="+(-rotate));
                robot.move( translate, -rotate );

            }

            // methods required by MouseMotionListener
            
            public void mouseExited( MouseEvent e )
            {}
            
            public void mouseEntered( MouseEvent e )
            {}
            
            public void mouseClicked( MouseEvent e )
            {}
            
            public void mouseMoved( MouseEvent e )
            {}
            
        }
    }
}