
import java.io.*;
import java.util.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.border.*;

import gnu.io.*; // for rxtxSerial library

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
     * Constant passed to {@link #getLight getLight} to select the left light sensor.
     */
    public static final int SENSOR_LIGHT_LEFT       = 0;
    /**
     * Constant passed to {@link #getLight getLight} to select the center light sensor.
     */
    public static final int SENSOR_LIGHT_CENTER     = 1;
    /**
     * Constant passed to {@link #getLight getLight} to select the right light sensor.
     */
    public static final int SENSOR_LIGHT_RIGHT      = 2;

    /**
     * Constant passed to {@link #getIR getIR} to select the left IR sensor.
     */
    public static final int SENSOR_IR_LEFT          = 0;
    /**
     * Constant passed to {@link #getIR getIR} to select the right IR sensor.
     */
    public static final int SENSOR_IR_RIGHT         = 1;

    /**
     * Constant passed to {@link #getLine getLine} to select the left line sensor.
     */
    public static final int SENSOR_LINE_LEFT        = 0;
    /**
     * Constant passed to {@link #getLine getLine} to select the right line sensor.
     */
    public static final int SENSOR_LINE_RIGHT       = 1;

    /**
     * Construct a Scribbler object and connect it to port portName.  If the connection was successfully made then it
     * is legal to invoke methods that require
     * the scribbler be connected; if the connection was not successful then it is not legal to invoke 
     * methods that require the scribbler to be connected.  Method {@link #isConnected isConnected} can be used to
     * determine if the connection was successfully made.
     * 
     * @param portName  the name of the port the Scribbler is attached to (e.g., "COM1", "/dev/ttyS0")
     */
    public Scribbler(String portName) {

        isOpened = false;
        connect( portName );
    }

    /**
     * Connect the Scribbler to port portName.  If the Scribbler is already connected to a port it is
     * first closed.  If the connection was successfully made then it is legal to invoke methods that require
     * the scribbler be connected; if the connection was not successful then it is not legal to invoke 
     * methods that require the scribbler to be connected.  Method {@link #isConnected isConnected} can be used to
     * determine if the connection was successfully made.
     * 
     * @param portName The name of the port the Scribbler is connected to (e.g., "COM1", "/dev/ttyS0")
     * @return true returned iff the connection to the Scribbler was successful
     * 
     */
    public boolean connect( String portName )
    {
        String portNameLower = portName.toLowerCase();   // ignore case when searching for port
        boolean portFound=false;

        // close the connection if it is currently opened
        if( isOpened )
            close();

        // parse ports, looking for the specified port
        portList = CommPortIdentifier.getPortIdentifiers();
        while (portList.hasMoreElements()) {
            portId = (CommPortIdentifier) portList.nextElement();
            //System.out.println(portId.getName());
            if (portId.getPortType() == CommPortIdentifier.PORT_SERIAL) {
                if (portId.getName().toLowerCase().equals(portNameLower)) {
                    portFound = true;
                    break;
                } 
            } 
        } 

        if (!portFound) {
            System.out.println("port " + portName + " not found.");
            return false;
        } 

        // initalize serial port
        try {
            serialPort = (SerialPort) portId.open("Scribbler", 2000);
        } catch (PortInUseException e)
        {
            System.out.println("PortInUseException");
            return false;}

        if( serialPort == null )
        {
            System.out.println("Open on port " + portName + " timed out.");
            return false;
        }

        try {
            // set port parameters
            serialPort.setSerialPortParams(19200, SerialPort.DATABITS_8, 
                SerialPort.STOPBITS_1, 
                SerialPort.PARITY_NONE);
            serialPort.setFlowControlMode( SerialPort.FLOWCONTROL_NONE );
        } catch (UnsupportedCommOperationException e)
        {
            System.out.println("UnsupportedCommOperationException");
            return false;
        }

        try {
            inputStream = serialPort.getInputStream();
        } catch (IOException e)
        {
            System.out.println("IOException");
            return false;
        }

        try {
            // get the outputstream
            outputStream = serialPort.getOutputStream();
        } catch (IOException e)
        {
            System.out.println("IOException");
            return false;
        }

        // Robot is now open for business!
        isOpened = true;

        // flush any garbage left in input buffer
        _flushInput();

        // make sure the robot is in a reset state
        reset();

        // Set _lastSensors to initial values
        _getAll();

        // Print information messages
        //System.out.println( getName() + " is Ready!!" );
        //System.out.println( getInfo() );

        return true;

    }

    /**
     * Close the connection between the computer and the Scribbler.  Any threads associated with this robot
     * (e.g., senses, joystick) will be killed.  After calling close the Scribbler cannot be
     * accessed again unless {@link #connect connect} is called to reestablish the connection.  
     */
    public  void close()
    {
        if( isOpened )
        {
            // kill threads associated with this robot
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
                System.out.println("Senses thread has died.");
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
                System.out.println("JoyStick thread has died.");
                currentJoyStickThread = null;
            }

        }

        serialPort.close();
        isOpened = false;
    }

    /**
     * Returns whether the scribbler is currently connected.
     * 
     * @return true iff the Scribbler is currently connected
     * 
     */
    public boolean isConnected()
    {
        return isOpened;
    }

    /**
     * resets the Scribbler.
     * <p><p>
     * Precondition: isConnected()
     */
    public  void reset()
    {
        assert isConnected() : "Scribbler not connected";

        _get( SOFT_RESET, 0 );
        //         int[] message = new int[] {SOFT_RESET};
        //         _write( message );
        //         int[] echo = _read( 9 );
        try
        {
            Thread.sleep(1000);  // give scribbler time to reset
        } catch (InterruptedException e) {}
    }

    /**
     * Returns whether the Scribbler has stalled (i.e., stopped moving).  Returns true iff the Scribbler has stalled.
     * <p><p>
     * Precondition: isConnected()
     */
    public  boolean getStall()
    {
        assert isConnected() : "Scribbler not connected";

        return _getAll()[10] != 0;
    }

    /**
     * Returns the state of one of the Scribbler's light sensors.  whichLight specifies the light sensor to query.
     * <p><p>
     * Precondition: isConnected() and whichLight is {@link #SENSOR_LIGHT_LEFT SENSOR_LIGHT_LEFT} (or 0), 
     * {@link #SENSOR_LIGHT_CENTER SENSOR_LIGHT_CENTER} (or 1), or {@link #SENSOR_LIGHT_RIGHT SENSOR_LIGHT_RIGHT} (or 2).
     * 
     * @param whichLight Specifies the light sensor to query.  Should be {@link #SENSOR_LIGHT_LEFT SENSOR_LIGHT_LEFT} (or 0), 
     * {@link #SENSOR_LIGHT_CENTER SENSOR_LIGHT_CENTER} (or 1), or {@link #SENSOR_LIGHT_RIGHT SENSOR_LIGHT_RIGHT} (or 2).
     * 
     * @return The value of the selected light sensor.  The value will be non-negative, and a low value indicates
     * bright light, a high value indicates low light.
     */
    public  int getLight( int whichLight ) 
    {
        assert isConnected() : "Scribbler not connected";
        assert whichLight>=SENSOR_LIGHT_LEFT && whichLight<=SENSOR_LIGHT_RIGHT : "Illegal light sensor";

        // set command to be the appropriate Scribbler command
        int command;
        if( whichLight== SENSOR_LIGHT_LEFT )
            command = GET_LIGHT_LEFT;
        else if( whichLight == SENSOR_LIGHT_CENTER )
            command = GET_LIGHT_CENTER;
        else
            command = GET_LIGHT_RIGHT;

        // issue the command to the Scribbler and read the response
        int[] data = _get( command, 2 );
        return (data[0] << 8) | data[1];
    }

    /**
     * Returns the state of all three Scribbler light sensors.
     * <p><p>
     * Precondition: isConnected()
     * 
     * @return A three element array.  element 0 contains the value of the left sensor, element 1 contains the value
     * of the center sensor, and element 2 contains the value of the right sensor.  All values are non-negative,
     * and low values indicate bright light, high values indicate low light.
     */
    public  int[] getLight()
    {
        assert isConnected() : "Scribbler not connected";

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
     * Precondition: isConnected() and whichIR is {@link #SENSOR_IR_LEFT SENSOR_IR_LEFT} (or 0)
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
        assert isConnected() : "Scribbler not connected";
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
     * Precondition: isConnected()
     * 
     * @return A two element boolean array containing the values of the IR sensort. True means that an obstacle is
     * NOT detected by the selected IR sensor, and false means that an obstacle IS detected by the sensor.
     */
    public  boolean[] getIR()
    {
        assert isConnected() : "Scribbler not connected";

        boolean[] retVal = new boolean[2];
        int[] data = _get( GET_IR_ALL, 2 );
        retVal[0] = data[0] == 1;
        retVal[1] = data[1] == 1;
        return retVal;
    }

    /**
     * Returns the state of one of the Scribbler's line sensors.  whichSensor specifies the line sensor to query.
     * <p><p>
     * Precondition: isConnected() and whichSensor is {@link #SENSOR_LINE_LEFT SENSOR_LINE_LEFT} (or 0)
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
        assert isConnected() : "Scribbler not connected";
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
     * Precondition: isConnected()
     * 
     * @return A two element boolean array containing the values of the line sensort. True means that a (dark) line is detected by the selected
     * line sensor, and false means that a (dark) line is not detected by the sensor.
     */
    public  boolean[] getLine()
    {
        assert isConnected() : "Scribbler not connected";

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
     * Precondition: isConnected()
     * 
     * @return A String containing information about the connected robot, such as robot type (e.g., Scribbler),
     * firmware version number, and communication mode (e.g., Serial).
     */
    public String getInfo()
    {
        int[] info = _getLine( GET_INFO );

        assert isConnected() : "Scribbler not connected";

        // create String from the data, using a temp byte array
        byte[] temp = new byte[ info.length ];
        for( int i=0;i<info.length; i++ )
            temp[i] = (byte)info[i];
        String retVal = new String(temp);
        return retVal;
    }

    /**
     * Returns the four "fudge factors" used to tweak the motors.  Each value is between 0.0 (inclusive) and 2.0 (inclusive). 
     * A value of 1.0
     * indicates no tweaking, values between 0.0 and 1.0 indicate a leftward adjustment, and values between 1.0 and 2.0
     * indicate a rightward adjustment.  The further a value is away from 1.0, the larger the adjustment.
     * <p><p>
     * Precondition: isConnected()
     * 
     * @return A four element array.  Element 0 is the adjustment for high forward speeds (i.e., > 0.5), element 1 is the
     * adjustment for slow forward speeds (i.e., &lt;= 0.5), element 2 is the adjustment for high backward speeds, element 3
     * is the adjustment for slow backward speeds.
     */
    public double[] getFudge()
    {
        int[] data;
        double[] retVal = new double[4];

        assert isConnected() : "Scribbler not connected";

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
     * Sets the four "fudge factors" for tweaking the motors.  Each value is between 0.0 (inclusive) and 2.0 (inclusive). 
     * A value of 1.0
     * indicates no tweaking, values between 0.0 and 1.0 indicate a leftward adjustment, and values between 1.0 and 2.0
     * indicate a rightward adjustment.  The further a value is away from 1.0, the larger the adjustment.
     * <p><p>
     * Precondition: isConnected and all four parameters between 0.0 (inclusive) and 2.0 (inclusive)
     * 
     * @param fastForward Tweak value for fast forward speeds (i.e., speed > 0.5 )
     * @param slowForward Tweak value for slow forward speeds (i.e., speed &lt;= 0.5 )
     * @param fastBackward Tweak value for fast backward speeds (i.e., speed > 0.5 )
     * @param slowBackward Tweak value for slow backward speeds (i.e., speed &lt;= 0.5 )
     */
    public void setFudge( double fastForward, double slowForward, double fastBackward, double slowBackward )
    {
        assert isConnected() : "Scribbler not connected";
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
     * Precondition: isConnected
     * 
     * @return The name of the Scribbler.
     */
    public  String getName()
    {
        assert isConnected() : "Scribbler not connected";

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
     * Precondition: isConnected
     * 
     * @param newName String containing the new name of the Scribbler.  Only the first 16 characters of newName are
     * used.
     */
    public  void setName( String newName )
    {
        assert isConnected() : "Scribbler not connected";

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
     * Precondition: isConnected
     * 
     * @param duration The length of the tone to be emitted, in seconds.
     * @param frequency The frequency of the tone to emit.
     */
    public  void beep( double duration, int frequency )
    {
        assert isConnected() : "Scribbler not connected";

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
     * Precondition: isConnected
     * 
     * @param duration The length of the tone to be emitted, in seconds.
     * @param frequency1 The frequency of one of the tones to emit.
     * @param frequency2 The frequency of the other tone to emit.
     */
    public  void beep ( double duration, int frequency1, int frequency2 )
    {
        assert isConnected() : "Scribbler not connected";

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

    /**
     * Opens a window that continually displays the Scribbler's sensor values.  The values are updated every .25
     * seconds.
     * <p><p>
     * Only one senses window is permitted to be opened for a particular Scribbler; no action occurs if
     * this method is invoked when a senses window is already opened.  The window will stay opened until the user closes
     * it (by clicking the window's close icon) or the {@link #close close} method is invoked.
     * <p><p>
     * Precondition: isConnected
     */
    public  void senses()
    {
        assert isConnected() : "Scribbler not connected";
        
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
     * to control the scribbler using a joystick-like interface, permitting forward, backward, right, and left
     * movement.
     * <p><p>
     * Only one joystick window is permitted to be opened for a particular Scribbler; no action occurs if
     * this method is invoked when a joystick window is already opened.  The window will stay opened until the user closes
     * it (by clicking the window's close icon) or the {@link #close close} method is invoked.
     * <p><p>
     * Precondition: isConnected
     */
    public void joyStick()
    {
        assert isConnected() : "Scribbler not connected";
        
        // can only have one joystick thread open for this robot
        if( currentJoyStickThread != null && currentJoyStickThread.isAlive() )
        {
            return;
        }
        
        // create a thread for this joystick and start it
        currentJoyStickThread = new Thread( new joyStickThread(this) );
        currentJoyStickThread.start();
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
     * {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected, and translate and rotate are both between -1.0 (inclusive) and 1.0 (inclusive)
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
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= translate && translate <= 1.0 : "translate not between -1.0 and 1.0";
        assert -1.0 <= rotate && rotate <= 1.0 : "rotate not between -1.0 and 1.0";
        
        _adjustSpeed( translate, rotate );
    }

    /**
     * Moves the Scribbler in a forward direction at a specified speed with no rotational movement for a specified amount
     * of time.  The Scribbler will stop moving
     * at the end of the specified time period.  This method will not return until the specified time period has
     * occurred.
     * <p><p>
     * Precondition: isConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the forward speed.  Positive values specify forward movement (1.0 is full forward speed),
     * negative values specify backward movement (-1.0 is full backward speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void forward( double speed, double numSeconds)
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";
        
        move( speed, 0.0 );
        _wait( numSeconds );
        stop();
    }

    /**
     * Starts the Scribbler moving forward at a specified speed with no rotational movement.  The Scribbler will continue
     * to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the speed.  Positive values specify forward movement (1.0 is full forward speed),
     * negative values specify backward movement (-1.0 is full backward speed).
     * 
     */
    public  void forward( double speed )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        
        move( speed, 0.0 );
    }

    /**
     * Starts the Scribbler moving forward at full speed with no rotational movement.  The Scribbler will continue to
     * move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected
     * 
     */
    public  void forward()
    {
        assert isConnected() : "Scribbler not connected";
        
        forward( 1.0 );
    }

    /**
     * Causes the Scribbler to stop moving.
     * <p><p>
     * Precondition: isConnected
     */
    public  void stop()
    {
        assert isConnected() : "Scribbler not connected";
        
        _lastRotate = 0.0;
        _lastTranslate = 0.0;
        _set( SET_MOTORS_OFF );
    }

    /**
     * Moves the Scribbler in a backward direction at a specified speed with no rotational movement for a specified
     * amount of time.  The Scribbler will stop moving
     * at the end of the specified time period.  This method will not return until the specified time period has
     * occurred.
     * <p><p>
     * Precondition: isConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the backward speed.  Positive values specify backward movement (1.0 is full backward speed),
     * negative values specify forward movement (-1.0 is full forward speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void backward( double speed, double numSeconds)
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";
        
        move( -speed, 0.0 );
        _wait( numSeconds );
        stop();
    }

    /**
     * Starts the Scribbler moving backward at a specified speed with no rotational movement.  The Scribbler will
     * continue to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the speed.  Positive values specify backward movement (1.0 is full backward speed),
     * negative values specify forward movement (-1.0 is full forward speed).
     * 
     */
    public  void backward( double speed )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        
        move( -speed, 0.0 );
    }

    /**
     * Starts the Scribbler moving backward at full speed with no rotational movement.  The Scribbler will continue to
     * move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected
     * 
     */
    public  void backward()
    {
        assert isConnected() : "Scribbler not connected";
        
        backward( 1.0 );
    }

    /**
     * Moves the Scribbler in a counterclockwise rotation at a specified speed with no forward or backward movement
     * for a specified amount of time.  The Scribbler will stop moving
     * at the end of the specified time period.  This method will not return until the specified time period has
     * occurred.
     * <p><p>
     * Precondition: isConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the rotational speed.  Positive values specify counterclockwise rotation 
     * (1.0 is full counterclockwise speed),
     * negative values specify clockwise rotation (-1.0 is full clockwise speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void turnLeft( double speed, double numSeconds )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";
        
        move( 0.0, speed );
        _wait( numSeconds );
        stop();
    }

    /**
     * Moves the Scribbler in a counterclockwise rotation at a specified speed with no forward or backward movement.
     * The Scribbler will continue to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected, speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the rotational speed.  Positive values specify counterclockwise rotation 
     * (1.0 is full counterclockwise speed),
     * negative values specify clockwise rotation (-1.0 is full clockwise speed).
     * 
     */
    public  void turnLeft( double speed )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        
        move (0.0, speed );
    }

    /**
     * Moves the Scribbler in a counterclockwise rotation at full speed with no forward or backward movement.
     * The Scribbler will continue to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected
     * 
     */
    public  void turnLeft()
    {
        assert isConnected() : "Scribbler not connected";
        
        turnLeft( 1.0 );
    }

    /**
     * Moves the Scribbler in a clockwise rotation at a specified speed with no forward or backward movement
     * for a specified amount of time.  The Scribbler will stop moving
     * at the end of the specified time period.  This method will not return until the specified time period has
     * occurred.
     * <p><p>
     * Precondition: isConnected, speed between -1.0 (inclusive) and 1.0 (inclusive), numSeconds > 0.0
     * 
     * @param speed Specifies the rotational speed.  Positive values specify clockwise rotation 
     * (1.0 is full clockwise speed),
     * negative values specify counterclockwise rotation (-1.0 is full counterclockwise speed).
     * 
     * @param numSeconds Specifies the length of time to move, in seconds.
     */
    public  void turnRight( double speed, double numSeconds )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        assert numSeconds > 0.0 : "numSeconds not > 0.0";
        
        move( 0.0, -speed );
        _wait( numSeconds );
        stop();
    }

    /**
     * Moves the Scribbler in a clockwise rotation at a specified speed with no forward or backward movement.
     * The Scribbler will continue to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected, speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the rotational speed.  Positive values specify clockwise rotation 
     * (1.0 is full clockwise speed),
     * negative values specify counterclockwise rotation (-1.0 is full counterclockwise speed).
     * 
     */
    public  void turnRight( double speed )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        
        move (0.0, -speed );
    }

    /**
     * Moves the Scribbler in a clockwise rotation at full speed with no forward or backward movement.
     * The Scribbler will continue to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected
     * 
     */
    public  void turnRight()
    {
        assert isConnected() : "Scribbler not connected";
        
        turnRight( 1.0 );
    }

    /**
     * Starts the Scribbler moving by specifying the amount of power going to each wheel. 
     * The Scribbler will continue to move until another movement method is invoked (e.g., {@link #stop stop}, 
     * {@link #move move}, {@link #forward forward}, {@link #backward backward}, {@link #turnLeft turnLeft}, 
     * {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected, and left and right are both between -1.0 (inclusive) and 1.0 (inclusive)
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
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= left && left <= 1.0 : "left not between -1.0 and 1.0";
        assert -1.0 <= right && right <= 1.0 : "right not between -1.0 and 1.0";
        
        double trans = (right + left) / 2.0;
        double rotate = (right - left) / 2.0;
        move( trans, rotate );
    }

    /**
     * Starts the Scribbler moving forward or backward at a specified speed without changing the Scribbler's current 
     * rotational movement.  The Scribbler will continue to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the speed.  Positive values specify forward movement (1.0 is full forward speed),
     * negative values specify backward movement (-1.0 is full backward speed).
     * 
     */
    public  void translate( double speed )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        
        _adjustSpeed( speed, _lastRotate );
    }

    /**
     * Starts the Scribbler rotating at a specified speed without changing the Scribbler's current forward or backward
     * movement.  The Scribbler will continue to move until another
     * movement method is invoked (e.g., {@link #stop stop}, {@link #move move}, {@link #forward forward}, 
     * {@link #backward backward}, {@link #turnLeft turnLeft}, {@link #turnRight turnRight} ).
     * <p><p>
     * Precondition: isConnected and speed between -1.0 (inclusive) and 1.0 (inclusive)
     * 
     * @param speed Specifies the rotational speed.  Positive values specify counterclockwise rotation (1.0 is full
     * counterclockwise speed),
     * negative values specify clockwise rotation (-1.0 is full clockwise speed).
     * 
     */
    public  void rotate( double speed )
    {
        assert isConnected() : "Scribbler not connected";
        assert -1.0 <= speed && speed <= 1.0 : "speed not between -1.0 and 1.0";
        
        _adjustSpeed( _lastTranslate, speed );
    }

    //---------------------------------------------------------------------------------------------
    //
    // private fields and methods
    //
    //---------------------------------------------------------------------------------------------

    private static CommPortIdentifier portId;
    private static CommPortIdentifier saveportId;
    private static Enumeration        portList;
    private InputStream              inputStream;
    private SerialPort               serialPort;
    private OutputStream      outputStream;

    // bytecode constants
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

    private static final int SET_SINGLE_DATA        = 96;
    private static final int SET_DATA               = 97;
    private static final int SET_ECHO_MODE          = 98;
    private static final int SET_MOTORS_OFF         = 108;
    private static final int SET_MOTORS             = 109;
    private static final int SET_NAME1              = 110;
    private static final int SET_NAME2              = 119;
    private static final int SET_SPEAKER            = 113;
    private static final int SET_SPEAKER_2          = 114;

    // robot state
    private double _lastTranslate = 0.0;
    private double _lastRotate = 0.0;
    private int[] _lastSensors;
    private boolean isOpened;       // true=>robot is connected and can be accessed
    private Thread currentSensesThread;
    private Thread currentJoyStickThread;

    private void _write(int[] messageString) {
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
            outputStream.write(byteString);
        } catch (IOException e) {System.out.println("_write:IOException");}
    }

    private int[] _read( int numBytes )
    {
        byte[] readBuffer = new byte[numBytes];
        int numBytesRead = 0;

        //         System.out.println("_read, numBytes=" + numBytes);
        try {

            while( numBytesRead < numBytes )
            {
                if (inputStream.available() > 0)
                {
                    //System.out.println(":");
                    numBytesRead += inputStream.read(readBuffer,numBytesRead,numBytes-numBytesRead);
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
                if (inputStream.available() > 0)
                {
                    //System.out.println(":");
                    numBytesRead += inputStream.read(readBuffer,numBytesRead,arrSize-numBytesRead);
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

    private void _flushInput()
    {
        final int arrSize = 1000;   // assume no more than this many characters in response
        byte[] readBuffer = new byte[arrSize];

        try
        {
            if( inputStream.available() > 0 )
                inputStream.read( readBuffer );
        } catch (IOException e) {System.out.println("IO Exception Raised");}

    }

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

    private void _set( int command )
    {
        int[] data = new int[8];
        _set( command, data );
    }

    private void _set( int command, int value1, int value2 )
    {
        int[] data = new int[] { value1, value2 };
        _set( command, data );
    }

    private synchronized void _set( int command, int[] values )
    {
        // construct message to scribbler
        int[] message = new int[ values.length + 1];
        message[0] = command;
        for (int i=0; i<values.length && i<8; i++ )
            message[i+1] = values[i];

        // send it, then get echo and response
        _write( message );
        int[] echo = _read( 9 );
        _checkEcho( message, echo );
        _lastSensors = _read( 11 );
    }

    private int[] _getAll()
    {
        _lastSensors = _get( GET_ALL, 11 );
        return _lastSensors;
    }

    private synchronized int[] _get( int command, int numResponseBytes )
    {
        int[] retVal = null;
        int[] message = new int[] { command };
        _write( message );
        int[] echo = _read( 9 );
        _checkEcho( message, echo );
        if( numResponseBytes > 0 )
            retVal = _read( numResponseBytes );
        return retVal;
    }

    private synchronized int[] _getLine( int command )
    {
        int[] retVal = null;
        int[] message = new int[] { command };
        _write( message );
        int[] echo = _read( 9 );
        _checkEcho( message, echo );
        retVal = _readLine();
        return retVal;
    }

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
        if( !echoOK )
        {
            System.out.println("There seems to be problems with the echo :-(");
            System.out.print("Expected:" );
            for(int k=0; k< 9; k++ )
                if( k < message.length )
                    System.out.print( message[k] + " ");
                else
                    System.out.print( "0 ");
            System.out.println();
            System.out.print("Received:");
            for( int k=0; k<echo.length; k++ )
                System.out.print(echo[k] + " ");
            System.out.println();
        }

        return echoOK;
    }

    private  void _wait( double numSeconds )
    {
        try
        {
            Thread.sleep( (int)(numSeconds * 1000.0) );
        } catch (InterruptedException e) {}

    }

    private class sensesThread implements Runnable 
    {
        private Scribbler robot;
        private JLabel stallValue;
        private JLabel IRLeftValue, IRRightValue;
        private JLabel LineLeftValue, LineRightValue;
        private JLabel LightLeftValue, LightCenterValue, LightRightValue;
        private boolean finished;
        private JFrame frame;

        public sensesThread(Scribbler _robot)
        {
            Container frameContentPane;
            GridBagLayout gridbag = new GridBagLayout();
            GridBagConstraints c = new GridBagConstraints();
            Border border = LineBorder.createBlackLineBorder();
            JLabel temp;

            finished = false;

            robot = _robot;

            frame = new JFrame("Scribbler Sensors");
            frameContentPane = frame.getContentPane();
            frameContentPane.setLayout( gridbag );

            frameContentPane.setFont(new Font("SansSerif", Font.PLAIN, 14));

            c.fill = GridBagConstraints.HORIZONTAL;

            c.weightx = 1.0;
            c.gridx = 0;
            c.gridy = 0;
            c.gridwidth = 1;
            temp = makeLabel("Stall", border);
            gridbag.setConstraints( temp, c );
            frameContentPane.add( temp );

            stallValue = makeLabel("false", border);
            c.gridx = 1;
            c.gridy = 0;
            c.gridwidth = 6;
            gridbag.setConstraints( stallValue, c );
            frameContentPane.add( stallValue );

            c.weightx = 0.0;                //reset to the default

            c.gridx = 0;
            c.gridy = 1;
            c.gridwidth = 1;
            temp = makeLabel("IR", border);
            gridbag.setConstraints( temp, c );
            frameContentPane.add( temp );

            c.gridx = 1;
            c.gridy = 1;
            c.gridwidth = 3;
            IRLeftValue = makeLabel("false", border);
            gridbag.setConstraints( IRLeftValue, c );
            frameContentPane.add( IRLeftValue );

            c.gridx = 4;
            c.gridy = 1;
            c.gridwidth = 3;
            IRRightValue = makeLabel("false", border);
            gridbag.setConstraints( IRRightValue, c );
            frameContentPane.add( IRRightValue );

            c.gridx = 0;
            c.gridy = 2;
            c.gridwidth = 1;
            temp = makeLabel("Line", border);
            gridbag.setConstraints( temp, c );
            frameContentPane.add( temp );

            c.gridx = 1;
            c.gridy = 2;
            c.gridwidth = 3;
            LineLeftValue = makeLabel("false", border);
            gridbag.setConstraints( LineLeftValue, c );
            frameContentPane.add( LineLeftValue );

            c.gridx = 4;
            c.gridy = 2;
            c.gridwidth = 3;
            LineRightValue = makeLabel("false", border);
            gridbag.setConstraints( LineRightValue, c );
            frameContentPane.add( LineRightValue );

            c.gridx = 0;
            c.gridy = 3;
            c.gridwidth = 1;
            temp = makeLabel("Light", border);
            gridbag.setConstraints( temp, c );
            frameContentPane.add( temp );

            c.gridx = 1;
            c.gridy = 3;
            c.gridwidth = 2;
            LightLeftValue = makeLabel("0", border);
            gridbag.setConstraints( LightLeftValue, c );
            frameContentPane.add( LightLeftValue );

            c.gridx = 3;
            c.gridy = 3;
            c.gridwidth = 2;
            LightCenterValue = makeLabel("0", border);
            gridbag.setConstraints( LightCenterValue, c );
            frameContentPane.add( LightCenterValue );

            c.gridx = 5;
            c.gridy = 3;
            c.gridwidth = 2;
            LightRightValue = makeLabel("0", border);
            gridbag.setConstraints( LightRightValue, c );
            frameContentPane.add( LightRightValue );

            frame.pack();
            frame.setVisible( true );

            frame.addWindowListener( new windowEventHandler() );

        }

        private JLabel makeLabel(String caption, Border border)
        {
            JLabel label = new JLabel();
            label.setPreferredSize(new Dimension(100, 20));
            label.setText(caption);
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setBorder( border );

            return label;
        }

        public void run()
        {
            int[] data;
            Boolean Stall;
            Boolean IRLeft, IRRight;
            Boolean LineLeft,LineRight;
            Integer LightLeft, LightCenter, LightRight;

            while( !finished )
            {
                // get the state of the scribbler and display it
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

                // wait .25 seconds.  If our parent thread interrupts us then we're finished
                try
                {
                    Thread.sleep( 250 );
                } catch (InterruptedException e)
                {
                    // We've been interrupted, so force an event as if the user closed the window
                    //System.out.println("We've been interrupted");
                    frame.getToolkit().getSystemEventQueue().postEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING)); 
                }
            }
        }

        private class windowEventHandler extends WindowAdapter
        {
            public void windowClosing(WindowEvent e) 
            {
                finished = true;
            }
        }

    }

    private class joyStickThread implements Runnable
    {
        private Scribbler robot;
        boolean finished;
        JPanel panel;
        int xPos, yPos;
        int panelHeight, panelWidth;
        int panelHalfHeight, panelHalfWidth;
        boolean robotMoving;
        JFrame frame;

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
            panel.addMouseListener (mouseEvent );
            panel.addMouseMotionListener (mouseEvent );
            panel.addComponentListener( new panelEventHandler() );

            frame.pack();
            frame.setVisible( true );

            frame.addWindowListener( new windowEventHandler() );
        }

        public void windowClosing(WindowEvent e) 
        {
            finished = true;
        }

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
                    //System.out.println("JoyStick has been interrupted");
                    frame.getToolkit().getSystemEventQueue().postEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING)); 
            }

        }

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

        private class windowEventHandler extends WindowAdapter
        {
            public void windowClosing(WindowEvent e) 
            {
                finished = true;
            }
        }

        private class mouseEventHandler extends MouseAdapter
        {
            public void mousePressed( MouseEvent e )
            {
                //System.out.println("Press at (" + e.getX() + "," + e.getY() + ")");

                processMouseEvent( e );
                robotMoving = true;

                panel.repaint();

            }

            public void mouseReleased( MouseEvent e )
            {
                // stop the robot
                robot.stop();

                // nothing more to draw in joyStick panel
                robotMoving = false;

                panel.repaint();
            }

            public void mouseDragged( MouseEvent e )
            {
                //System.out.println("Dragged at (" + e.getX() + "," + e.getY() + ")");

                processMouseEvent( e );

                panel.repaint();

            }

            private void processMouseEvent( MouseEvent e )
            {
                double translate, rotate;

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

                // move the robot
                robot.move( translate, -rotate );

                //System.out.println("Translate=" + translate +", rotate="+rotate);

            }

        }
    }
}
