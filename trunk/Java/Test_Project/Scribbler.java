// Scribber Test 1
// Douglas Harms
// First test of communication with the scribbler robot.  ASssume only a serial connection (i.e.,
// no Fluke)

import java.io.*;
import java.util.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.border.*;

//import javax.comm.*; // for SUN's serial/parallel port libraries
import gnu.io.*; // for rxtxSerial library

public class Scribbler  {

    // public constants
    public static final int SENSOR_LIGHT_LEFT       = 0;
    public static final int SENSOR_LIGHT_CENTER     = 1;
    public static final int SENSOR_LIGHT_RIGHT      = 2;

    public static final int SENSOR_IR_LEFT          = 0;
    public static final int SENSOR_IR_RIGHT         = 1;

    public static final int SENSOR_LINE_LEFT        = 0;
    public static final int SENSOR_LINE_RIGHT       = 1;

    public Scribbler(String portName) {

        String portNameLower = portName.toLowerCase();   // ignore case when searching for port

        boolean portFound=false;
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
            return;
        } 

        // initalize serial port
        try {
            serialPort = (SerialPort) portId.open("Scribbler", 2000);
        } catch (PortInUseException e) {System.out.println("PortInUseException");}

        if( serialPort == null )
        {
            System.out.println("Open on port " + portName + " timed out.");
            return;
        }

        try {
            // set port parameters
            serialPort.setSerialPortParams(19200, SerialPort.DATABITS_8, 
                SerialPort.STOPBITS_1, 
                SerialPort.PARITY_NONE);
            serialPort.setFlowControlMode( SerialPort.FLOWCONTROL_NONE );
        } catch (UnsupportedCommOperationException e) {System.out.println("UnsupportedCommOperationException");}

        try {
            inputStream = serialPort.getInputStream();
        } catch (IOException e) {System.out.println("IOException");}

        try {
            // get the outputstream
            outputStream = serialPort.getOutputStream();
        } catch (IOException e) {System.out.println("IOException");}

        // flush any garbage left in input buffer
        _flushInput();

        // make sure the robot is in a reset state
        reset();

        // Set _lastSensors to initial values
        getAll();

        // Print information messages
        System.out.println( getName() + " is Ready!!" );
        System.out.println( getInfo() );

    }

    public  void close()
    {
        serialPort.close();
        try
        {
            System.exit(0);
        } catch (SecurityException e) {}
    }

    public  void reset()
    {
        _get( SOFT_RESET, 0 );
        //         int[] message = new int[] {SOFT_RESET};
        //         _write( message );
        //         int[] echo = _read( 9 );
        try
        {
            Thread.sleep(1000);  // give scribbler time to reset
        } catch (InterruptedException e) {}
    }

    public  int[] getAll()
    {
        _lastSensors = _get( GET_ALL, 11 );
        return _lastSensors;
    }

    public  boolean getStall()
    {
        return getAll()[10] != 0;
    }

    public  int getLight( int sensor ) 
    {
        assert sensor>=SENSOR_LIGHT_LEFT && sensor<=SENSOR_LIGHT_RIGHT;

        int command;
        if( sensor == SENSOR_LIGHT_LEFT )
            command = GET_LIGHT_LEFT;
        else if( sensor == SENSOR_LIGHT_CENTER )
            command = GET_LIGHT_CENTER;
        else
            command = GET_LIGHT_RIGHT;

        int[] data = _get( command, 2 );
        return (data[0] << 8) | data[1];
    }

    public  int[] getLight()
    {
        int[] retVal = new int[3];
        int[] data = _get( GET_LIGHT_ALL, 6 );
        retVal[0] = (data[0] << 8) | data[1];
        retVal[1] = (data[2] << 8) | data[3];
        retVal[2] = (data[4] << 8) | data[5];
        return retVal;
    }

    public  boolean getIR( int sensor ) 
    {
        assert sensor>=SENSOR_IR_LEFT && sensor<=SENSOR_IR_RIGHT;

        int command;
        if( sensor == SENSOR_IR_LEFT )
            command = GET_IR_LEFT;
        else
            command = GET_IR_RIGHT;

        int[] data = _get( command, 1 );
        return data[0] == 1;
    }

    public  boolean[] getIR()
    {
        boolean[] retVal = new boolean[2];
        int[] data = _get( GET_IR_ALL, 2 );
        retVal[0] = data[0] == 1;
        retVal[1] = data[1] == 1;
        return retVal;
    }

    public  boolean getLine( int sensor ) 
    {
        assert sensor>=SENSOR_LINE_LEFT && sensor<=SENSOR_LINE_RIGHT;

        int command;
        if( sensor == SENSOR_LINE_LEFT )
            command = GET_LINE_LEFT;
        else
            command = GET_LINE_RIGHT;

        int[] data = _get( command, 1 );
        return data[0] == 1;
    }

    public  boolean[] getLine()
    {
        boolean[] retVal = new boolean[2];
        int[] data = _get( GET_LINE_ALL, 2 );
        retVal[0] = data[0] == 1;
        retVal[1] = data[1] == 1;
        return retVal;
    }

    public String getInfo()
    {
        int[] info = _getLine( GET_INFO );

        // create String from the data, using a temp byte array
        byte[] temp = new byte[ info.length ];
        for( int i=0;i<info.length; i++ )
            temp[i] = (byte)info[i];
        String retVal = new String(temp);
        return retVal;
    }

    public  String getName()
    {
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

    public  void setName( String newName )
    {
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

    public  void beep( double duration, int frequency )
    {
        int durationInt = ((int)(duration*1000.0)) & 0xffff;
        int durationHigh = (int)(durationInt >> 8);
        int durationLow  = (int)(durationInt & 0xff);
        int frequencyHigh = (int)((frequency >> 8) & 0xff);
        int frequencyLow  = (int)(frequency & 0xff);
        int[] data = new int[] {durationHigh, durationLow, frequencyHigh, frequencyLow};
        _set( SET_SPEAKER, data );
    }

    public  void beep ( double duration, int frequency1, int frequency2 )
    {
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

    public  void setEchoMode( boolean on ) {
        int[] message = new int[] { SET_ECHO_MODE , 0};
        if( on )
            message[1] = 1;

        _write( message );
    }

    public  void wait( double seconds )
    {
        try
        {
            Thread.sleep( (int)(seconds * 1000.0) );
        } catch (InterruptedException e) {}

    }

    public  void senses()
    {
        (new Thread( new sensesThread(this) ) ).start();
    }

    public void joyStick()
    {
        (new Thread( new joyStickThread(this) ) ).start();
    }

    //---------------------------------------------------------------------------------------------
    //
    // Movement methods
    //
    //---------------------------------------------------------------------------------------------

    public  void move( double translate, double rotate )
    {
        _adjustSpeed( translate, rotate );
    }

    public  void forward( double speed, double interval)
    {
        move( speed, 0.0 );
        wait( interval );
        stop();
    }

    public  void forward( double speed )
    {
        move( speed, 0.0 );
    }

    public  void forward()
    {
        forward( 1.0 );
    }

    public  void stop()
    {
        _lastRotate = 0.0;
        _lastTranslate = 0.0;
        _set( SET_MOTORS_OFF );;
    }

    public  void backward( double speed, double interval)
    {
        move( -speed, 0.0 );
        wait( interval );
        stop();
    }

    public  void backward( double speed )
    {
        move( -speed, 0.0 );
    }

    public  void backward()
    {
        backward( 1.0 );
    }

    public  void turnLeft( double speed, double interval )
    {
        move( 0.0, speed );
        wait( interval );
        stop();
    }

    public  void turnLeft( double speed )
    {
        move (0.0, speed );
    }

    public  void turnLeft()
    {
        turnLeft( 1.0 );
    }

    public  void turnRight( double speed, double interval )
    {
        move( 0.0, -speed );
        wait( interval );
        stop();
    }

    public  void turnRight( double speed )
    {
        move (0.0, -speed );
    }

    public  void turnRight()
    {
        turnRight( 1.0 );
    }

    public  void motors( double left, double right )
    {
        double trans = (right + left) / 2.0;
        double rotate = (right - left) / 2.0;
        move( trans, rotate );
    }

    public  void translate( double speed )
    {
        _adjustSpeed( speed, _lastRotate );
    }

    public  void rotate( double amount )
    {
        _adjustSpeed( _lastTranslate, amount );
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

    private class sensesThread implements Runnable 
    {
        private Scribbler robot;
        private JLabel stallValue;
        private JLabel IRLeftValue, IRRightValue;
        private JLabel LineLeftValue, LineRightValue;
        private JLabel LightLeftValue, LightCenterValue, LightRightValue;
        private boolean finished;

        public sensesThread(Scribbler _robot)
        {
            JFrame frame;
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
                data = getAll();
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

                robot.wait(0.25);
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

        public joyStickThread( Scribbler _robot )
        {
            JFrame frame;
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
            //frame.addMouseListener( new mouseEventHandler() );
        }

        public void windowClosing(WindowEvent e) 
        {
            finished = true;
        }

        public void run()
        {
            while( !finished )
                try
                {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {};
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
