using System.IO.Ports;
using System;

public class myro {

    public class Scribbler {
	
	SerialPort _port;
	
	public Scribbler(string port, int baud) {
	    _port = new SerialPort(port, baud);
	    _port.Open();
	    robot = this;
	}

	public void forward(float power, float time) {
	}

	public string read() {
	    byte tmpByte;
	    string rxString = "";
	    tmpByte = (byte) _port.ReadByte();
	    while (tmpByte != 255) {
		rxString += ((char) tmpByte);
		tmpByte = (byte) _port.ReadByte();			
	    }
	    return rxString;
	}
 
	public bool write(string data) {
	    _port.Write(data);
	    return true;		
	}
    }
    
    public static Scribbler robot;
    
    public static void forward(float power, float time) {
	_robot.forward(power, time);
    }
    
    public static void backward(float power, float time) {
	_robot.backward(power, time);
    }
    
    
}

