using System;
using System.IO.Ports;

public static class myro {
  public static _Robot robot;
  
  public static void init(string port, int baud=38400) {
	robot = new _Scribbler(port, baud);
  }
  
  public static void forward(float power=1, float? time=null) {
	robot.forward(power, time);
  }
  
  public static void backward(float power=1, float? time=null) {
	robot.backward(power, time);
  }

  public class _Robot {
	public virtual void forward(float power, float? time) {
	}
	
	public virtual void backward(float power, float? time) {
	}
  }
  
  [Serializable()]
  public class _Scribbler: _Robot {
	
	SerialPort _port;
	
	public _Scribbler(string port, int baud) {
	  _port = new SerialPort(port, baud);
	  _port.Open();
	}
	
	public override void forward(float power, float? time) {
	  // deal with null time
	}
	
	public override void backward(float power, float? time) {
	  // deal with null time
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
}

