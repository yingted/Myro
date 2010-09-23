using System;
using System.IO.Ports;
using System.Threading;
using IronPython.Runtime;

public static class Myro {
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
	string _port;
	SerialPort _serial;

    static int SOFT_RESET=33;
    static int GET_ALL=65 ;
    static int GET_ALL_BINARY=66  ;
    static int GET_LIGHT_LEFT=67  ;
    static int GET_LIGHT_CENTER=68  ;
    static int GET_LIGHT_RIGHT=69  ;
    static int GET_LIGHT_ALL=70  ;
    static int GET_IR_LEFT=71  ;
    static int GET_IR_RIGHT=72  ;
    static int GET_IR_ALL=73  ;
    static int GET_LINE_LEFT=74  ;
    static int GET_LINE_RIGHT=75  ;
    static int GET_LINE_ALL=76  ;
    static int GET_STATE=77  ;
    static int GET_NAME1=78;
    static int GET_NAME2=64;
    static int GET_STALL=79  ;
    static int GET_INFO=80  ;
    static int GET_DATA=81  ;

    static int GET_PASS1=50;
    static int GET_PASS2=51;

    static int GET_RLE=82 ; // a segmented and run-length encoded image
    static int GET_IMAGE=83 ; // the entire 256 x 192 image in YUYV format
    static int GET_WINDOW=84 ; // the windowed image (followed by which window)
    static int GET_DONGLE_L_IR=85 ; // number of returned pulses when left emitter is turned on
    static int GET_DONGLE_C_IR=86 ; // number of returned pulses when center emitter is turned on
    static int GET_DONGLE_R_IR=87 ; // number of returned pulses when right emitter is turned on
    static int GET_WINDOW_LIGHT=88   ; // average intensity in the user defined region
    static int GET_BATTERY=89 ; // battery voltage
    static int GET_SERIAL_MEM=90 ; // with the address returns the value in serial memory
    static int GET_SCRIB_PROGRAM=91 ; // with offset, returns the scribbler program buffer
    static int GET_CAM_PARAM=92; // with address, returns the camera parameter at that address

    static int GET_BLOB=95;

    static int SET_PASS1=55;
    static int SET_PASS2=56;
    static int SET_SINGLE_DATA=96;
    static int SET_DATA=97;
    static int SET_ECHO_MODE=98;
    static int SET_LED_LEFT_ON=99 ;
    static int SET_LED_LEFT_OFF=100;
    static int SET_LED_CENTER_ON=101;
    static int SET_LED_CENTER_OFF=102;
    static int SET_LED_RIGHT_ON=103;
    static int SET_LED_RIGHT_OFF=104;
    static int SET_LED_ALL_ON=105;
    static int SET_LED_ALL_OFF=106;
    static int SET_LED_ALL=107 ;
    static int SET_MOTORS_OFF=108;
    static int SET_MOTORS=109 ;
    static int SET_NAME1=110 ;
    static int SET_NAME2=119;           // set name2 byte
	static int SET_LOUD=111;
    static int SET_QUIET=112;
    static int SET_SPEAKER=113;
    static int SET_SPEAKER_2=114;

    static int SET_DONGLE_LED_ON=116;   // turn binary dongle led on
    static int SET_DONGLE_LED_OFF=117;  // turn binary dongle led off
    static int SET_RLE=118;             // set rle parameters 
    static int SET_DONGLE_IR=120;       // set dongle IR power
    static int SET_SERIAL_MEM=121;      // set serial memory byte
    static int SET_SCRIB_PROGRAM=122;   // set scribbler program memory byte
    static int SET_START_PROGRAM=123;   // initiate scribbler programming process
    static int SET_RESET_SCRIBBLER=124; // hard reset scribbler
    static int SET_SERIAL_ERASE=125;    // erase serial memory
    static int SET_DIMMER_LED=126;      // set dimmer led
    static int SET_WINDOW=127;          // set user defined window
    static int SET_FORWARDNESS=128;     // set direction of scribbler
    static int SET_WHITE_BALANCE=129;   // turn on white balance on camera 
    static int SET_NO_WHITE_BALANCE=130; // diable white balance on camera (default)
    static int SET_CAM_PARAM=131;       // with address and value, sets the camera parameter at that address

    static int GET_JPEG_GRAY_HEADER=135;
	static int GET_JPEG_GRAY_SCAN=136;
    static int GET_JPEG_COLOR_HEADER=137;
    static int GET_JPEG_COLOR_SCAN=138;

    static int SET_PASS_N_BYTES=139;
    static int GET_PASS_N_BYTES=140;
    static int GET_PASS_BYTES_UNTIL=141;

    static int GET_VERSION=142;

    static int GET_IR_MESSAGE = 150;
    static int SEND_IR_MESSAGE = 151;
    static int SET_IR_EMITTERS = 152;

    static int PACKET_LENGTH     =  9;
    	
	// #### Camera Addresses ####
	static int CAM_PID=0x0A;
	static int CAM_PID_DEFAULT=0x76;
	static int	CAM_VER=0x0B;
	static int CAM_VER_DEFAULT=0x48;
	static int CAM_BRT=0x06;
	static int CAM_BRT_DEFAULT=0x80;
	static int CAM_EXP=0x10;
	static int CAM_EXP_DEFAULT=0x41;
	static int CAM_COMA=0x12;
	static int CAM_COMA_DEFAULT=0x14;
	static int CAM_COMA_WHITE_BALANCE_ON= (CAM_COMA_DEFAULT |  (1 << 2));
	static int CAM_COMA_WHITE_BALANCE_OFF=(CAM_COMA_DEFAULT & ~(1 << 2));
	static int CAM_COMB=0x13;
	static int CAM_COMB_DEFAULT=0xA3;
	static int CAM_COMB_GAIN_CONTROL_ON= (CAM_COMB_DEFAULT |  (1 << 1));
	static int CAM_COMB_GAIN_CONTROL_OFF=(CAM_COMB_DEFAULT & ~(1 << 1));
	static int CAM_COMB_EXPOSURE_CONTROL_ON= (CAM_COMB_DEFAULT |  (1 << 0));
	static int CAM_COMB_EXPOSURE_CONTROL_OFF=(CAM_COMB_DEFAULT & ~(1 << 0));

	public _Scribbler(string port, int baud) {
	  _port = port;
	  _serial = new SerialPort(port, baud);
	  _serial.Open();
	  PythonDictionary info = getInfo();
	  if (info.Contains("fluke")) {
		Console.WriteLine("You are using fluke firmware {0}", info["fluke"]);
	  } else if (info.Contains("dongle")) {
		Console.WriteLine("You are using fluke firmware {0}", info["dongle"]);
	  }
	}

	public void manual_flush() {
	  int old = _serial.ReadTimeout; // milliseconds
	  //old = self.ser.timeout
	  //self.ser.setTimeout(.5)
	  _serial.ReadTimeout = 500; // milliseconds
	  string l = "a";
	  int count = 0;
	  while (l.Length != 0 & count < 50000) {
		l = read();
		count += l.Length;
	  }
	  _serial.ReadTimeout = old;
	}

	public PythonDictionary getInfo() {
	  PythonDictionary retDict = new PythonDictionary();
	  int old = _serial.ReadTimeout; // milliseconds
	  string retval;
	  // _serial.setTimeout(4)
	  _serial.ReadTimeout = 4000; // milliseconds
        
	  manual_flush();
	  // have to do this twice since sometime the first echo isn't
	  // echoed correctly (spaces) from the scribbler

	  _serial.Write(String.Format("{0}        ", (char)_Scribbler.GET_INFO));
	  retval = _serial.ReadLine();
	  //#print "Got", retval

	  Thread.Sleep(100); 
	  //time.sleep(.1)
        
	  _serial.Write(String.Format("{0}        ", (char)_Scribbler.GET_INFO));
	  retval = _serial.ReadLine();
	  //#print "Got", retval
        
	  //# remove echoes
	  if (retval.Length == 0) {
		return retDict;
	  }
        
	  if (retval[0] == 'P' | retval[0] == 'p') {
		retval = retval.Substring(1);
	  }
        
	  if (retval[0] == 'P' | retval[0] == 'p') {
		retval = retval.Substring(1);
	  }

	  _serial.ReadTimeout = old;

	  foreach (string pair in retval.Split(',')) {
		if (pair.Contains(":")) {
		  string [] split_pair = pair.Split(':');
		  string it = split_pair[0]; string value = split_pair[1];
		  retDict[it.ToLower().Trim()] = value.Trim();
		}
        //if (len(item) == 0) {
		//  return retDict;
		//} else {             
		//retval = [];
		//for it in item {
		//retval.append(retDict[it.ToLower().Trim()]);
		//}
		//if len(retval) == 1 {
		//return retval[0];
		//} else {
		//return retval;
		//}
	  }
	  return retDict;
	}
	
	public override void forward(float power, float? time) {
	  // deal with null time
	}
	
	public override void backward(float power, float? time) {
	  // deal with null time
	}
	
	public string read() {
	  //byte[] buffer = new byte[256];
	  //len = _serial.Read(buffer, 0, (int)buffer.Length);
	  //sp.BaseStream.Read(buffer, 0, (int)buffer.Length);
	  byte tmpByte;
	  string rxString = "";
	  try {
		tmpByte = (byte) _serial.ReadByte();
		while (tmpByte != 255) {
		  rxString += ((char) tmpByte);
		  tmpByte = (byte) _serial.ReadByte();			
		}
	  } catch {
	  }
	  return rxString;
	}
	
	public bool write(string data) {
	  _serial.Write(data);
	  return true;		
	}
  }
}

