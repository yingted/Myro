using System;
using System.IO.Ports;
using System.Threading;
using IronPython.Runtime;
using System.Collections.Generic; // IList

public static class Myro {
  public static Robot robot;
  public static string REVISION = "$Revision: $";
  
  public static void init(string port, int baud=38400) {
	robot = new Scribbler(port, baud);
  }
  
  public static void forward(float power=1, float? time=null) {
	robot.forward(power, time);
  }
  
  public static void backward(float power=1, float? time=null) {
	robot.backward(power, time);
  }

  public class Robot {
	public virtual void forward(float power, float? time) {
	}
	
	public virtual void backward(float power, float? time) {
	}
  }

  public static bool Contains(object item, params object[] items) {
	return ((IList<object>)items).Contains(item);
  }
  
  [Serializable()]
  public class Scribbler: Robot {
	public string port;
	public SerialPort serial;
	public string dongle;

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

	public Scribbler(string port, int baud) {
	  this.port = port;
	  serial = new SerialPort(this.port, baud);
	  serial.ReadTimeout = 100; // milliseconds
	  serial.Open();
	  PythonDictionary info = getInfo();
	  if (info.Contains("fluke")) {
		dongle = (string)info["fluke"];
		Console.WriteLine("You are using fluke firmware {0}", info["fluke"]);
	  } else if (info.Contains("dongle")) {
		dongle = (string)info["dongle"];
		Console.WriteLine("You are using fluke firmware {0}", info["dongle"]);
	  } else {
		dongle = null;
		Console.WriteLine("You are using the scribbler without the fluke");
	  }
	}

	public PythonDictionary dict(params object [] list) {
	  // make a dictionary from a list
	  PythonDictionary retval = new PythonDictionary();
	  for (int i = 0; i < list.Length; i += 2) {
		retval[list[i]] = list[i+1];
	  }
	  return retval;
	}

	public PythonDictionary list(params object [] items) {
	  // make a list from an array
	  List retval = new List();
	  for (int i = 0; i < items.Length; i++) {
		retval.append(list[i]);
	  }
	  return retval;
	}

    public byte [] GetBytes(int value, int bytes=1) {
	  byte [] retval = null;
	  try {
		//lock.acquire();
		write(value);
		read(Scribbler.PACKET_LENGTH); // read the echo
		retval = read(bytes);
	  } finally {
		//lock.release();
	  }
	  return retval;
	}

    public List GetWord(int value, int bytes=1) {
	  List retval = new List();
	  try {
		//lock.acquire();
		write(value);
		read(Scribbler.PACKET_LENGTH); // read the echo
		byte [] retvalBytes = read(bytes);
		for (int p = 0; p < retvalBytes.Length; p += 2) {
		  retval.append(retvalBytes[p] << 8 | retvalBytes[p + 1]);
		}
	  } finally {
		// lock.release();
	  }
	  return retval;
	}

    public object Get(string sensor="all", params int [] position) {
	  object retval = null;
	  sensor = sensor.ToLower();
	  if (sensor == "config") {
		if (dongle == null) {
		  return dict("ir", 2, "line", 2, "stall", 1, "light", 3);
		} else {
		  return dict("ir", 2, "line", 2, "stall", 1, "light", 3,
			  "battery", 1, "obstacle", 3, "bright", 3);
		}
	  } else if (sensor == "stall") {
		retval = GetBytes(Scribbler.GET_ALL, 11); // returned as bytes
		_lastSensors = retval; // single bit sensors
		return retval[10];
	  } else if (sensor == "forwardness") {
		if (read_mem(ser, 0, 0) != 0xDF) {
		  retval = "fluke-forward";
		} else {
		  retval = "scribbler-forward";
		}
		return retval;
	  } else if (sensor == "startsong") {
		//TODO: need to get this from flash memory
		return "tada";
	  } else if (sensor == "version") {
		//TODO: just return this version for now; get from flash
		return REVISION.Split()[1];
	  } else if (sensor == "data") {
		return getData(position);
	  } else if (sensor == "info") {
		return getInfo(position);
	  } else if (sensor == "name") {
		string c = "Scribby";
		//c = GetBytes(Scribbler.GET_NAME1, 8);
		//c += GetBytes(Scribbler.GET_NAME2, 8);
		//c = string.join([chr(x) for x in c if "0" <= chr(x) <= "z"], '').strip();
		return c;
	  } else if (sensor == "password") {
		string c = "Scribby";
		//c = GetBytes(Scribbler.GET_PASS1, 8);
		//c += GetBytes(Scribbler.GET_PASS2, 8);
		//c = string.join([chr(x) for x in c if "0" <= chr(x) <= "z"], '').strip();
		return c;
	  } else if (sensor == "volume") {
		return _volume;
	  } else if (sensor == "battery") {
		return getBattery();
	  } else if (sensor == "blob") {
		return getBlob();
	  } else {
		if (len(position) == 0) {
		  if (sensor == "light") {
			return GetWord(Scribbler.GET_LIGHT_ALL, 6);
		  } else if (sensor == "line") {
			return GetBytes(Scribbler.GET_LINE_ALL, 2);
		  } else if (sensor == "ir") {
			return GetBytes(Scribbler.GET_IR_ALL, 2);
		  } else if (sensor == "obstacle") {
			return new List(getObstacle("left"), getObstacle("center"), getObstacle("right"));
		  } else if (sensor == "bright") {
			return new List(getBright("left"), getBright("middle"), getBright("right"));
		  } else if (sensor == "all") {
			retval = GetBytes(Scribbler.GET_ALL, 11); // returned as bytes
			_lastSensors = retval; // single bit sensors
			if (dongle == null) {
			  return dict(
				  "light", new List(retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], 
					  retval[6] << 8 | retval[7]),
				  "ir", new List(retval[0], retval[1]), 
				  "line", new List(retval[8], retval[9]), 
				  "stall", retval[10]);
			} else {
			  return dict(
				  "light", new List(retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], 
					  retval[6] << 8 | retval[7]),
				  "ir", new List(retval[0], retval[1]), 
				  "line", new List(retval[8], retval[9]), 
				  "stall", retval[10],
				  "obstacle", new List(getObstacle("left"), getObstacle("center"), getObstacle("right")),
				  "bright", new List(getBright("left"), getBright("middle"), getBright("right")),
				  "blob", getBlob(),
				  "battery", getBattery()
						  );
			}
		  } else {
			throw new Exception("invalid sensor name: '%s'" % sensor);
		  }
		}
		List retvals = new List();
		foreach (int pos in position) {
		  if (sensor == "light") {
			values = GetWord(Scribbler.GET_LIGHT_ALL, 6);
			if (Contains(pos, 0, "left")) {
			  retvals.Append(values[0]);
			} else if (Contains(pos, 1, "middle", "center")) {
			  retvals.Append(values[1]);
			} else if (Contains(pos, 2, "right")) {
			  retvals.Append(values[2]);
			} else if (pos == None | pos == "all") {
			  retvals.Append(values);
			}
		  } else if (sensor == "ir") {
			values = GetBytes(Scribbler.GET_IR_ALL, 2);
			if (Contains(pos, 0, "left")) {
			  retvals.Append(values[0]);
			} else if (Contains(pos, 1, "right")) {
			  retvals.Append(values[1]);
			} else if (pos == None | pos == "all") {
			  retvals.Append(values);
			}
		  } else if (sensor == "line") {
			values = GetBytes(Scribbler.GET_LINE_ALL, 2);
			if (Contains(pos, 0, "left")) {
			  retvals.Append(values[0]);
			} else if (Contains(pos, 1, "right")) {
			  retvals.Append(values[1]);
			}
		  } else if (sensor == "obstacle") {
			return getObstacle(pos);
		  } else if (sensor == "bright") {
			return getBright(pos);
		  } else if (sensor == "picture") {
			return takePicture(pos);
		  } else {
			throw new Exception("invalid sensor name: '%s'" % sensor);
		  }
		}
		if (len(retvals) == 0) {
		  return None;
		} else if (len(retvals) == 1) {
		  return retvals[0];
		} else {
		  return retvals;
		}
	  }
	}
	/*
        sensor = sensor.lower()
        if sensor == "config":
            if dongle == None:
                return {"ir": 2, "line": 2, "stall": 1, "light": 3}
            else:
                return {"ir": 2, "line": 2, "stall": 1, "light": 3,
                        "battery": 1, "obstacle": 3, "bright": 3}
        elif sensor == "stall":
            retval = GetBytes(Scribbler.GET_ALL, 11) // returned as bytes
            _lastSensors = retval // single bit sensors
            return retval[10]
        elif sensor == "forwardness":
            if read_mem(ser, 0, 0) != 0xDF:
                retval = "fluke-forward"
            else:
                retval = "scribbler-forward"
            return retval
        elif sensor == "startsong":
            //TODO: need to get this from flash memory
            return "tada"
        elif sensor == "version":
            //TODO: just return this version for now; get from flash
            return __REVISION__.split()[1]
        elif sensor == "data":
            return getData(*position)
        elif sensor == "info":
            return getInfo(*position)
        elif sensor == "name":
            c = GetBytes(Scribbler.GET_NAME1, 8)
            c += GetBytes(Scribbler.GET_NAME2, 8)
            c = string.join([chr(x) for x in c if "0" <= chr(x) <= "z"], '').strip()
            return c
        elif sensor == "password":
            c = GetBytes(Scribbler.GET_PASS1, 8)
            c += GetBytes(Scribbler.GET_PASS2, 8)
            c = string.join([chr(x) for x in c if "0" <= chr(x) <= "z"], '').strip()
            return c
        elif sensor == "volume":
            return _volume
        elif sensor == "battery":
            return getBattery()
        elif sensor == "blob":
            return getBlob()
        else:
            if len(position) == 0:
                if sensor == "light":
                    return GetWord(Scribbler.GET_LIGHT_ALL, 6)
                elif sensor == "line":
                    return GetBytes(Scribbler.GET_LINE_ALL, 2)
                elif sensor == "ir":
                    return GetBytes(Scribbler.GET_IR_ALL, 2)
                elif sensor == "obstacle":
                    return [getObstacle("left"), getObstacle("center"), getObstacle("right")]
                elif sensor == "bright":
                    return [getBright("left"), getBright("middle"), getBright("right") ]
                elif sensor == "all":
                    retval = GetBytes(Scribbler.GET_ALL, 11) // returned as bytes
                    _lastSensors = retval // single bit sensors
                    if dongle == None:
                        return {"light": [retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], retval[6] << 8 | retval[7]],
                                "ir": [retval[0], retval[1]], "line": [retval[8], retval[9]], "stall": retval[10]}
                    else:
                        return {"light": [retval[2] << 8 | retval[3], retval[4] << 8 | retval[5], retval[6] << 8 | retval[7]],
                                "ir": [retval[0], retval[1]], "line": [retval[8], retval[9]], "stall": retval[10],
                                "obstacle": [getObstacle("left"), getObstacle("center"), getObstacle("right")],
                                "bright": [getBright("left"), getBright("middle"), getBright("right")],
                                "blob": getBlob(),
                                "battery": getBattery(),
                                }
                else:                
                    raise ("invalid sensor name: '%s'" % sensor)
            retvals = []
            for pos in position:
                if sensor == "light":
                    values = GetWord(Scribbler.GET_LIGHT_ALL, 6)
                    if pos in [0, "left"]:
                        retvals.append(values[0])
                    elif pos in [1, "middle", "center"]:
                        retvals.append(values[1])
                    elif pos in [2, "right"]:
                        retvals.append(values[2])
                    elif pos == None | pos == "all":
                        retvals.append(values)
                elif sensor == "ir":
                    values = GetBytes(Scribbler.GET_IR_ALL, 2)                    
                    if pos in [0, "left"]:
                        retvals.append(values[0])
                    elif pos in [1, "right"]:
                        retvals.append(values[1])
                    elif pos == None | pos == "all":
                        retvals.append(values)
                elif sensor == "line":
                    values = GetBytes(Scribbler.GET_LINE_ALL, 2)
                    if pos in [0, "left"]:
                        retvals.append(values[0])
                    elif pos in [1, "right"]:
                        retvals.append(values[1])
                elif sensor == "obstacle":
                    return getObstacle(pos)
                elif sensor == "bright":
                    return getBright(pos)
                elif sensor == "picture":
                    return takePicture(pos)
                else:
                    raise ("invalid sensor name: '%s'" % sensor)
            if len(retvals) == 0:
                return None
            elif len(retvals) == 1:
                return retvals[0]
            else:
                return retvals

    def getData(self, *position):
        if len(position) == 0: 
            return GetBytes(Scribbler.GET_DATA, 8)
        else:   
            retval = []               
            for p in position:
                retval.append(GetBytes(Scribbler.GET_DATA, 8)[p])
            if len(retval) == 1:
                return retval[0]
            else:
                return retval
	*/

	public PythonDictionary getInfo() {
	  PythonDictionary retDict = new PythonDictionary();
	  int old = serial.ReadTimeout; // milliseconds
	  string retval;
	  // serial.setTimeout(4)
	  serial.ReadTimeout = 4000; // milliseconds
        
	  manual_flush();
	  // have to do this twice since sometime the first echo isn't
	  // echoed correctly (spaces) from the scribbler

	  serial.Write(String.Format("{0}        ", (char)Scribbler.GET_INFO));
	  retval = serial.ReadLine();
	  //#print "Got", retval

	  Thread.Sleep(100); 
	  //time.sleep(.1)
        
	  serial.Write(String.Format("{0}        ", (char)Scribbler.GET_INFO));
	  retval = serial.ReadLine();
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

	  serial.ReadTimeout = old;

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
	
	public byte [] read(int bytes) {
	  byte[] buffer = new byte[bytes];
	  len = serial.Read(buffer, 0, (int)buffer.Length);
	  //sp.BaseStream.Read(buffer, 0, (int)buffer.Length);
	  return buffer;
	}

	public string read() {
	  //byte[] buffer = new byte[256];
	  //len = _serial.Read(buffer, 0, (int)buffer.Length);
	  //sp.BaseStream.Read(buffer, 0, (int)buffer.Length);
	  byte tmpByte;
	  string rxString = "";
	  try {
		tmpByte = (byte) serial.ReadByte();
		while (tmpByte != 255) {
		  rxString += ((char) tmpByte);
		  tmpByte = (byte) serial.ReadByte();			
		}
	  } catch {
	  }
	  return rxString;
	}
	
	public bool write(string data) {
	  serial.Write(data);
	  return true;		
	}

	public bool write(params int [] data) {
	  foreach (int datum in data) {
		serial.Write((char)datum);
	  }
	  return true;		
	}

	public void manual_flush() {
	  int old = serial.ReadTimeout; // milliseconds
	  //old = ser.timeout
	  //ser.setTimeout(.5)
	  serial.ReadTimeout = 500; // milliseconds
	  string l = "a";
	  int count = 0;
	  while (l.Length != 0 & count < 50000) {
		l = read();
		count += l.Length;
	  }
	  serial.ReadTimeout = old;
	}

  }
}

