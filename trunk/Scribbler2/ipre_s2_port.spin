{
***************************************
*  IPRE Scribbler2 Firmware v1.1.2    *
*  Date:   12-6-2010                  *
*  Author: Daniel Harris              *
***************************************

This program runs commands sent by Georgia Tech's Fluke module on Parallax's new Scribbler2
robot.  The port uses the Parallax Propeller in the Scribbler 2 to simulate the functionality
of the BS2 powered Scribbler (1) robot.  The program relies heavily on the use of Bueno Systems'
S2 object which acts as an interface and controller to the Scribbler2's low level hardware.  The
port implements each function defined in the original "scribbler_server.bs2" file so that the
Propeller generates identical output.  Original BS2 firmware can be found at:

http://svn.cs.brynmawr.edu/viewvc/Myro/trunk/Scribbler/scribbler_server.bs2?view=markup


}
con


  _clkmode = xtal1 + pll16x
  _xinfreq = 5_000_000


  _SOFT_RESET         = 33  ' Format: 33 0 0 0 0 0 0 0 0, Notes: Performs a software reset of robot.  (turns name broadcast back on)

  _GET_PASS1          = 50  ' Respone: 16 bytes from flash memory
  _GET_PASS2          = 51  ' Respone: 16 bytes from flash memory
   
  _SET_PASS1          = 55  ' Format: 55 PASS1 PASS2 ... PASS8
  _SET_PASS2          = 56  ' Format: 56 PASS9 PASS2 ... PASS16
   
  _GET_NAME2          = 64  ' Response: char9 char10 char11 char12 char13 char14 char15 char16 87
  _GET_ALL            = 65  ' Response: leftIR rightIR LeftLightHighyte LeftLightLowByte CenterLightHighByte CenterLightLowByte RightLightHighByte RightLightLowByte LineLeft LineRight Stall 65
  _GET_ALL_BINARY     = 66  ' Response: BinaryData 66, Notes: where the individual bits of BinaryData are: 0x000 IRLeft IRRight Stall LineLeft LineRight
  _GET_LIGHT_LEFT     = 67  ' Response: HighByte LowByte 67
  _GET_LIGHT_CENTER   = 68  ' Response: HighByte LowByte 68
  _GET_LIGHT_RIGHT    = 69  ' Response: HighByte LowByte 69
  _GET_LIGHT_ALL      = 70  ' Response: LeftHighyte LeftLowByte CenterHighByte CenterLowByte RightHighByte RightLowByte 70
  _GET_IR_LEFT        = 71  ' Response: leftIR 71, Notes: IR is 1 when there is no obstacle to the left of robot
  _GET_IR_RIGHT       = 72  ' Response: rightIR 72, Notes: IR is 1 when there is no obstacle to the left of robot
  _GET_IR_ALL         = 73  ' Response: LeftIR RightIR 73, Notes: IR is 1 when there is no obstacle to the left of robot
  _GET_LINE_LEFT      = 74  ' Response: lineLeft 74
  _GET_LINE_RIGHT     = 75  ' Response: lineRight 75
  _GET_LINE_ALL       = 76  ' Response: LineLeft LineRight 76
  _GET_STATE          = 77  ' Response: inPins outPins 77, Notes: inPins is the state of all the input pins (0-7), and outPins is the state of all the output pins (8-15) as defined in the I/O Pin Declarations.
  _GET_NAME1          = 78  ' Response: char1 char2 char3 char4 char5 char6 char7 char8 78
  _GET_STALL          = 79  ' Response: stall 79
  _GET_INFO           = 80  ' Response: "information on version robot, etc" 10 80
  _GET_DATA           = 81  ' Response: data from flash memory 81
   
   
  _SET_SINGLE_DATA    = 96  ' Sets a single byte of data in flash memory'
  _SET_DATA           = 97  ' Sets 8 bytes of data in flash memory
  _SET_ECHO_MODE      = 98  ' Set the echo mode on or off; if off, no echo is made after command
  _SET_LED_LEFT_ON    = 99
  _SET_LED_LEFT_OFF   = 100
  _SET_LED_CENTER_ON  = 101
  _SET_LED_CENTER_OFF = 102
  _SET_LED_RIGHT_ON   = 103
  _SET_LED_RIGHT_OFF  = 104
  _SET_LED_ALL_ON     = 105
  _SET_LED_ALL_OFF    = 106
  _SET_LED_ALL        = 107 ' Format: 107 LeftLEDstate CenterLEDstate RightLEDstate 0 0 0 0 0
  _SET_MOTORS_OFF     = 108 ' Format: 108 0 0 0 0 0 0 0 0
  _SET_MOTORS         = 109 ' Format: 109 rightmotor leftmotor 0 0 0 0 0 0, Notes: 0 = full speed backwards, 100 = stop, 200 = full speed forward
  _SET_NAME1          = 110 ' Format: 110 char1 char2 char3 char4 char5 char6 char7 char8
  _SET_NAME2          = 119 ' Format: 119 char9 char10 char11 char12 char13 char14 char15 char16
  _SET_LOUD           = 111
  _SET_QUIET          = 112
  _SET_SPEAKER        = 113 ' Format: 113 DurationHighByte DurationLowByte FreqHighByte FreqLowByte
  _SET_SPEAKER_2      = 114 ' Format: 114 DurationHighByte DurationLowByte Freq1HighByte Freq1LowByte Freq2HighByte Freq2LowByte


  TIMEOUT       = 1000      ' 3 of milliseconds for timeout on waiting for data from the S2's serial port
  S2BAUD        = 38_400

  DATA_LENGTH   = 8
  PACKET_LENGTH = 9

  MAX_OUTBUFF   = 100

'2.0 Extensions for S2 Begin
'2.0 New messages

  _SET_VOLUME         = 160 'Format 160 volume
  _PATH               = 161 'Format 161 begin_or_end speed
  _MOVE               = 162 'Format 162 type hXByte lXByte hYByte lYByte
  _ARC                = 163 'Format 163 type hXByte lXByte hYByte lYByte hRadByte lRadByte
  _TURN               = 164 'Format 164 type hAngleByte lAngleByte
  _GET_POSN           = 165 'Format 165
  _SET_POSN           = 166 'Format 166 x0Byte x1Byte x2Byte x3Byte y0Byte y1Byte y2Byte y3Byte
  _GET_ANGLE          = 167 'Format 167
  _SET_ANGLE          = 168 'Format 168 angle0Byte angle1Byte angle2Byte angle3Byte
  _GET_MIC_ENV        = 169 'Format 169
  _GET_MOTOR_STATS    = 170 'Format 170
  _GET_ENCODERS       = 171 'Format 171 type

  _BY                 = 4
  _TO                 = 2
  _DEG                = 1
  _MM                 = 1

  _BEGIN              = 1
  _END                = 0

'2.0 Extensions for S2 End


'2.1 Extensions for S2 Begin
'2.1 New messages
  _GET_IR_EX          = 172 'Format 172 side type thres
  _GET_LINE_EX        = 173 'Format 173 side type thres
  _DISTANCE_EX        = 175 'Format 175 side

  _NOW                = 8
  _REL                = 8


'2.1 Extensions for S2 End
  
  
var

  long reset_stack[20]
  long FMStack[50]
  long my_stats[6]

  byte indata[PACKET_LENGTH]
  byte outdata[MAX_OUTBUFF]
  byte outdata_idx

  byte  quiet, send_name, echo_mode, uninit, led_state
  
obj

  s2    :       "s2"
  ser   :       "FullDuplexSerial"
  i2c   :       "Basic_I2C_Driver"
  'd    :       "Parallax Serial Terminal"              'uncomment to enable an object used for debugging communication

dat

roboData      byte      "Robot-Version:1.1.5,Robot:Scribbler2,Mode:Serial", 10, 0

nameData      byte      "Scribby         ", 0           'null terminate string
ipreData      byte      127, 127, 127, 127, 0, 0, 0, 0
passData      byte      "YBBIRCs         ", 0           'null terminate string

echoData      byte      0
sendData      byte      0
quietData     byte      0

pub go | i

  s2.start
  ser.start(s2#RX, s2#TX, %0000, S2BAUD)  'Start the main serial port

  s2.start_tones                          'Start the tone sequencer/generator.

  s2.start_motors                         'Start the motor cog up
  s2.get_wheel_calibration                'Read calibration data and inform the motor process    

  s2.start_mic_env                        '2.0 start the Mic cog          

  s2.heading_is(0)
  s2.here_is(0, 0)
  cognew(reset_button, @reset_stack)      'Start the reset button monitor cog
  cognew(FaultMonitor, @FMStack)          'Start the fault monitoring cog
  
  'd.StartRxTx(0, 1, 0, 9600)             'TEK, start debug serial port.  Uses pins on the hacker port inside of the S2
  'd.str(STRING(13,"Scribbler2 Started",13)) 'Print a "started" message to the debug serial port
  
  led_state~
  outdata_idx~

  'read these values from the Propeller's RAM (which was copied from the EEPROM on startup)
  quiet := quietData
  send_name := sendData 
  echo_mode := echoData

  Play_Intro                    'Play the turn on chirp


  '======================== Main Loop ===========================
  repeat
    toggle_center_led
    update_leds                 'write any changes to led_state to the S2's LEDs

    if send_name                'for name broadcasting
      enqueuestr(STRING("IPRE"))
      repeat i from 0 to PACKET_LENGTH - 1
        enqueue(i2c.ReadByte(i2c#BootPin, i2c#EEPROM, @nameData + i))
      enqueue(0)
      send_data                 'dont wait for a valid packet to be received before sending, send now!

    if get_packet == -1         'get the packet         
      next                      'packet didnt come in properly, so start loop over (effectively discarding bad packed)

      
    'We can only get here in the main loop if a packet was properly received.
    
    if outdata_idx > 0          '2.0 don't send it if we don't have it 
      send_data                 '2.0 echo back packet immediately to be consistent with original Scribbler

    process_packet              'now process the packet

    if echo_mode                'if echo is on, then insert the command in the transmit buffer
      enqueue(indata[0])

    'if we have data to send, then send it!
    if outdata_idx > 0          'by software design, we shouldnt have to do this check, but its a good catch-all
      send_data

    send_name := false          'turn off name broadcast

  '======================= End of Main Loop ======================
  


pub process_packet

  case(indata[0])               'the command is the first byte in the received packet             
                                
    _GET_STATE:
      Get_State
     
    _GET_LINE_RIGHT:
      Get_Line_Right
     
    _GET_LINE_LEFT:
      Get_Line_Left
     
    _GET_LIGHT_LEFT:
      Get_Light_Left
     
    _GET_LIGHT_CENTER:
      Get_Light_Center
     
    _GET_LIGHT_RIGHT:
      Get_Light_Right
     
    _GET_IR_LEFT:
      Get_IR_Left
     
    _GET_IR_RIGHT:
      Get_IR_Right
     
    _GET_STALL:
      Get_Stall
     
    _GET_NAME1:
      Get_Name1
     
    _GET_NAME2:
      Get_Name2

    _GET_PASS1:
      Get_Pass1
    
    _GET_PASS2:
      Get_Pass2
           
    _SET_MOTORS_OFF:
      Set_Motors_Off
     
    _SET_MOTORS:
      Set_Motors

'2.0 Extensions for S2 Begin

    _SET_VOLUME:
      Set_Volume 

    _PATH:
      Path

    _MOVE:    
      Move

    _ARC:
      Arc

    _TURN:
      Turn

    _GET_POSN:
      Get_Posn

    _SET_POSN:
      Set_Posn 

    _GET_ANGLE:
      Get_Angle

    _SET_ANGLE:
      Set_Angle

    _GET_MIC_ENV:
      Get_Mic_Env

    _GET_MOTOR_STATS:
      Get_Motor_Stats

    _GET_ENCODERS:
      Get_Encoders

'2.0 Extensions for S2 End

'2.1 Extensions for S2 Begin
'2.1 New messages

    _GET_IR_EX:
      Get_IR_Ex 

    _GET_LINE_EX:
      Get_Line_Ex

    _DISTANCE_EX:
      Distance_Ex

'2.0 Extensions for S2 End



     
    _SET_SPEAKER:
      Set_Speaker
     
    _SET_SPEAKER_2:
      Set_Speaker2
     
    _SET_LED_LEFT_ON:
      Set_LED_Left_On
     
    _SET_LED_LEFT_OFF:
      Set_LED_Left_Off
     
    _SET_LED_CENTER_ON:
      Set_LED_Center_On
     
    _SET_LED_CENTER_OFF:
      Set_LED_Center_Off
     
    _SET_LED_RIGHT_ON:
      Set_LED_Right_On
     
    _SET_LED_RIGHT_OFF:
      Set_LED_Right_Off
     
    _SET_NAME1:
      Set_Name1
     
    _SET_NAME2:
      Set_Name2
     
    _SET_PASS1:
      Set_Pass1
     
    _SET_PASS2:
      Set_Pass2
     
    _SET_LED_ALL_ON:
      Set_LED_All_On
     
    _SET_LED_ALL_OFF:
      Set_LED_All_Off
     
    _GET_LIGHT_ALL:
      Get_Light_All
     
    _GET_IR_ALL:
      Get_IR_All
     
    _GET_LINE_ALL:
      Get_Line_All
     
    _GET_ALL:
      Get_All
     
    _SET_LOUD:
      Set_Loud
     
    _SET_QUIET:
      Set_Quiet
     
    _GET_ALL_BINARY:
      Get_All_Binary
     
    _SOFT_RESET:
      Reboot_S2
     
    _SET_LED_ALL:
      Set_LED_All
     
    _GET_INFO:
      Get_Info
     
    _GET_DATA:
      Get_Data
     
    _SET_DATA:
      Set_Data
     
    _SET_SINGLE_DATA:
      Set_Single_Data
     
    _SET_ECHO_MODE:
      Set_Echo_Mode

pub Set_Data | i

  repeat i from 0 to DATA_LENGTH - 1
    i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @ipreData + i, indata[1 + i])
    pause(5_000)
  Get_All

pub Set_Single_Data

  i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @ipreData + indata[1], indata[2])
  pause(5_000)
  Get_All

pub Set_Pass1 | i
  
  repeat i from 0 to DATA_LENGTH - 1
    i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @passData + i, indata[1 + i])
    pause(5_000)
  Get_All

pub Set_Pass2 | i
  
  repeat i from 0 to DATA_LENGTH - 1
    i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @passData + 8 + i, indata[1 + i])
    pause(5_000)
  Get_All

pub Get_IR_Left
  enqueue(!s2.obstacle(s2#LEFT, 0) & 1)

pub Get_IR_Right
  enqueue(!s2.obstacle(s2#RIGHT, 0) & 1)
      
pub Get_Light_Right | temp
  temp := s2.light_sensor_raw_inv(s2#RIGHT) 
  enqueue((temp>>8) & $FF)     'enqueue high byte
  enqueue((temp) & $FF)        'enqueue low byte
      
pub Get_Light_Center | temp
  temp := s2.light_sensor_raw_inv(s2#CENTER)
  enqueue((temp>>8) & $FF)    'enqueue high byte
  enqueue((temp) & $FF)       'enqueue low byte

pub Get_Light_Left | temp
  temp := s2.light_sensor_raw_inv(s2#LEFT)
  enqueue((temp>>8) & $FF)      'enqueue high byte
  enqueue((temp) & $FF)         'enqueue low byte

pub Set_LED_All

  led_state := ((indata[1]&1)<<2) | ((indata[2]&1)<<1) | (indata[3]&1)
  Get_All

pub toggle_center_led
  led_state ^= %010
  
pub Set_LED_Left_On
  led_state |= %100
  Get_All
  
pub Set_LED_Left_Off
  led_state &= %011
  Get_All
  
pub Set_LED_Center_On
  led_state |= %010
  Get_All
  
pub Set_LED_Center_Off
  led_state &= %101
  Get_All
  
pub Set_LED_Right_On
  led_state |= %001
  Get_All

pub Set_LED_Right_Off
  led_state &= %110
  Get_All

pub Set_Motors | left_velocity, right_velocity

  left_velocity := (indata[2]-100) << 1                 'unpack motor data
  right_velocity := (indata[1]-100) << 1

  s2.wheels_now(left_velocity, right_velocity, 0)       'stick motor data to the wheels (make the robot GO!)

  Get_All

'2.0 Extensions for S2 Begin

pub Set_Volume | vol
  vol := indata[1]
  s2.set_volume(vol)

  Get_All

pub  Path | speed
'        0        1           2           3
'Formal 161 begin_or_end hSpeedByte lSpeedByte
  speed := (indata[2] << 8) + (indata[3])
  ~~speed

  case(indata[1])
    _BEGIN:
      s2.begin_path
      s2.set_speed(speed)

    _END:
      s2.end_path

  Get_All
    

pub Move | x_coord, y_coord
'        0    1     2      3      4      5 
'Format 162 type hXByte lXByte hYByte lYByte
'  type    := indata[1]
  x_coord := (indata[2]<<8 | indata[3])
  y_coord := (indata[4]<<8 | indata[5])
  ~~x_coord
  ~~y_coord

  case(indata[1])
    _BY:
      s2.move_by(x_coord, y_coord)

    _BY + _MM:
      s2.move_by(x_coord*2, y_coord*2)

    _BY + _REL:
      s2.move(x_coord, y_coord, 0, 15, 1)

    _BY + _REL + _MM:
      s2.move(x_coord*2, y_coord*2, 0, 15, 1)

    _TO:
      s2.move_to(x_coord, y_coord)

    _TO + _MM:
      s2.move_to(x_coord*2, y_coord*2)

  Get_All

pub Arc  | x_coord, y_coord, radius
'        0    1    2      3      4      5        6        7 
'Format 163 type hXByt e lXByte hYByte lYByte hRadByte lRadByte
'  type    := indata[1]
  x_coord := (indata[2] << 8) + (indata[3])
  y_coord := (indata[4] << 8) + (indata[5])
  radius  := (indata[6] << 8) + (indata[7])
  ~~x_coord
  ~~y_coord
  ~~radius

  case(indata[1])

    _TO:
      s2.arc_to(x_coord, y_coord, radius)

    _TO + _MM:
      s2.arc_to(x_coord*2, y_coord*2, radius*2)

    _BY:
      s2.arc_by(x_coord, y_coord, radius)

    _BY + _REL:
     ' interpreting x_coord as degrees here
      s2.arc_deg(x_coord, radius)

    _BY + _MM:
      s2.arc_by(x_coord*2, y_coord*2, radius*2)

    _BY + _REL + _MM:
      ' interpreting x_coord as degrees here
      s2.arc_deg(x_coord, radius*2)

  Get_All



pub Turn | angle
'Format 164 type hAngleByte lAngleByte
'  type  := indata[1]
  angle := (indata[2] << 8) + (indata[3])
  ~~angle

  case(indata[1])
    _BY:
      s2.turn_by(angle)

    _TO:
      s2.turn_to(angle)

    _BY + _DEG:
      s2.turn_by_deg(angle)

    _TO + _DEG:
      s2.turn_to_deg(angle)

    _BY + _NOW:
      s2.turn_now(angle)

    _BY + _DEG + _NOW:
      s2.turn_deg_now(angle)

  Get_All


pub Get_Posn | coord
  coord := s2.get_current_x / 2 'multiply by .5

  enqueue((coord >> 24) & $FF)
  enqueue((coord >> 16) & $FF)
  enqueue((coord >>  8) & $FF)
  enqueue((coord      ) & $FF)

  coord := s2.get_current_y / 2 'mulitply by .5
  enqueue((coord >> 24) & $FF)
  enqueue((coord >> 16) & $FF)
  enqueue((coord >>  8) & $FF)
  enqueue((coord      ) & $FF)

pub Set_Posn | x, y
  x := (((((indata[1] << 8) | indata[2]) << 8) | indata[3]) << 8) | indata[4]
  y := (((((indata[5] << 8) | indata[6]) << 8) | indata[7]) << 8) | indata[8]
  x := x * 2   'divide by .5
  y := y * 2   'divide by .5
  s2.here_is(x, y)

  Get_All

pub Get_Angle | coord
  coord := s2.get_current_w_in_deg
  enqueue((coord >> 24) & $FF)
  enqueue((coord >> 16) & $FF)
  enqueue((coord >>  8) & $FF)
  enqueue((coord      ) & $FF)

pub Set_Angle | angle
  angle := (((((indata[1] << 8) | indata[2]) << 8) | indata[3]) << 8) | indata[4]
  s2.heading_is_deg(angle)

  Get_All

pub Get_Mic_Env | env
  env := s2.get_mic_env
  enqueue((env >> 24) & $FF)
  enqueue((env >> 16) & $FF)
  enqueue((env >>  8) & $FF)
  enqueue((env      ) & $FF)

pub Get_Motor_Stats | temp
  longmove(@my_stats, s2.motion_addr, 6)
  enqueue((my_stats[0] >> 24) & $FF)
  enqueue((my_stats[0] >> 16) & $FF)
  enqueue((my_stats[0] >>  8) & $FF)
  enqueue((my_stats[0]      ) & $FF)

  temp := s2.move_ready
  if (temp)
    enqueue(1)
  else
    enqueue(0)

pub Get_Encoders | left, right, clear
  clear := indata[1]
  left  := s2.get_left_enc(clear)
  right := s2.get_right_enc(clear)

  enqueue((left >> 24) & $FF)
  enqueue((left >> 16) & $FF)
  enqueue((left >>  8) & $FF)
  enqueue((left      ) & $FF)

  enqueue((right >> 24) & $FF)
  enqueue((right >> 16) & $FF)
  enqueue((right >>  8) & $FF)
  enqueue((right      ) & $FF)


'2.0 Extensions for S2 End

'2.1 Extensions for S2 Begin
'2.1 New messages

pub Get_IR_Ex | side, type, thres, temp
  side  := indata[1]
  type  := indata[2]
  thres := indata[3]

  case (side)
    0:
      temp := !s2.obstacle(s2#LEFT, thres)

    1:
      temp := !s2.obstacle(s2#RIGHT, thres)
 
  enqueue(temp & $01)

pub Get_Line_Ex | side, type, thres, temp
  side  := indata[1]
  type  := indata[2]
  thres := indata[3]

  case (side)
    0:
      temp := !s2.line_sensor(s2#LEFT, thres)

    1:
      temp := !s2.line_sensor(s2#RIGHT, thres)

  enqueue(temp & 1)


pub Distance_Ex | side, temp
  side  := indata[1]

  case(side)
    0:
      temp := s2.distance(s2#LEFT)

    1:
      temp := s2.distance(s2#RIGHT)

  enqueue(temp & $FF)

'2.1 Extensions for S2 End


pub Get_Data | i

  repeat i from 0 to DATA_LENGTH - 1
    enqueue(i2c.ReadByte(i2c#BootPin, i2c#EEPROM, @ipreData + i))                  'make a call to get ipreData from the EEPROM and queue up the read byte


pub Get_State | outByte

  outByte := 0

  'most sensors on the S2 dont feed directly into the input pins, so build the state one bit at a time.
  
  outByte |= s2.light_sensor(s2#RIGHT) << 0            'LightRightPin
  outByte |= s2.light_sensor(s2#CENTER) << 1           'LightCenterPin
  outByte |= s2.light_sensor(s2#LEFT) << 2             'LightLeftPin
  outByte |= 1 << 3             'LineEnablePin, always running on S2, so static true here
  outByte |= (!s2.line_sensor(s2#RIGHT, 0) & 1) << 4 'LineRightPin
  outByte |= (!s2.line_sensor(s2#LEFT, 0) & 1) << 5  'LineLeftPin
  outByte |= (ina[s2#OBS_RX] & 1) << 6              'ObsRxPin
  outByte |= (s2.stalled & 1) << 7                  'StallPin

  outByte |= ((led_state >> 0) & 1) << 8           'LedRightPin
  outByte |= ((led_state >> 1) & 1) << 9           'LedCenterPin
  outByte |= ((led_state >> 2) & 1) << 10          'LedLeftPin
  outByte |= (ina[s2#SPEAKER] & 1) << 11
  outByte |= 0 << 12
  outByte |= 0 << 13
  outByte |= (!ina[s2#OBS_TX_RIGHT] & 1) << 14
  outByte |= (!ina[s2#OBS_TX_LEFT] & 1) << 15

  enqueue(outByte & $FF)                                'enqueue low byte
  enqueue((outByte >> 8) & $FF)                         'enqueue high byte

pub Get_All_Binary | temp

'%0 0 0 IRLeft IRRight Stall LineLeft LineRight

  temp~

  temp := (!s2.obstacle(s2#LEFT, 0) & 1) << 4
  temp |= (!s2.obstacle(s2#RIGHT, 0) & 1) << 3
  temp |= (s2.stalled & 1) << 2
  temp |= (!s2.line_sensor(s2#LEFT, 0) & 1) << 1
  temp |= (!s2.line_sensor(s2#RIGHT, 0) & 1)

  enqueue(temp)    

pub Get_Line_Right
  enqueue(!s2.line_sensor(s2#RIGHT, 0) & 1)

pub Get_Line_Left
  enqueue(!s2.line_sensor(s2#LEFT, 0) & 1)

pub Set_Quiet

  quiet := true                                         'write value to global variable
  i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @quietData, true)  'write value to persistent ROM
  pause(5_000)
  Get_All

pub Set_Loud

  quiet := false                                        'write value to global variable
  i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @quietData, false) 'write value to persistent ROM
  pause(5_000)
  Get_All

pub Set_Echo_Mode

  echo_mode := indata[1]
  i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @echoData, indata[1]) 'write value to persistent ROM
  pause(5_000)
  Get_All

pub Set_LED_All_On

  led_state := %111
  Get_All

pub Set_LED_All_Off

  led_state := %000
  Get_All

pub Get_All | temp

  enqueue(!s2.obstacle(s2#LEFT, 0) & 1)
  enqueue(!s2.obstacle(s2#RIGHT, 0) &1)

  temp := s2.light_sensor_raw_inv(s2#LEFT)
  enqueue((temp>>8)&$FF)
  enqueue((temp)&$FF)
  
  temp := s2.light_sensor_raw_inv(s2#CENTER)
  enqueue((temp>>8)&$FF)
  enqueue((temp)&$FF)

  temp := s2.light_sensor_raw_inv(s2#RIGHT)
  enqueue((temp>>8)&$FF)
  enqueue((temp)&$FF)

  enqueue(!s2.line_sensor(s2#LEFT, 0) & 1)
  enqueue(!s2.line_sensor(s2#RIGHT, 0) & 1)

  enqueue(s2.stalled & 1)

pub Get_Light_All | temp

  temp := s2.light_sensor_raw_inv(s2#LEFT)
  enqueue((temp>>8)&$FF)
  enqueue((temp)&$FF)
  
  temp := s2.light_sensor_raw_inv(s2#CENTER)
  enqueue((temp>>8)&$FF)
  enqueue((temp)&$FF)

  temp := s2.light_sensor_raw_inv(s2#RIGHT)
  enqueue((temp>>8)&$FF)
  enqueue((temp)&$FF)

pub Get_IR_All

  enqueue(!s2.obstacle(s2#LEFT, 0) & 1)
  enqueue(!s2.obstacle(s2#RIGHT, 0) & 1)

pub Get_Line_All

  enqueue(!s2.line_sensor(s2#LEFT, 0)  & 1)
  enqueue(!s2.line_sensor(s2#RIGHT, 0) & 1)

pub Reboot_S2
  
  reboot                        'actually soft-reboot the processor

pub Get_Name1 | i

  repeat i from 0 to 7
    enqueue(i2c.ReadByte(i2c#BootPin, i2c#EEPROM, @NameData + i))

pub Get_Name2 | i

  repeat i from 8 to 15
    enqueue(i2c.ReadByte(i2c#BootPin, i2c#EEPROM, @NameData + i))

pub Get_Pass1 | i

  repeat i from 0 to 7
    enqueue(i2c.ReadByte(i2c#BootPin, i2c#EEPROM, @PassData + i))

pub Get_Pass2 | i

  repeat i from 8 to 15
    enqueue(i2c.ReadByte(i2c#BootPin, i2c#EEPROM, @PassData + i))
    
pub Set_Motors_Off

  s2.stop_now
  Get_All

pub Set_Speaker

  ifnot quiet
    s2.play_tone((indata[1]<<8 | indata[2]), (indata[3]<<8 | indata[4]), 0)
    pause(1000 * (indata[1]<<8 | indata[2]))
  else
    pause(1000 * (indata[1]<<8 | indata[2]))

  Get_All

pub Set_Speaker2

  ifnot quiet
    s2.play_tone((indata[1]<<8 | indata[2]), (indata[3]<<8 | indata[4]), (indata[5]<<8 | indata[6]))
    pause(1000 * (indata[1]<<8 | indata[2]))
  else
    pause(1000 * (indata[1]<<8 | indata[2]))            'wait for the amount of time the tone would usually be played

  Get_All

pub Set_Name1 | i

  repeat i from 0 to DATA_LENGTH - 1
    i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @nameData + i, indata[1 +i])
    pause(5_000)

  Get_All
    
pub Set_Name2 | i

  repeat i from 0 to DATA_LENGTH - 1
    i2c.WriteByte(i2c#BootPin, i2c#EEPROM, @nameData + 8 + i, indata[1 +i])
    pause(5_000)
    
  Get_All

pub Get_Stall
  enqueue(s2.stalled & 1)

pub Get_Info

  enqueuestr(@roboData)

  

      



  
'=================================================================================================================================================|
'|                                                                                                                                                |
'|                                            Helper Methods Below                                                                                |
'|                                                                                                                                                |
'=================================================================================================================================================|
pub send_data | i

  repeat i from 0 to (outdata_idx - 1)  'only send the number of bytes in the outdata buffer (take one off because the index is already one ahead)
    ser.tx(outdata[i])          'Actually writes to the serial port here.
    'd.char(outdata[i])         'TEK, dump the data to debug serial port
                                
  outdata_idx~                  'reset the buffer index

pub enqueue(data)

  outdata[outdata_idx] := data  'add the data to the queue
  outdata_idx++                 'increment the queues index

pub enqueuestr(strptr)

  repeat strsize(strptr)
    enqueue(byte[strptr++])     'enqueue the string one byte at a time    

pub get_packet | i, inbyte

  repeat i from 0 to PACKET_LENGTH - 1
    inbyte := ser.rxtime(TIMEOUT)  'get the byte, but only wait for so long (TIMOUT milliseconds)
    
    if inbyte == -1          'if we timed out on any byte, bail with a failure result!!! :D
      return -1
    'd.char(indata[i])       'uncomment to check data on debug serial port
    indata[i] := inbyte

    if indata[0] <> $50      'Special case, dont re-echo the Get_Info command.
      enqueue(inbyte)        'Due to hardware design, the BS2 automatically echoes anything RXed.
                             'The Propeller does not do this, so queue up incoming data to simulate
                             'this behavior.

  return 0
  
pub reset_button
  repeat
    if(s2.button_press)
      Reboot_S2

PUB FaultMonitor : value
'---[Battery and Over-current Monitor Cog]-------------------------------------

  value := $ffff
  waitcnt(cnt + 80_000_000)
  repeat
    value <#= s2.get_adc_results(s2#ADC_VBAT)
    if value > constant((700*2550)/(400*33))      '7.0V
      s2.set_led(s2#POWER,s2#BLUE)
    elseif value > constant((600*2550)/(400*33))  '6.0V
      s2.set_led(s2#POWER,$20)
    else
      s2.set_led(s2#POWER,s2#BLINK_BLUE)
    if s2.get_adc_results(s2#ADC_IMOT) > 210
      cogstop(cogid)
      s2.stop_now
      s2.set_leds(s2#BLINK_RED,s2#BLINK_RED,s2#BLINK_RED,s2#OFF)
      repeat

pub Pause(duration)

  'This method pauses program execution for approximately [duration] micro seconds
  'so 1_000_000 would be approximately one second.  Doesnt account for instruction
  'execution time overhead.

  if duration < 381
    duration := 381             'lower bound limit. anything lower than this, we
                                'have to wait until the counter comes back around,
                                'which is MUCH longer than [duration].
  waitcnt(((clkfreq / 1_000_000) * duration) + cnt)

pub Play_Intro

  s2.set_volume(25)
  s2.set_voices(s2#SIN, s2#SIN)

  s2.set_leds(s2#RED, s2#OFF, s2#OFF, s2#BLUE)
  ifnot quiet
    s2.play_tone(80, 784, 0)
  pause(80_000)

  s2.set_leds(s2#RED, s2#YELLOW, s2#OFF, s2#BLUE)
  ifnot quiet
    s2.play_tone(80, 880, 0)
  pause(80_000)

  s2.set_leds(s2#RED, s2#YELLOW, s2#GREEN, s2#BLUE)
  ifnot quiet
    s2.play_tone(80, 698, 0)
  pause(80_000)

  s2.set_leds(s2#RED, s2#YELLOW, s2#OFF, s2#BLUE)
  ifnot quiet
    s2.play_tone(80, 349, 0)
  pause(80_000)

  s2.set_leds(s2#RED, s2#OFF, s2#OFF, s2#BLUE)
  ifnot quiet
    s2.play_tone(80, 523, 0)
  pause(80_000)

  s2.set_leds(s2#OFF, s2#OFF, s2#OFF, s2#BLUE)

pub update_leds

  if (led_state >> 2) & 1                               'update the state of the left LED
    s2.set_led(s2#LEFT,s2#GREEN)
  else
    s2.set_led(s2#LEFT,s2#OFF)
    
  if (led_state >> 1) & 1                               'update the state of the center LED
    s2.set_led(s2#CENTER,s2#GREEN)
  else
    s2.set_led(s2#CENTER,s2#OFF)

  if (led_state >> 0) & 1                               'update the state of the right LED
    s2.set_led(s2#RIGHT,s2#GREEN)
  else
    s2.set_led(s2#RIGHT,s2#OFF)
