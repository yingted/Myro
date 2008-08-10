#ifndef __MESSAGES_HEADERS_H__
#define  _MESSAGES_HEADERS_H__

const unsigned char SOFT_RESET = 33;

const unsigned char GET_ALL = 65;
const unsigned char GET_ALL_BINARY = 66;
const unsigned char GET_LIGHT_LEFT = 67;
const unsigned char GET_LIGHT_CENTER = 68;
const unsigned char GET_LIGHT_RIGHT = 69; 
const unsigned char GET_LIGHT_ALL = 70;
const unsigned char GET_IR_LEFT = 71;
const unsigned char GET_IR_RIGHT = 72;
const unsigned char GET_IR_ALL = 73;
const unsigned char GET_LINE_LEFT = 74;
const unsigned char GET_LINE_RIGHT = 75;
const unsigned char GET_LINE_ALL = 76;
const unsigned char GET_STATE = 77;
const unsigned char GET_NAME1 = 78;
const unsigned char GET_NAME2 = 64;
const unsigned char GET_STALL = 79;
const unsigned char GET_INFO = 80;
const unsigned char GET_DATA = 81;
const unsigned char GET_PASS1 = 50;
const unsigned char GET_PASS2 = 51;

/**Camera Message Headers **/
const unsigned char GET_RLE = 82;  //segmented and run-length encoded image
const unsigned char GET_IMAGE = 83; //entire 256 x 192 imagea in YUYV format
const unsigned char GET_JPEG_GRAY_HEADER=135;
const unsigned char GET_JPEG_GRAY_SCAN=136;
const unsigned char GET_JPEG_COLOR_HEADER=137;
const unsigned char GET_JPEG_COLOR_SCAN=138;

//windowed image, followed by which window
//look into what they mean by window? buffers?
const unsigned char GET_WINDOW = 84;  

//Followed with the address, for the desired parameter.
const unsigned char GET_CAM_PARAM = 92; 

const unsigned char SET_CAM_PARAM = 131;
const unsigned char SET_WHITE_BALANCE = 129; //must be turned on
const unsigned char SET_NO_WHITE_BALANCE = 130; //default setting
/** end Camera Message Headers **/

const unsigned char GET_DONGLE_L_IR = 85;
const unsigned char GET_DONGLE_C_IR = 86;
const unsigned char GET_DONGLE_R_IR = 87;
const unsigned char GET_WINDOW_LIGHT = 88;
const unsigned char GET_BATTERY = 89;
const unsigned char GET_SERIAL_MEM = 90;
const unsigned char GET_SCRIB_PROGRAM = 91;

const unsigned char GET_BLOB = 95; //Need to look into this message

const unsigned char SET_PASS1 = 55;
const unsigned char SET_PASS2 = 56;
const unsigned char SET_SINGLE_DATA = 96;
const unsigned char SET_DATA = 97;
const unsigned char SET_ECHO_MODE = 98;
const unsigned char SET_LED_LEFT_ON = 99;
const unsigned char SET_LED_LEFT_OFF = 100;
const unsigned char SET_LED_CENTER_ON = 101;
const unsigned char SET_LED_CENTER_OFF = 102;
const unsigned char SET_LED_RIGHT_ON = 103;
const unsigned char SET_LED_RIGHT_OFF = 104;
const unsigned char SET_LED_ALL_ON = 105;
const unsigned char SET_LED_ALL_OFF = 106;
const unsigned char SET_LED_ALL = 107;
const unsigned char SET_MOTORS_OFF = 108;
const unsigned char SET_MOTORS = 109;
const unsigned char SET_NAME1 = 110;
const unsigned char SET_NAME2 = 119;
const unsigned char SET_LOUD = 111;
const unsigned char SET_QUIET = 112;
const unsigned char SET_SPEAKER = 113;
const unsigned char SET_SPEAKER_2 = 114;

const unsigned char SET_DONGLE_LED_ON = 116;
const unsigned char SET_DONGLE_LED_OFF = 117;
const unsigned char SET_RLE = 118;
const unsigned char SET_DONGLE_IR = 120;

const unsigned char SET_SERIAL_MEM = 121;
const unsigned char SET_SCRIB_PROGRAM = 122;
const unsigned char SET_RESET_SCRIBBLE = 123;
const unsigned char SET_SERIAL_ERASE = 125;
const unsigned char SET_DIMMER_LED = 126;
const unsigned char SET_WINDOW = 127;
const unsigned char SET_FORWARDNESS = 128;

#endif

