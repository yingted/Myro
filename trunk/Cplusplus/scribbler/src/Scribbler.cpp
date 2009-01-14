#include "Scribbler.h"
#include "message_headers.h"
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <assert.h>
#include <iostream>
#include <Magick++.h>
#include "ColorPicture.h"
#include "GrayPicture.h"

using namespace Magick;

#define PACKET_SIZE 9
#define ECHO_ON 1
#define ECHO_OFF 0
#define SENSOR_ON 1
#define SENSOR_OFF 0

/** Begin C code for handling Control C through the robot Lib **/
typedef struct exit_struct {
	Scribbler * robot;
} *_exit_struct;

_exit_struct exit_robot;

extern "C" {
	void ctrlc_routine(int foo) {
		printf("Closing Connection to Robot\n");
		exit_robot->robot->disconnect();
		delete exit_robot->robot;
		free(exit_robot);
		exit(0);
	}

	void create_exit_robot(Scribbler * robot) {
		exit_robot = (_exit_struct)malloc(sizeof(struct exit_struct));
		exit_robot->robot = robot;
		signal(SIGINT, ctrlc_routine);
	}
}
/** End C code for handling Control C through the robot lib **/

Scribbler::Scribbler() {
	con = NULL;
	lastRotate = 0;
	lastTranslate = 0;
	dongle = 0;

	//11 bytes for sensor information!
	sensors = (unsigned char *)malloc(sizeof(unsigned char) * 11);

	CAM_PID = (unsigned char)0x0A;
	CAM_PID_DEFAULT = (unsigned char)0x76;

	CAM_VER = (unsigned char)0x0B;
	CAM_VER_DEFAULT = (unsigned char)0x48;

	CAM_BRT = (unsigned char)0x06;
	CAM_BRT_DEFAULT = (unsigned char)0x80;

	CAM_EXP = (unsigned char)0x10;
	CAM_EXP_DEFAULT = (unsigned char)0x41;

	CAM_COMA = (unsigned char)0x12;
	CAM_COMA_DEFAULT = (unsigned char)0x14;
	CAM_COMA_WHITE_BALANCE_ON = (CAM_COMA_DEFAULT | (1 << 2));
	CAM_COMA_WHITE_BALANCE_OFF = (CAM_COMA_DEFAULT & ~(1 << 2));

	CAM_COMB = (unsigned char)0x13;
	CAM_COMB_DEFAULT = (unsigned char)0xA3;
	CAM_COMB_GAIN_CONTROL_ON = (CAM_COMB_DEFAULT | (1 << 1));
	CAM_COMB_GAIN_CONTROL_OFF = (CAM_COMB_DEFAULT & ~(1 << 1));
	CAM_COMB_EXPOSURE_CONTROL_ON = (CAM_COMB_DEFAULT | (1 << 0));
	CAM_COMB_EXPOSURE_CONTROL_OFF = (CAM_COMB_DEFAULT & ~(1 << 0));

	robot_lock = (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t));
	image_lock = (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t));
	pthread_mutex_init(robot_lock, NULL);
	pthread_mutex_init(image_lock, NULL);
	/**
	 * ctrl+c exit hack, to make sure the connect is close correctly.
	 **/
	create_exit_robot(this);
}

Scribbler::~Scribbler() {
	free(robot_lock);
	free(image_lock);
	free(sensors);
	delete con;
}

int Scribbler::connect() {
	std::string device;
	std::cout << "Please enter the comport: ";
	std::cin >> device;
	con = new serial(38400, 1);
	int status = con->connect(device.c_str());
	if(status < 0) {
		fprintf(stderr, "Failed to connect to %s\n", device.c_str());
		delete con;
		con = NULL;
		return -1;
	}
	con->flush_input();
	con->flush_output();


	//THIS INFO CODE IS POTIENTIALLY BUGGY! NEED TO FIND A CLEAN FIX

	size_t found;
	std::vector<std::string> info = getInfo();
	for(int i = 0; i < (int)info.size(); i++) {
		found = info.at(i).find("fluke");
		if( found != std::string::npos ) {
			char * tempStr = (char*)info.at(i).c_str();
			char * col_pt = strchr(tempStr, ':') + 1;
			char * end_str = strchr(tempStr, 0);

			printf("Using Fluke Version: ");
			for(int i = 0; col_pt + i < end_str; i++)
				printf("%c", col_pt[i]);
			printf("\n");	
			this->dongle = 1;
			break;
		}
	}

	if(dongle) {
		//configures the robot's camera after connecting
		set_cam_param(CAM_COMA, CAM_COMA_WHITE_BALANCE_ON);
		set_cam_param(CAM_COMB, 
				CAM_COMB_GAIN_CONTROL_OFF | CAM_COMB_EXPOSURE_CONTROL_OFF);
		conf_gray_window(0,   2, 0, 128, 191, 1, 1);
		conf_gray_window(1,  64, 0, 190, 190, 1, 1);
		conf_gray_window(2, 128, 0, 254, 191, 1, 1);
		conf_rle(90, 4, 0, 255, 51, 136, 190, 255);
		set_ir_power(135);
		con->flush_input();
		con->flush_output();
	}
	this->restart();
	return 0;
}

int Scribbler::disconnect() {
	int status = 0;
	//pthread_mutex_lock(this->robot_lock);
	if(con) {
		con->disconnect();
	}
	else {
		fprintf(stderr, "Attempting to disconnect without connecting\n");
		status = -1;
	}
	//pthread_mutex_unlock(this->robot_lock);
	return status;
}

void Scribbler::move(double translate, double rotate, double z) {
	lastRotate = rotate;
	lastTranslate = translate;
	_adjustSpeed(translate, rotate);
}

void Scribbler::rotate(double amount) {
	lastRotate = amount;
	_adjustSpeed(lastTranslate, amount);
}

void Scribbler::translate(double amount) {
	lastTranslate = amount;
	_adjustSpeed(amount, lastRotate);
}

/* No longer required, it is now inherited from Robot
 * void Scribbler::stop() {
	lastRotate = 0;
	lastTranslate = 0;
	_adjustSpeed(lastTranslate, lastRotate);
}*/

void Scribbler::hardStop() {
	this->_set(SET_MOTORS_OFF, -1);
	lastTranslate = 0;
	lastRotate = 0;
}

void Scribbler::restart() {
	con->flush_input();
	con->flush_output();
	this->setEchoMode(0);
	this->stop();
	this->set("led", "all", 0);
	this->beep(.03, 784);
	this->beep(.03, 880);
	this->beep(.03, 698);
	this->beep(.03, 349);
	this->beep(.03, 523);
	std::string name = this->getName();
	printf("Hello, I'm %s\n", name.c_str());
}

void Scribbler::beep(double duration, int freq, int freq2) {
	if(freq2) {
		_set_speaker(freq, freq2, (int)(duration * 1000));
	}
	else
		_set_speaker(freq, (int)(duration * 1000));
}

void Scribbler::_adjustSpeed(double translate, double rotate) {
	double left  
		= std::min(std::max(translate - rotate, -1.0), 1.0);
	double right  
		= std::min(std::max(translate + rotate, -1.0), 1.0);

	double lP = (left + 1.0) * 100;
	double rP = (right + 1.0) * 100;

	int leftPower = (int)(lP);
	int rightPower  = (int)(rP);

	//printf("Lp:	   %lf Rp	 %lf\n", lP, rP);
	//printf("uc Lp: %i uc Rp: %i\n", leftPower, rightPower);

	this->_set(SET_MOTORS, rightPower, leftPower, -1);
}

int Scribbler::_set(int message_header, ...) {
	pthread_mutex_lock(this->robot_lock);
	va_list list;
	int nargs = 1;
	int next;

	va_start( list, message_header );
	for( ; (next = va_arg( list, int )) != -1; nargs++); 
	va_end(list);

	unsigned char * message_buf 
		= (unsigned char *)malloc(sizeof(unsigned char) * nargs);
	va_start( list, message_header );
	message_buf[0] = (unsigned char)message_header;
	for(int i = 1; (next = va_arg(list, int)) != -1; i++) {
		message_buf[i] = (unsigned char)next;
	}
	va_end(list);

	int status =  send_message(message_buf, nargs);
	free(message_buf);
	pthread_mutex_unlock(this->robot_lock);
	return status;
}

int * Scribbler::_get(int message_header, int bytes, std::string mode) {
	pthread_mutex_lock(this->robot_lock);
	send_message( (unsigned char*)(&message_header), 1, ECHO_ON, SENSOR_OFF);

	unsigned char * buffer = 
		(unsigned char*)malloc(sizeof(unsigned char) * bytes);

	int * result;
	if(mode == "byte" || mode == "word") {
		this->read_message(buffer, bytes);
		if(mode == "word") {
			//bytes better be even!
			result = (int*)malloc(sizeof(int) * bytes/2);
			for(int i = 0, j = 0; i < bytes/2; i++, j+=2) 
				result[i] = (((int)buffer[j]) << 8 | ((int)buffer[j+1]));	
		}
		else {
			result = (int*)malloc(sizeof(int) * bytes);
			for(int i = 0; i < bytes; i++)
				result[i] = (int)buffer[i];
		}
	}
	else if(mode == "line") {
		unsigned char temp;
		std::string tResult = "";
		this->read_message(&temp, 1);
		tResult += temp;
		while(temp != '\n') {
			this->read_message(&temp, 1);
			tResult += temp;
		}
		result = (int*)malloc(sizeof(int) * tResult.length() + 1);
		for(int i = 0; i < (int)tResult.length(); i++) {
			result[i] = tResult.at(i);
		}
		result[tResult.length()] = 0;
	}
	free(buffer);
	pthread_mutex_unlock(this->robot_lock);
	return result;
}

int Scribbler::_set_speaker(int freq, int duration) {
	return this->_set(SET_SPEAKER, duration >> 8, duration % 256,
									freq >> 8, freq % 256, -1);

}

int Scribbler::_set_speaker(int freq, int freq2, int duration) {
	return this->_set(SET_SPEAKER_2, duration >> 8, duration % 256,
			freq >> 8, freq % 256, freq2 >> 8, freq2 % 256, -1);
}

int Scribbler::set_ir_power(int irpower) {
	unsigned char buffer[2];
	buffer[0] = SET_DONGLE_IR;
	buffer[0] = irpower;
	pthread_mutex_lock(this->robot_lock);
	int status = con->_write(buffer, 2);
	pthread_mutex_unlock(this->robot_lock);
	return status;
}

int Scribbler::set_cam_param(unsigned char addr, unsigned char byte) {
	unsigned char buffer[3];
	buffer[0] = SET_CAM_PARAM;
	buffer[1] = addr;
	buffer[2] = byte;
	pthread_mutex_lock(this->robot_lock);
	int status = con->_write(buffer, 3);
	pthread_mutex_unlock(this->robot_lock);
	usleep(50000); //sleep used to give camera time to reconfigure
	return status;
}

int Scribbler::conf_window(unsigned char window, unsigned char x_low,
		unsigned char y_low, unsigned char x_high, unsigned char y_high,
		unsigned char x_step, unsigned char y_step) {
	unsigned char buffer[8];
	buffer[0] = SET_WINDOW;
	buffer[1] = window;
	buffer[2] = x_low;
	buffer[3] = y_low;
	buffer[4] = x_high;
	buffer[5] = y_high;
	buffer[6] = x_step;
	buffer[7] = y_step;
	pthread_mutex_lock(this->robot_lock);
	int status = con->_write(buffer, 8);
	pthread_mutex_unlock(this->robot_lock);
	return status;
}

int Scribbler::conf_gray_window(unsigned char window, unsigned char lx,
		unsigned char ly, unsigned char ux, unsigned char uy,
		unsigned char xstep, unsigned char ystep) {

	if(lx % 2 == 0)
		lx += 1;
	if( xstep % 2 == 1)
		xstep += 1;
	conf_window(window, lx, ly, ux, uy, xstep, ystep);

	return 0;
}

int Scribbler::conf_gray_image() {
	return conf_window(0, 1, 0, 255, 191, 2, 2);
}

int Scribbler::conf_rle(unsigned char delay, unsigned char smooth_tresh,
		unsigned char y_low, unsigned char y_high,
		unsigned char u_low, unsigned char u_high,
		unsigned char v_low, unsigned char v_high) {

	unsigned char buffer[9];
	buffer[0] = SET_RLE;
	buffer[1] = delay;
	buffer[2] = smooth_tresh;
	buffer[3] = y_low;
	buffer[4] = y_high;
	buffer[5] = u_low;
	buffer[6] = u_high;
	buffer[7] = v_low;
	buffer[8] = v_high;
	pthread_mutex_lock(this->robot_lock);
	int status = con->_write(buffer, 9);
	pthread_mutex_unlock(this->robot_lock);
	return status;
}

unsigned char * Scribbler::grab_array_rgb() {
	int width = 256;
	int height = 192;

	unsigned char * yuv_buffer 
		= (unsigned char*)malloc(sizeof(unsigned char) * width * height);
	unsigned char * rgb_buffer
		= (unsigned char*)malloc(sizeof(unsigned char) * width * height * 3);
	memset(rgb_buffer, 'B', sizeof(unsigned char) * width * height * 3);

	int size = width* height;
	unsigned char command = GET_IMAGE;

	//Black Voodoo Magic, since I don't know the YUV encode format
	int vy, vu, y1v, y1u, uy, uv, y2u, y2v;

	int Y = 0,U = 0,V = 0;

	pthread_mutex_lock(this->robot_lock);
	con->_write(&command, 1);
	con->_read(yuv_buffer, size);
	pthread_mutex_unlock(this->robot_lock);

	//printf("Decoding Image\n");
	for(int i = 0; i < height; i++) {
		for(int j = 0; j < width; j++) {
			if( j >= 3 ) {
				vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2;
				y2u = -1; y2v = -3;
			}
			else {
				vy = 1; vu = 2; y1v = 3; y1u = 1; uy = 1; uv = 2; 
				y2u = 3; y2v = 1;
			}
			
			//VYUY VYUY VYUY
			if( j % 4 == 0 ) {
				V = (int)yuv_buffer[i * width + j];
				Y = (int)yuv_buffer[i * width + j + vy];
				U = (int)yuv_buffer[i * width + j + vu];
			}
			if( j % 4 == 1 ) {
				Y = (int)yuv_buffer[i * width + j];
				V = (int)yuv_buffer[i * width + j + y1v];
				U = (int)yuv_buffer[i * width + j + y1u];
			}
			if( j % 4 == 2 ) {
				U = (int)yuv_buffer[i * width + j];
				Y = (int)yuv_buffer[i * width + j + uy];
				V = (int)yuv_buffer[i * width + j + uv];
			}
			if( j % 4 == 3 ) {				 
				Y = (int)yuv_buffer[i * width + j];
				U = (int)yuv_buffer[i * width + j + y2u];
				V = (int)yuv_buffer[i * width + j + y2v];
			}

			assert( Y <= 255 && U <= 255 && V <= 255);

			/*if(i == 0 && j == 0) {
				printf("Y: %i U: %i V: %i\n", Y, U, V);
			}*/

			U = U - 128;
			V = V - 128;

			double R =  
				std::max(std::min(Y + 1.13983 * V, 255.0), 0.0);
			double G =
				std::max(std::min(Y - 0.39466 * U - 0.7169 * V, 255.0),0.0);
			double B =
				std::max(std::min(Y + 2.03211 * U, 255.0), 0.0);

			rgb_buffer[(i * width + j) * 3 + 0] = (unsigned char)R;
			rgb_buffer[(i * width + j) * 3 + 1] = (unsigned char)G;
			rgb_buffer[(i * width + j) * 3 + 2] = (unsigned char)B;
		}
	}
	//printf("Image Decoded\n");
	free(yuv_buffer);
	return rgb_buffer;
}

unsigned char * Scribbler::grab_gray_array() {
	int width = 128;
	int height = 96;
	int size = width * height;

	unsigned char * sm_gray_buffer
		= (unsigned char *)malloc(sizeof(unsigned char) * size);

	unsigned char command = GET_WINDOW;
	unsigned char value = 0;//(char)48;
	con->_write(&command, 1);
	con->_write(&value, 1);

	//printf("Reading Image\n");

	read_message(sm_gray_buffer, size);

	//printf("Image Read\n");

	unsigned char * fl_gray_buffer 
		= quadrupleSize(sm_gray_buffer, width, size);
	free(sm_gray_buffer);

	return fl_gray_buffer;
}

unsigned char * Scribbler::quadrupleSize(unsigned char * gb, 
		int width, int size) {
	int row = 0, col = 0;

	unsigned char * fg_buffer
		= (unsigned char *)malloc(sizeof(unsigned char) * size * 4);

	for(int i = 0; i < size; i++) {
		fg_buffer[row		* 2 * width + col]	 = gb[i];
		fg_buffer[row		* 2 * width + col+1] = gb[i];
		fg_buffer[(row + 1) * 2 * width + col]	 = gb[i];
		fg_buffer[(row + 1) * 2 * width + col+1] = gb[i];
		col += 2;
		if( col == (width * 2) ) {
			col = 0;
			row += 2;
		}
	}
	return fg_buffer;
}

unsigned char * Scribbler::grab_blob_array() {
	int width = 256;
	int height = 192;

	int size;

	unsigned char * blobs 
		= (unsigned char *)malloc(sizeof(unsigned char) * (width+1) 
				* (height + 1));
	unsigned char * rgb_blob
		= (unsigned char *)malloc(sizeof(unsigned char) * width * height * 3);

	unsigned char * data_buffer;

	pthread_mutex_lock(this->robot_lock);

	unsigned char command = GET_RLE;
	unsigned char size_buffer[2] = {0,0};
	con->_write(&command, 1);
	con->_read(size_buffer, 2);

	size = (size_buffer[0] << 8) | size_buffer[1];
	//printf("size %i\n", size);

	data_buffer = (unsigned char*)malloc(sizeof(unsigned char) * size);
	con->_read(data_buffer, size);
	//pthread_mutex_unlock(this->robot_lock);

	int inside = 0;
	int counter = 0;
	int val = 128;
	int px = 0;

	for(int i = 0; i < height; i++) {
		for(int j = 0; j < width; j+=4) {
			if(counter < 1 && px < size) {
				counter = (int)data_buffer[px];
				px += 1;
				counter = (counter << 8) | data_buffer[px];
				px += 1;
				//printf("Counter %i\n", counter);
				if(inside) {
					val = 0;
					inside = 0;
				}
				else {
					val = 255;
					inside = 1;
				}
			}
			for(int k = 0; k < 4; k++) {
				blobs[(i * width) + j + k] = val;
			}
			counter -=  1;
		}
	}

	for(int i = 0; i < height; i++) {
		for(int j = 0; j < width; j++) {
			for(int k = 0; k < 3; k++)
				rgb_blob[(i * width) * 3 + (j * 3) + k] 
					= blobs[(i * width) + j];
		}
	}
	free(blobs);
	pthread_mutex_unlock(this->robot_lock);
	return rgb_blob;
}

unsigned char * Scribbler::read_jpeg_header(int &size) {
	unsigned char jpeg_header_size[2];
	unsigned char * jpeg_header;

	jpeg_header_size[0] = 0;
	jpeg_header_size[1] = 0;

	read_message(jpeg_header_size, 2);
	size = (int)jpeg_header_size[0] + (int)jpeg_header_size[1] * 256;

	//printf("jpeg_header_size %i\n", size);

	jpeg_header 
		= (unsigned char*)malloc(sizeof(unsigned char) * size);
	read_message(jpeg_header, size);

	return jpeg_header;
}

std::vector<unsigned char> Scribbler::read_jpeg_scan() {
	std::vector<unsigned char> jpeg_scan_vect;
	unsigned char last_byte = 0;
	unsigned char byte = 0;

	unsigned char int_buffer[4];
	unsigned int bm0;
	unsigned int bm1;
	unsigned int bm2;

	while(1) {
		read_message(&byte, 1);
		jpeg_scan_vect.push_back(byte);
		if( last_byte == 0xff && byte == 0xd9 ) 
			break;
		last_byte = byte;
	}
	read_message(int_buffer, 4);
	bm0 = int_buffer[0] + int_buffer[1] * 256 + 
		int_buffer[2] * 65536 + int_buffer[3] *16777216;
	read_message(int_buffer, 4);
	bm1 = int_buffer[0] + int_buffer[1] * 256 + 
		int_buffer[2] * 65536 + int_buffer[3] *16777216;
	read_message(int_buffer, 4);
	bm2 = int_buffer[0] + int_buffer[1] * 256 + 
		int_buffer[2] * 65536 + int_buffer[3] *16777216;

	//printf("%lf %lf\n", ((double)(bm1 - bm0))/60e6, 
	//	((double)(bm2 - bm1) / 60e6) );

	return jpeg_scan_vect;
}

unsigned char * Scribbler::grab_jpeg_color(int reliable, int &size) {
	pthread_mutex_lock(this->robot_lock);
	unsigned char * jpeg_header;
	int jpeg_header_size = 0;
	std::vector<unsigned char> jpeg_scan;
	unsigned char command[2] 
		= {GET_JPEG_COLOR_HEADER, (unsigned char)reliable};
	unsigned char * jpeg_buffer;
	unsigned char * decompressedResizedJpeg;

	con->_write(command, 1);
	jpeg_header = read_jpeg_header(jpeg_header_size);

	command[0] = GET_JPEG_COLOR_SCAN;
	con->_write(command, 2);
	jpeg_scan = read_jpeg_scan();

	size = jpeg_scan.size() + jpeg_header_size;
	//printf("JpegScanSize: %i JpegHeaderSize: %i\n",
	//		jpeg_scan.size(), jpeg_header_size);
	jpeg_buffer 
		= (unsigned char*)malloc(sizeof(unsigned char) * size);
	
	int jpeg_buffer_index = 0;
	for(; jpeg_buffer_index < jpeg_header_size; jpeg_buffer_index++) 
		jpeg_buffer[jpeg_buffer_index] = jpeg_header[jpeg_buffer_index];

	for(jpeg_buffer_index = 0; 
			jpeg_buffer_index < (int)jpeg_scan.size(); jpeg_buffer_index++) 
		jpeg_buffer[jpeg_buffer_index + jpeg_header_size]
			= jpeg_scan.at(jpeg_buffer_index);

	decompressedResizedJpeg = jpegStretch(jpeg_buffer, 1, size);
	free(jpeg_buffer);
	pthread_mutex_unlock(this->robot_lock);
	return decompressedResizedJpeg;
}

unsigned char * Scribbler::grab_jpeg_gray(int reliable, int &size) {
	pthread_mutex_lock(this->robot_lock);

	int jpeg_header_size = 0;
	std::vector<unsigned char> jpeg_scan;
	unsigned char * jpeg_header;
	unsigned char * jpeg_buffer;
	unsigned char * decompressedResizedJpeg;
	unsigned char command[2] 
		= {GET_JPEG_GRAY_HEADER, (unsigned char)reliable};

	con->_write(command, 1);
	jpeg_header = read_jpeg_header(jpeg_header_size);

	command[0] = GET_JPEG_GRAY_SCAN;
	con->_write(command, 2);
	jpeg_scan = read_jpeg_scan();


	size = jpeg_scan.size() + jpeg_header_size;
	//printf("JpegScanSize: %i JpegHeaderSize: %i\n",
	//		jpeg_scan.size(), jpeg_header_size);
	jpeg_buffer
		= (unsigned char*)malloc(sizeof(unsigned char) * size);

	int jpeg_buffer_index = 0;
	for(; jpeg_buffer_index < jpeg_header_size; jpeg_buffer_index++)
		jpeg_buffer[jpeg_buffer_index] = jpeg_header[jpeg_buffer_index];

	for(jpeg_buffer_index = 0;
			jpeg_buffer_index < (int)jpeg_scan.size(); jpeg_buffer_index++)
		jpeg_buffer[jpeg_buffer_index + jpeg_header_size]
			= jpeg_scan.at(jpeg_buffer_index);

	decompressedResizedJpeg = jpegStretch(jpeg_buffer, 0, size);
	free(jpeg_buffer);
	pthread_mutex_unlock(this->robot_lock);
	return decompressedResizedJpeg;
}

unsigned char * Scribbler::jpegStretch(unsigned char * jpegBuffer, 
		int color_space, int &size) {

	unsigned char * decompressedBuffer;
	unsigned char * resizedDecompressedBuffer;

	Blob jpegBlob(jpegBuffer, size);

	Image jpegImage;
	jpegImage.magick("JPG");
	jpegImage.size("256x192");
	jpegImage.read(jpegBlob);

	switch(color_space) {
		case 0:		decompressedBuffer 
					= (unsigned char*)malloc(sizeof(unsigned char) 
							* 128 * 192);
					resizedDecompressedBuffer 
					= (unsigned char*)malloc(sizeof(unsigned char) 
							* 256 * 192);
					jpegImage.write(0, 0,
							128, 192, "G", CharPixel, decompressedBuffer);
					for(int h = 0; h < 192; h++) {
						for(int w = 0; w < 128; w++) {
							resizedDecompressedBuffer[(h * 256) + (2 * w)] 
								= decompressedBuffer[(h * 128) + w];
							resizedDecompressedBuffer[(h * 256) + (2 * w + 1)]
								= decompressedBuffer[(h * 128) + w];
						}
					}
							
			break;
		case 1:		decompressedBuffer
					= (unsigned char*)malloc(sizeof(unsigned char)
							* 128 * 192 * 3);
					resizedDecompressedBuffer
					= (unsigned char*)malloc(sizeof(unsigned char)
							* 256 * 192 * 3);
					jpegImage.write(0, 0,
							128, 192, "RGB", CharPixel, decompressedBuffer);
					for(int h = 0; h < 192; h++) {
						for(int w = 0; w < 128; w++) 
							for(int rgb = 0; rgb < 3; rgb++) {
								resizedDecompressedBuffer
									[(h * 256 * 3) + (2 * w * 3) + rgb]
									= decompressedBuffer
									[(h * 128 * 3) + (w * 3) + rgb];
								resizedDecompressedBuffer
									[(h * 256 * 3) + ((2 * w + 1) * 3) + rgb]
									= decompressedBuffer
									[(h * 128 * 3) + (w * 3) + rgb];
							}
						
					}
			break;
		default:	fprintf(stderr, "Invalid Color Space!\n");
					return NULL;
			break;
	}
	free(decompressedBuffer);
	return resizedDecompressedBuffer;
}

int Scribbler::set(std::string item, int position, int value) {

	std::string item_lower = "";
	for(int i = 0; i < (int)item.length(); i++)
		item_lower += tolower(item.at(i));

	if(item_lower == "led") {
		switch(position) {
			case 0: if(value)
						this->_set(SET_LED_LEFT_ON, -1);
					else
						this->_set(SET_LED_LEFT_OFF, -1);
				break;
			case 1: if(value)
						this->_set(SET_LED_CENTER_ON, -1);
					else
						this->_set(SET_LED_CENTER_OFF, -1);
				break;
			case 2: if(value)
						this->_set(SET_LED_RIGHT_ON, -1);
					else
						this->_set(SET_LED_RIGHT_OFF, -1);
				break;
			default: std::cout << "unknown LED: " << position << std::endl;
		}
	}
	else if(item_lower == "data") {
	}
	else if(item_lower == "volume") {
		if(position) {
			 this->_set(SET_LOUD, -1);
			 volume = 1;
		}
		else {
			 this->_set(SET_QUIET, -1);
			 volume = 0;
		}
	}
	else if(item_lower == "echomode") {
		this->setEchoMode(position);
	}
	else if(item_lower == "whitebalance") {
		this->setWhiteBalance(position);
	}
	else if(item_lower == "irpower") {
		this->setIRPower(position);
	}
	else if(item_lower == "forwardness") {
		this->setForwardness(position);
	}
	return 0;
}

int Scribbler::set(std::string item, std::string position, double value) {

	std::string item_lower = "";
	for(int i = 0; i < (int)item.length(); i++) 
		item_lower += tolower(item.at(i));

	std::string position_lower = "";
	for(int i = 0; i < (int)position.length(); i++) {
		position_lower += tolower(position.at(i));
	}

	if(item_lower == "led") {
		if(position_lower == "center") {
			if((int)value)
				this->_set(SET_LED_CENTER_ON,-1);
			else
				this->_set(SET_LED_CENTER_OFF,-1);
		}
		else if( position_lower == "left") {
			if((int)value)
				this->_set(SET_LED_LEFT_ON,-1);
			else
				this->_set(SET_LED_LEFT_OFF,-1);
		}
		else if( position_lower == "right") {
			if((int)value)
				this->_set(SET_LED_RIGHT_ON,-1);
			else
				this->_set(SET_LED_RIGHT_OFF,-1);
		}
		else if( position_lower == "front") {
			this->setLEDFront(value);
		}
		else if ( position_lower == "back") {
			this->setLEDBack(value);
		}
		else if( position_lower == "all") {
			if((int)value)
				this->_set(SET_LED_ALL_ON, -1);
			else
				this->_set(SET_LED_ALL_OFF, -1);
		}
	}
	else if(item_lower == "name") {
		this->setName(position);
	}
	else if(item_lower == "password") {
		this->setPassword(position);
	}
	else if(item_lower == "startsong") {
		//not sure what to do with this
	}
	else if(item_lower == "forwardness"){
		setForwardness(position_lower);
	}

	return 0;
}

void * Scribbler::get(std::string sensor, std::vector<std::string> position) {
	void * retval = NULL;

	for(int i = 0; i < (int)sensor.length(); i++)
		sensor[i] = tolower(sensor.at(i));

	if(sensor == "config"){
		std::vector< std::pair<std::string,int> > * result
			= new std::vector< std::pair<std::string, int> >();
		result->push_back( std::pair<std::string, int>("ir", 2) );
		result->push_back( std::pair<std::string, int>("line", 2) );
		result->push_back( std::pair<std::string, int>("stall", 1) );
		result->push_back( std::pair<std::string, int>("light", 3) );
		if(dongle) {
			result->push_back( std::pair<std::string, int>("battery", 1));
			result->push_back( std::pair<std::string, int>("obstacle", 3));
			result->push_back( std::pair<std::string, int>("bright", 3));
		}
		retval = (void*)result;
	}
	else if (sensor == "stall") {
		int * result = this->_get(GET_ALL, 11);
		for(int i = 0; i < 11; i++)
			sensors[i] = (unsigned char)result[i];
		free(result);
		retval = (void*)(sensors + 10);
	}
	else if (sensor == "forwardness") {
		std::string * result;
		if( read_mem(0,0) != 0xDF ) 
			result = new std::string("fluke-forward");
		else
			result = new std::string("scribbler-forward");
		retval = (void*)result;
	}
	else if (sensor == "startsong") {
		//not implemented yet in myro
		std::string * result = new std::string("tada");
		retval = (void*)result;
	}
	else if (sensor == "version") {
		//TODO: determine an effective way to return a version
	}
	else if (sensor == "data") {
		std::vector<int> * data 
			= new std::vector<int>(this->getData());
		retval = (void*)data;
	}
	else if (sensor == "info") {
		std::vector<std::string> * info
			= new std::vector<std::string>(getInfo());
		retval = (void*)info;
	}
	else if (sensor == "name") {
		std::string * name = new std::string(this->getName());
		retval = (void*)name;
	}
	else if (sensor == "password") {
		std::string * pass = new std::string(this->getPassword());
		retval = (void*)pass;
	}
	else if (sensor == "volume") {
		int * result = new int[1];
		result[0] = this->volume;
		retval = (void*)result;
	}
	else if (sensor == "battery") {
		double * result = new double[1];
		result[0] = getBattery();
		retval = (void*)result;
	}
	else if (sensor == "blob") {
		std::vector<int> * blob 
			= new std::vector<int>(this->getBlob());
		retval = (void*)blob;
	}
	else { 
		if ( position.size() == 0 ) {
			if(sensor == "light") {
				int * light = this->_get(GET_LIGHT_ALL, 6, "word");
				std::vector<int> * list = new std::vector<int>();
				for(int i = 0; i < 3; i++)
					list->push_back( light[i] );
				free(light);
				retval = (void*)list;
			}
			else if( sensor == "line") {
				int * line = this->_get(GET_LINE_ALL, 2);
				std::vector<int> * list = new std::vector<int>();
				for(int i = 0; i < 3; i++)
					list->push_back( line[i] );
				free(line);
				retval = (void*)list;
			}
			else if( sensor == "obstacle") {
				std::vector<int> * obstacle = new std::vector<int>();
				obstacle->push_back( getObstacle("left") );
				obstacle->push_back( getObstacle("center") );
				obstacle->push_back( getObstacle("right") );
				retval = (void*)obstacle;
			}
			else if( sensor == "bright")  {
				std::vector<int> * bright = new std::vector<int>();
				bright->push_back( getBright("left") );
				bright->push_back( getBright("center") );
				bright->push_back( getBright("right") );
				retval = (void*)bright;
			}
			else if( sensor == "all") {
				//TODO: figure out a good data structure to hold this data
				if(dongle) {
				}
				retval = (void*)retval;
			}
			else
				std::cout << "invalid sensor name: " << sensor << std::endl;
		}
		else {
			if(sensor == "light") {
				int * values = this->_get(GET_LIGHT_ALL, 6, "word");
				std::vector<int>* light = new std::vector<int>();
				for(int i = 0; i < (int)position.size(); i++) {
					if(position.at(i) == "left") 
						light->push_back(values[0]);
					else if(position.at(i) == "middle" || 
							position.at(i) == "center" ) 
						light->push_back(values[1]);
					else if(position.at(i) == "right") 
						light->push_back(values[2]);
					else if(position.at(i) == "all") 
						for(int j = 0; j < 3; j++)
							light->push_back(values[j]);
					free(values);
					values = this->_get(GET_LIGHT_ALL, 6, "word");
				}
				free(values);
				retval = (void*)light;
			}
			else if( sensor == "ir" ) {
				int * values = this->_get(GET_IR_ALL, 2);
				std::vector<int> * ir = new std::vector<int>();
				for(int i = 0; i < (int)position.size(); i++) {
					if(position.at(i) == "left")
						ir->push_back(values[0]);
					if(position.at(i) == "right") 
						ir->push_back(values[1]);
					if(position.at(i) == "all") {
						ir->push_back(values[0]);
						ir->push_back(values[1]);
					}
					free(values);
					values = this->_get(GET_IR_ALL, 2);
				}
				free(values);
				retval = (void*)ir;
			}
			else if( sensor == "line" ) {
				int * values = this->_get(GET_LINE_ALL, 2);
				std::vector<int> * line = new std::vector<int>();
				for(int i  = 0; i < (int)position.size(); i++) {
					if(position.at(i) == "left") 
						line->push_back(values[0]);
					else if(position.at(i) == "right") 
						line->push_back(values[1]);
					else if(position.at(i) == "all") {
						line->push_back(values[0]);
						line->push_back(values[1]);
					}
					free(values);
					values = this->_get(GET_LINE_ALL, 2);
				}
				free(values);
				retval = (void*)line;
			}
			else if( sensor == "obstacle" ) {
				int obstacle = this->getObstacle(position.at(0));
				int * ptr_obstacle = new int[1];
				ptr_obstacle[0] = obstacle;
				retval = (void*)ptr_obstacle;
			}
			else if( sensor == "bright" ) {
				int bright = this->getBright(position.at(0));
				int * ptr_bright = new int[1];
				ptr_bright[0] = bright;
				retval = (void*)ptr_bright;
			}
			else if( sensor == "picture" ) {
				retval = (void*)this->takePicture(position.at(0));
			}
			else 
				std::cout << "invalid sensor name: " << sensor << std::endl;
		}
	}
	return retval;
}

void * Scribbler::get(std::string item, std::vector<int> position) {
	void * retval = NULL;

	std::string sensor;
	for(int i = 0; i < (int)item.length(); i++)
		sensor += tolower(item.at(i));

	if(sensor == "data") {
		std::vector<int> * data 
			= new std::vector<int>(this->getData(position));
		retval = (void*)data;
	}
	else if(sensor == "light") {
		std::vector<int> * light = new std::vector<int>();
		for(int i = 0; i < (int)position.size(); i++) {
			light->push_back(this->getLight(position.at(i)));
		}
		retval = (void*)light;
	}
	else if(sensor == "ir") {
		std::vector<int> * ir = new std::vector<int>();
		int * irVal = this->_get(GET_IR_ALL, 2);
		for(int i = 0; i < (int)position.size(); i++) {
			switch(position.at(i)) {
				case 0: ir->push_back(irVal[0]);
					break;
				case 1: ir->push_back(irVal[1]);
					break;
			}
			free(irVal);
			irVal = this->_get(GET_IR_ALL, 2);
		}
		free(irVal);
		retval = (void*)ir;
	}
	else if(sensor == "line") {
		std::vector<int> * line = new std::vector<int>();
		int * lineVal = this->_get(GET_IR_ALL, 2);
		for(int i = 0; i < (int)position.size(); i++) {
			switch(position.at(i)) {
				case 0: line->push_back(lineVal[0]);
					break;
				case 1: line->push_back(lineVal[1]);
					break;
			}
			free(lineVal);
			lineVal = this->_get(GET_IR_ALL, 2);
		}
		free(lineVal);
		retval = (void*)line;
	}
	else if(sensor == "obstacle") {
		std::vector<int> * obstacle = new std::vector<int>();
		for(int i = 0; i < (int)position.size(); i++) {
			obstacle->push_back(this->getObstacle(position.at(i)));
		}
		retval = (void*)obstacle;
	}
	else
		std::cout << "Invalid sensor name: " << sensor << std::endl;
	
	return retval;
}

std::vector<int> Scribbler::getLastSensors() {
	pthread_mutex_lock(this->robot_lock);
	std::vector<int> result;

	for(int i = 2; i < 7; i+=2) {
		result.push_back( sensors[i] << 8 | sensors[i+1] );
	}
	result.push_back( sensors[0] );
	result.push_back( sensors[1] );
	result.push_back( sensors[8] );
	result.push_back( sensors[9] );
	result.push_back( sensors[10] );
	pthread_mutex_unlock(this->robot_lock);
	return result;
}

void Scribbler::setSingleData(int position, int value) {
	this->_set(SET_SINGLE_DATA, position, (unsigned char)value, -1);
}

void Scribbler::setData(int position, int value) {
	int * data = this->_get(GET_DATA, 8);
	data[position] = value;
	this->_set(SET_DATA, (unsigned char)data[0], 
			(unsigned char)data[1], (unsigned char)data[2], 
			(unsigned char)data[3], (unsigned char)data[4],
			(unsigned char)data[5], (unsigned char)data[6], 
			(unsigned char)data[7], (unsigned char)data[8]);
	free(data);
}

void Scribbler::setEchoMode(int value) {
	if(value) 
		this->_set(SET_ECHO_MODE, 1, -1);
	else
		this->_set(SET_ECHO_MODE, 0, -1);
	usleep(25000);
	con->flush_input();
	con->flush_output();
}

void Scribbler::setName(std::string name) {
	unsigned char * name_buffer 
		= (unsigned char*)malloc(sizeof(unsigned char) * (16 + 2));
	name_buffer[0] = SET_NAME1;
	name_buffer[9] = SET_NAME2;

	for(int j = 0; j < 2; j++) {
		for(int i = j * 9 + 1; i < 9 * (j+1); i++) {
			if(i-1 < (int)name.length()) 
				name_buffer[i] = name.at(i-1);
			else name_buffer[i] = ' ';
		}
	}
	pthread_mutex_lock(this->robot_lock);
	send_message(name_buffer, 18);
	pthread_mutex_unlock(this->robot_lock);
	free(name_buffer);
}

void Scribbler::setPassword(std::string pass) {
	unsigned char * pass_buffer
		= (unsigned char *)malloc(sizeof(unsigned char) * (16 + 2));
	pass_buffer[0] = SET_PASS1;
	pass_buffer[9] = SET_PASS2;

	for(int j = 0; j < 2; j++) {
		for(int i = j * 9 + 1; i < 9 * (j+1); i++) {
			if(i - 1 < (int)pass.length())
				pass_buffer[i] = pass.at(i-1);
			else pass_buffer[i] = ' ';
		}
	}
	pthread_mutex_lock(this->robot_lock);
	send_message(pass_buffer, 18);
	pthread_mutex_unlock(this->robot_lock);
	free(pass_buffer);
}

void Scribbler::setLEDFront(int value) {
	value = std::min(std::max(value, 0), 1);
	unsigned char buffer;
	if(value) 
		buffer = SET_DONGLE_LED_ON;
	else
		buffer = SET_DONGLE_LED_OFF;
	pthread_mutex_lock(this->robot_lock);
	con->_write(&buffer, 1);
	pthread_mutex_unlock(this->robot_lock);
}

void Scribbler::setLEDBack(double value) {
	if(value > 1)
		value = 1;
	else if (value <= 0) {
		value = 0;
	}
	else
		value = (value * (255 - 170)) + 170;

	unsigned char buffer[2];
	buffer[0] = SET_DIMMER_LED;
	buffer[1] = (unsigned char)value;
	pthread_mutex_lock(this->robot_lock);
	con->_write(buffer, 2);
	pthread_mutex_unlock(this->robot_lock);
}

void Scribbler::setForwardness(std::string direction) {
	int iDirection;
	if(direction == "fluke-forward")
		iDirection = 1;
	else if(direction == "scribbler-forward") 
		iDirection = 0;
	else {
		std::cout << "Unknown Direction: " << direction << std::endl;
		return;
	}
	unsigned char buffer[2];
	buffer[0] = SET_FORWARDNESS;
	buffer[1] = (unsigned char)iDirection;
	pthread_mutex_lock(this->robot_lock);
	con->_write(buffer, 2);
	pthread_mutex_unlock(this->robot_lock);
}

void Scribbler::setForwardness(int direction) {
	switch(direction)  {
		case 1:
			break;
		case 0:
			break;
		default:
			std::cout << "Unknown Direction: " << direction << std::endl;
			return;
	}
	unsigned char buffer[2];
	buffer[0] = SET_FORWARDNESS;
	buffer[1] = (unsigned char)direction;
	pthread_mutex_lock(this->robot_lock);
	con->_write(buffer, 2);
	pthread_mutex_unlock(this->robot_lock);
}

void Scribbler::setIRPower(int power) {
	unsigned char buffer[2];
	buffer[0] = SET_DONGLE_IR;
	buffer[1] = (unsigned char)power;
	pthread_mutex_lock(this->robot_lock);
	con->_write(buffer, 2);
	pthread_mutex_unlock(this->robot_lock);
}

void Scribbler::setWhiteBalance(int value) {
	unsigned char buffer;
	if(value)
		buffer = SET_WHITE_BALANCE;
	else
		buffer = SET_NO_WHITE_BALANCE;
	pthread_mutex_lock(this->robot_lock);
	con->_write(&buffer, 1);
	pthread_mutex_unlock(this->robot_lock);
}

void Scribbler::conf_rle_range(unsigned char * picture, int x1, 
		int y1, int x2, int y2) {


	int width = 256;
	//int height = 192;

	int minY=255, minV=255, minU=255;
	int maxY=0, maxV=0, maxU=0;

	int R, G, B;

	double totalY = 0;
	double totalU = 0;
	double totalV = 0;

	double meanY, sY = 0;
	double meanU, sU = 0;
	double meanV, sV = 0;

	double area = abs(x1-x2) * abs(y1-y2);
	int * y_array = (int*)malloc(sizeof(int) * area); 
	int * u_array = (int*)malloc(sizeof(int) * area);
	int * v_array = (int*)malloc(sizeof(int) * area);

	memset(y_array, 0, area);
	memset(u_array, 0, area);
	memset(v_array, 0, area);

	int index = 0;

	for(int i = y1; i < y2; i++) {
		for(int j = x1; j < x2; j++, index++) {
			R = picture[ (i * width) + j ];
			G = picture[ (i * width) + j + 1];
			B = picture[ (i * width) + j + 2];

			rgb2yuv(R, G, B, y_array[index], u_array[index], v_array[index]);
			
			totalY += y_array[index];
			totalU += u_array[index];
			totalV += v_array[index];
		}
	}

	meanY = totalY/area;
	meanU = totalU/area;
	meanV = totalV/area;

	for(int i = 0; i < area; i++) {
		sY += pow(y_array[i] - meanY,2);
		sU += pow(u_array[i] - meanU,2);
		sV += pow(v_array[i] - meanV,2);
	}

	sY = sqrt( sY/area );
	sU = sqrt( sU/area );
	sV = sqrt( sV/area );

	minY = abs(meanY - sY * 1.5); maxY = abs(meanY + sY * 1.5);
	minU = abs(meanU - sU * 1.5); maxU = abs(meanU + sU * 1.5);
	minV = abs(meanV - sV * 1.5); maxV = abs(meanV + sV * 1.5);

	minY = std::max(minY, 0); maxY = std::min(maxY, 255);
	minU = std::max(minU, 0); maxU = std::min(maxU, 255);
	minV = std::max(minV, 0); maxV = std::min(maxU, 255);

	printf("Min Y: %i, Max Y: %i\n", minY, maxY);
	printf("Min U: %i, Max U: %i\n", minU, maxU);
	printf("Min V: %i, Max V: %i\n", minV, maxV);

	this->configureBlob(0, 254, minU, maxU, minV, maxV, 4);
	free(y_array);
	free(u_array);
	free(v_array);
}

void Scribbler::configureBlob(int y_low, int y_high, int u_low, int u_high,
		int v_low, int v_high, int smooth_thresh) {
	this->conf_rle(90, smooth_thresh, y_low, y_high, 
					u_low, u_high, v_low, v_high);
}

double Scribbler::getBattery() {
	double battery = 0;
	unsigned char command = GET_BATTERY;
	pthread_mutex_lock(this->robot_lock);
	con->_write(&command,1);
	battery = read_2byte() / 20.9813;
	pthread_mutex_unlock(this->robot_lock);
	return battery;
}

int Scribbler::getObstacle(std::string value) {
	unsigned char obst;
	if( value == "none"){
		//to be determined!
	}
	else if (value == "left") {
		obst = GET_DONGLE_L_IR;
	}
	else if (value == "right") {
		obst = GET_DONGLE_R_IR;
	}
	else if ( value == "center" || value == "middle") {
		obst = GET_DONGLE_C_IR;
	}
	pthread_mutex_lock(this->robot_lock);
	con->_write(&obst, 1);
	int result = read_2byte();
	pthread_mutex_unlock(this->robot_lock);
	return result;
}

int Scribbler::getObstacle(int value) {
	unsigned char obst;
	if(value == -1) {
		//to be determined
	}
	if(value == 0) {
		obst = GET_DONGLE_L_IR;
	}
	else if(value == 1) {
		obst = GET_DONGLE_C_IR;
	}
	else if(value == 2) {
		obst = GET_DONGLE_R_IR;
	}
	pthread_mutex_lock(this->robot_lock);
	con->_write(&obst, 1);
	int result = read_2byte();
	pthread_mutex_unlock(this->robot_lock);
	return result;
}

int Scribbler::getBright(std::string value) {
	unsigned char buffer[2];
	unsigned char window;
	int retval;
	if(value == "") {
		//to be determined
	}
	else if(value == "left")
		window = 0;
	else if( value == "middle" || value == "center")
		window = 1;
	else if( value == "right" )
		window = 2;
	else {
		std::cout << "Invalid Window: " << value << std::endl;
		return -1;
	}
	buffer[0] = GET_WINDOW_LIGHT;
	buffer[1] = window;
	pthread_mutex_lock(this->robot_lock);
	con->_write(buffer, 2);
	retval = read_3byte();
	pthread_mutex_unlock(this->robot_lock);
	return retval;
}

int Scribbler::getLight(std::string value) {
	int * result;
	int retval;
	if(value == "none") {
	}
	else if(value == "right") {
		result = _get(GET_LIGHT_RIGHT, 2, "word");
		retval = result[0];
		free(result);
	}
	else if(value == "left") {
		result = _get(GET_LIGHT_LEFT, 2, "word");
		retval = result[0];
		free(result);
	}
	else if(value == "center" || value == "middle") {
		result = _get(GET_LIGHT_CENTER, 2, "word");
		retval = result[0];
		free(result);
	}
	return retval;
}

int Scribbler::getLight(int value) {
	int * result;
	int retval = 0;

	switch( value ) {
		case -1: return retval;
		case 0:  result = _get(GET_LIGHT_LEFT, 2, "word");
				 break;
		case 1:  result = _get(GET_LIGHT_CENTER, 2, "word");
				 break;
		case 2:  result = _get(GET_LIGHT_RIGHT, 2, "word");
				 break;
	}
	retval = result[0];
	return retval;
}

std::vector<int> Scribbler::getData(std::vector<int> position) {
	std::vector<int> dataV;
	int * data;
	data = this->_get(GET_DATA, 8);

	if(!position.size()) {
		for(int i = 0; i < 8; i++)
			dataV.push_back(data[i]);
	}
	else {
		for(int i = 0; i < (int)position.size(); i++)
			dataV.push_back(data[position.at(i)]);
	}
	free(data);
	return dataV;
}

Picture * Scribbler::takePicture(std::string type) {
	unsigned char * imageBuffer = NULL;
	int size;

	Picture * image;

	pthread_mutex_lock(this->image_lock);
	if(type == "color") {
		imageBuffer = grab_array_rgb();
		image = new ColorPicture(imageBuffer, 256, 192);
	}
	else if( type == "gray" || type == "grey") {
		conf_window(0, 1, 0, 255, 191, 2, 2);
		imageBuffer = grab_gray_array();
		conf_gray_window(0, 2, 0, 128, 191, 1, 1);
		image = new GrayPicture(imageBuffer, 256, 192);
	}
	else if( type == "blob" ) {
		imageBuffer = grab_blob_array();
		image = new ColorPicture(imageBuffer, 256, 192);
	}
	else if( type == "jpeg" ) {
		imageBuffer = grab_jpeg_color(1,size);
		image = new ColorPicture(imageBuffer, 256, 192);
	}
	else if( type == "jpeg-fast") {
		imageBuffer = grab_jpeg_color(0,size);
		image = new ColorPicture(imageBuffer, 256, 192);
	}
	else if( type == "grayjpeg") {
		imageBuffer = grab_jpeg_gray(1,size);
		image = new ColorPicture(imageBuffer, 256, 192);
	}
	else if( type == "grayjpeg-fast" ) {
		imageBuffer = grab_jpeg_gray(0,size);
		image = new ColorPicture(imageBuffer, 256, 192);
	}
	pthread_mutex_unlock(this->image_lock);
	return image;
}

/*unsigned char * Scribbler::takePicture(std::string type, int &size) {
	unsigned char * imageBuffer = NULL;
	if(type == "jpeg") {
		imageBuffer = grab_jpeg_color(1, size);
	}
	else if(type == "jpeg-fast") {
		imageBuffer = grab_jpeg_color(0, size);
	}
	else if(type == "grayjpeg") {
		imageBuffer = grab_jpeg_gray(1, size);
	}
	else if(type == "grayjpeg-fast") {
		imageBuffer = grab_jpeg_gray(0, size);
	}
	return imageBuffer;
}*/

std::vector<int> Scribbler::getBlob() {
	std::vector<int> blob(3,0);

	int numpixs;
	int xloc;
	int yloc;
	unsigned char buffer[2];

	pthread_mutex_lock(this->robot_lock);

	unsigned char command = GET_BLOB;
	con->_write(&command, 1);
	numpixs = read_2byte();
	con->_read(buffer, 2);
	xloc = buffer[0];
	yloc = buffer[1];

	pthread_mutex_unlock(this->robot_lock);
	blob[0] = numpixs;
	blob[1] = xloc;
	blob[2] = yloc;
	return blob;
}

void Scribbler::darkenCamera(int level) {
	this->set_cam_param(CAM_COMA, CAM_COMA_WHITE_BALANCE_OFF);
	this->set_cam_param(CAM_COMB, 
			(CAM_COMB_GAIN_CONTROL_OFF & CAM_COMB_EXPOSURE_CONTROL_OFF));
	this->set_cam_param((unsigned char)0, (unsigned char)level);
	this->set_cam_param((unsigned char)1, (unsigned char)0);
	this->set_cam_param((unsigned char)2, (unsigned char)0);
	this->set_cam_param((unsigned char)6, (unsigned char)0);
	this->set_cam_param((unsigned char)0x10, (unsigned char)0);
}

void Scribbler::autoCamera() {
	this->set_cam_param((unsigned char)0, (unsigned char)0);
	this->set_cam_param((unsigned char)1, (unsigned char)0x80);
	this->set_cam_param((unsigned char)2, (unsigned char)0x80);
	this->set_cam_param((unsigned char)6, (unsigned char)0x80);
	this->set_cam_param((unsigned char)0x10, (unsigned char)0x41);
	this->set_cam_param(CAM_COMA, CAM_COMA_DEFAULT);
	this->set_cam_param(CAM_COMB, CAM_COMB_DEFAULT);
}

std::string Scribbler::getName() {
	std::string name = "";

	int * retval = this->_get(GET_NAME1, 8);
	for(int i = 0; i < 8; i++) 
		name += (char)retval[i];
	free(retval);
	retval = this->_get(GET_NAME2, 8);
	for(int i = 0; i < 8; i++)
		name += (char)retval[i];
	free(retval);

	return name;
}

std::string Scribbler::getPassword() {
	std::string pass = "";
	int * retval = this->_get(GET_PASS1, 8);
	for(int i = 0; i < 8; i++)
		pass += (char)retval[i];
	free(retval);
	retval = this->_get(GET_PASS2, 8);
	for(int i = 0; i < 8; i++)
		pass += (char)retval[i];
	return pass;
}

std::vector<std::string> Scribbler::getInfo() {

	std::vector<std::string> result;

	int * retval = this->_get(GET_INFO, 1, "line");
	int size = 0;
	for(; retval[size]; size++);

	char * infoCString1 = (char*)malloc(sizeof(char) * (size + 1));
	for(int i = 0; i < size; i++)
		infoCString1[i] = (char)retval[i];
	infoCString1[size] = 0;

	//printf("Info %s\n", infoCString1);

	char * comma_pt = strchr(infoCString1, ',');
	char * info_pt = infoCString1;

	std::string temp_pair = "";

	while( comma_pt ) {
		for(int i = 0; info_pt + i < comma_pt; i++) {
			temp_pair += info_pt[i];
		}
		result.push_back(temp_pair);
		temp_pair = "";
		info_pt =  comma_pt + 1;
		comma_pt = strchr(comma_pt + 1, ',');
	}
	for(int i = 0; info_pt + i < strlen(infoCString1)-1 + infoCString1; i++)
		temp_pair += info_pt[i];
	result.push_back(temp_pair);

	return result;
}


int Scribbler::send_message(unsigned char * buf, int bytes, int _echo, 
		int _sensor) {

	int packets = bytes/PACKET_SIZE;
	int status = 0;
	if(bytes%PACKET_SIZE)
		packets++;

	//for(int j = 0; j < bytes; j++) 
	//	printf("Unpadded Message %i: Value %i\n", j, buf[j]);

	unsigned char * padded_buf 
		= (unsigned char *)malloc(sizeof(unsigned char) 
				* packets * PACKET_SIZE);
	memcpy(padded_buf, buf, bytes);
	for(int i = bytes; i < packets * PACKET_SIZE; i++)
		padded_buf[i] = '0';

	unsigned char * echo 
		= (unsigned char *)malloc(sizeof(unsigned char) * PACKET_SIZE);

	for(int i = 0; i < packets; i++) {
		/*for(int j = 0; j < 9; j++) {
			printf("Message %i: Value %i\n", 
					j, *((padded_buf + i * PACKET_SIZE) + j));
		}*/
		status += con->_write(padded_buf + i * PACKET_SIZE, PACKET_SIZE);
		if(_echo) {
			read_message(echo, PACKET_SIZE);
			/*for(int j = 0; j < 9; j++) {
				printf("Echo %i: value %i\n", j, echo[j]);
			}
			printf("Echo LP: %i Echo RP: %i\n", echo[1], echo[2]);*/
		}
		if(_sensor)
			read_message(sensors, 11);
	}
	return status;
}

int Scribbler::read_message(unsigned char * buf, int bytes) {
	return con->_read(buf, bytes);
}

int Scribbler::read_2byte() {
	int ret = 0;
	unsigned char buf[2];
	con->_read(buf, 2);
	ret = ((int)(buf[0])) << 8 | ((int)buf[1]);
	return ret;
}

int Scribbler::read_3byte() {
	int ret = 0;
	unsigned char buf[3];
	con->_read(buf, 3);
	ret = ((int)buf[0]) << 16 | ((int)buf[1]) << 8 | ((int)buf[0]);
	return ret;
}

int Scribbler::write_mem(int page, int offset, int byte) {
	return 0;
}

int Scribbler::write_2byte(int value) {
	unsigned char command_buffer[2];
	command_buffer[0] = (unsigned char)((value >> 8) & 0xFF);
	command_buffer[1] = (unsigned char)(value & 0xFF);
	return con->_write(command_buffer, 2);
}

int Scribbler::read_mem(int page, int offset) {
	unsigned char command = GET_SERIAL_MEM;
	unsigned char value;
	con->_write(&command, 1);
	write_2byte(page);
	write_2byte(offset);
	con->_read(&value, 1);
	return (int)value;
}

int Scribbler::erase_mem(int page) {
	return 0;
}

int Scribbler::rgb2yuv(int R, int G, int B, int &Y,
											int &U,
											int &V) {
	double tY = 0.299 * R + 0.587 * G + 0.114 * B;
	double tU = -0.14713 * R - 0.28886 * G + 0.436 * B + 128;
	double tV = 0.615 * R - 0.51499 * G - 0.10001 * B + 128;

	tY = std::max(std::min(tY, 255.0), 0.0);
	tU = std::max(std::min(tU, 255.0), 0.0);
	tV = std::max(std::min(tV, 255.0), 0.0);

	Y = (int)tY;
	U = (int)tU;
	V = (int)tV;
	return 0;
}
										  

int Scribbler::yuv2rgb(int Y, int U, int V, unsigned char &R, 
											unsigned char &G, 
											unsigned char &B) {
	int tR = (int)(Y + (1.13983 * (V - 128)));
	int tG = (int)(Y - (0.39466 * (U - 128)) - (0.58060 * (V - 128)));
	int tB = (int)(Y + (2.03211 * (U - 128)));

	R = std::max(std::min(tR, 255),0);
	G = std::max(std::min(tG, 255),0);
	B = std::max(std::min(tB, 255),0);
	return 0;
}
