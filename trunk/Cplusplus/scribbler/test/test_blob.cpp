#include "Scribbler.h"
#include "VideoStream.h"
#include "Filter.h"
#include <list>
#include <sstream>
#include <math.h>
#include <stdio.h>
#include <iostream>
#include <Magick++.h>
using namespace Magick;
using namespace std;

class PictureInPicture: public Filter {

	public:

	PictureInPicture(Scribbler * robot, int width, int height, int color)
		: Filter(height, width, color) {
			this->robot = robot;
	}

	~PictureInPicture(){}

	void filter(unsigned char * image) {
		unsigned char * colorImage = robot->takePicture("jpeg")->getRawImage();

		int height = getHeight();
		int width = getWidth();
		int reduction = 4;

		//May not need this allocation
		unsigned char * newImage 
			= (unsigned char*)malloc(sizeof(unsigned char) 
					* height/reduction * width/reduction * 3);

		for(int h = 0; h < height/reduction; h++) {
			for(int w = 0; w < width/reduction; w++) {
				for(int rgb = 0; rgb < 3; rgb++) {
					newImage[(h * width/reduction * 3) + (w * 3) + rgb]
						= colorImage[(reduction * h * width * 3) 
						+ (reduction * w * 3) + rgb];
				}
			}
		}

		for(int h = 0; h < height/reduction; h++) {
			for(int w = width - width/reduction, 
					w2 = 0; w < width; w++, w2++) {
				for(int rgb = 0; rgb < 3; rgb++) {
					image[(h * width * 3) + (w * 3) + rgb]
						= newImage[(h * width/reduction * 3) + (w2 * 3) + rgb];
				}
			}
		}
		free(colorImage);
		free(newImage);	
	}

	private:

	Scribbler * robot;
};

class Record : public Filter {
	public:

	Record(string location, string fileName, 
			int height, int width, int color_mode) 
		: Filter(height, width, color_mode) {
		this->location = location;
		this->fileName = fileName;
		imageList = new list<Image>();
	}

	~Record(){
		string full_file = location + fileName;
		writeImages( imageList->begin(), imageList->end(), full_file );
		imageList->clear();
		delete imageList;
	}

	void filter(unsigned char * image) {
		Image robotImage(256,192, "RGB", CharPixel,image); 
		imageList->push_back(robotImage);
	}

	private:

	string location;
	string fileName;

	list<Image> * imageList;

};

int main(int argc, char ** argv) {

	int enablePnp = 0;
	int enableRecord = 0;

	int check_index = 1;
	int path_index = -1;
	int file_name_index = -1;

	if(argc > 1) {
		if(!strcmp(argv[1], "record")) { 
			enableRecord = 1;
			check_index += 3;
			path_index = 2;
			file_name_index = 3;
		}
		else if(!strcmp(argv[1], "pnp")) {
			enablePnp = 1;
			check_index++;
		}
		else if(strcmp(argv[1], "none"))
			fprintf(stderr, "Invalid Filter Opition\n");
		if(argc > check_index) {
			if(!strcmp(argv[check_index], "record")) {
				enableRecord = 1;
				check_index += 3;
				path_index = check_index - 2;
				file_name_index = check_index - 1;
			}
			else if(!strcmp(argv[check_index], "pnp")) {
				enablePnp = 1;
				check_index++;
			}
			else if(strcmp(argv[check_index], "none"))
				fprintf(stderr, "Invalid Filter Opition\n");
		}
	}
	else if(argc < 2) {
		fprintf(stderr, "Usuage: ./test_blob pnp|record path filename|none\n");
		return -1;
	}
	
	if(argc < check_index) {
		fprintf(stderr, 
				"Usuage: ./test_blob pnp|record path filename|none\n");
		return -1;
	}

	Scribbler * robot = new Scribbler();

	int status = 0;
	status = robot->connect();
	if(status < 0) {
		return -1;
	}

	cout << "Connected to Robot\n";
	cout << "Proceeding to Train Blob\n";

	int mid_w = 256/2;
	int mid_h = 192/2;

	int x1 = mid_w - 10;
	int y1 = mid_h - 10;
	int x2 = mid_w + 10;
	int y2 = mid_h + 10;

	int train = 0;
	string toTrain;

	unsigned char * tempImageBuffer 
		= (unsigned char*)malloc(sizeof(unsigned char) * 256 * 192 * 3);
	unsigned char * rgb_image_buffer = robot->takePicture()->getRawImage();
	while(!train) {
		memcpy(tempImageBuffer, rgb_image_buffer, sizeof(unsigned char) *
				256 * 192 * 3);
		for(int h = y1; h < y2; h++) 
			for(int w = x1; w < x2; w++) {
					tempImageBuffer[(h * 256 * 3) + (w * 3)]
						= 255;
					for(int rgb = 1; rgb < 3; rgb++)
						tempImageBuffer[(h * 256 * 3) + (w * 3) + rgb]
							= 0;
			}
		Image tempImage(256,192,"RGB", CharPixel, tempImageBuffer);
		cout << "Used this image to train?" << endl;
		if(fork() == 0) {
			tempImage.display();
			exit(0);
		}
		cin >> toTrain;
		if(toTrain == "y" || toTrain == "yes")
			train = 1;
		else {
			free(rgb_image_buffer);
			rgb_image_buffer = robot->takePicture()->getRawImage();
		}
	}
	robot->conf_rle_range(rgb_image_buffer, x1, y1, x2, y2);

	cout << "Proceeding to Test Blob Tracking\n";

	PictureInPicture * pnp;
	Record * record;
	VideoStream foo(robot, 2);

	if(enablePnp) {	
		pnp = new PictureInPicture(robot,256,192,1);
		foo.addFilter(pnp);
	}
	if(enableRecord) {	
		record = new Record(argv[path_index], argv[file_name_index], 
				256,192,1);
		foo.addFilter(record);
	}
	foo.startStream();

	int close = 0;
	string done;
	while(!close) {
		cout << "Enter E or Exit to quit\n";
		cin >> done;
		if(done == "E" || done == "Exit" || done == "e" || done == "exit")
			close = 1;
	}

	foo.endStream();
	if(enablePnp) 
		delete pnp;
	if(enableRecord)
		delete record;
	status = robot->disconnect();

	return 0;
}
