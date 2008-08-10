#include "VideoStream.h"
#include "Scribbler.h"
#include <string>
#include <iostream>
#include <math.h>
#include <Magick++.h>
using namespace Magick;

class InvertFilter: public Filter {

	public:

	InvertFilter(int width, int height, int color)
		: Filter(height, width, color){
		}
	~InvertFilter(){}

	void filter(unsigned char * image) {
		int height = getHeight();
		int width = getWidth();
		int colorMode = getColorMode();

		unsigned char temp;
		unsigned char tempRGB[3] = {0,0,0};


		if(colorMode == 0) {
			for(int i = 0; i < height * width/2; i++) {
				temp = image[i];
				image[i] = image[((height * width)-i-1)];
				image[((height * width)-i-1)] = temp;
			}
		}
		else {
			for(int h = 0; h < height/2; h++) {
				for(int w = 0; w < width; w++) {
					for(int rgb = 0; rgb < 3; rgb++) {
						tempRGB[rgb] 
							= image[(h * width * 3) + (w * 3) + rgb];
					}
					for(int rgb = 0; rgb < 3; rgb++) {
						image[(h * width * 3) + (w * 3) + rgb] = 
						image[((height - h - 1) 
								* width * 3) + (w * 3) + rgb];
					}
					for(int rgb = 0; rgb < 3; rgb++) {
						image[((height - h - 1)
								* width * 3) + (w * 3) + rgb]
							= tempRGB[rgb];
					}
				}
			}
		}
	}

	private:
};

class LightMask: public Filter {
	
	public:

	LightMask(int width, int height, int color)
		: Filter(height, width, color) {
		}
	~LightMask(){}

	void filter(unsigned char * image) {

		int color_mode = getColorMode();
		if(color_mode)
			return;

		int numPartions = 4;
		int imagePartion = getWidth()/numPartions;

		int imageHeight =  getHeight();
		int imageWidth = getWidth();

		int shadePartion;
		double brightEst = 0;
		int partion[6] = {0,0,0,0,0,0};

		for(int i = 0; i < imageHeight; i++) {
			for(int j = 0; j < numPartions; j++) {
				for(int k = 0; k < imagePartion; k++) {
					partion[j] += image[(i * imageWidth) 
						+ (j * imagePartion) + k];
				}
			}
		}

		shadePartion = 0;
		brightEst = partion[0]/(imageHeight * imagePartion);
		for(int i = 1; i < numPartions; i++) {
			if( partion[i]/(imageHeight * imagePartion) > brightEst ) {
				shadePartion = i;
				brightEst = partion[i]/(imageHeight * imagePartion);
			}
		}

		for(int i = 0; i < imageHeight; i++) {
			for(int j = 0; j < numPartions; j++) {
				if(j != shadePartion) {
					for(int k = 0; k < imagePartion; k++) {
						image[(i * imageWidth) + (j * imagePartion) + k] = 0;
					}
				}
			}
		}
		
	}

	private:
};

int main(int argc, char ** argv) {
	Scribbler * foo = new Scribbler();

	int color_mode = -1;
	int enableInvert = 0;
	int enableLightMask = 0;

	if(argc < 2) {
		fprintf(stderr, 
				"Usuage: ./video_stream color|gray invert|light|none\n");
		return -1;
	}
	else if(!strcmp(argv[1], "color")) {
		color_mode = 1;
	}
	else if(!strcmp(argv[1], "gray")) {
		color_mode = 0;
	}
	else {
		fprintf(stderr, 
				"Usuage: ./video_stream color|gray invert|light|none\n");
		return -1;
	}

	if(argc < 5 && argc > 2) {
		if(!strcmp(argv[2], "invert"))
			enableInvert = 1;
		else if(!strcmp(argv[2], "light"))
			enableLightMask = 1;
		else if(strcmp(argv[2], "none"))
			fprintf(stderr, "Invalid Filter Opition\n");

		if(argc > 3) {
			if(!strcmp(argv[3], "invert"))
				enableInvert = 1;
			else if(!strcmp(argv[3], "light"))
				enableLightMask = 1;
			else if(strcmp(argv[3], "none"))
				fprintf(stderr, "Invalid Filter Opition\n");
		}
	}

	if(foo->connect() < 0) {
		return -1;
	}

	InvertFilter * myInvertFilter;
	LightMask * myLightMask;
	VideoStream video(foo, color_mode);

	if(enableInvert) {
	    myInvertFilter = new InvertFilter(256, 192, color_mode);
		video.addFilter(myInvertFilter);
	}
	if(enableLightMask) {
		myLightMask = new LightMask(256, 192, color_mode);
		video.addFilter(myLightMask);
	}

	video.startStream();

	int close = 0;
	std::string quit;
	while(!close) {
		std::cout << "Type in E or Exit to quit\n";
		std::cin >> quit;
		if(quit == "E" || quit == "Exit" || quit == "e" || quit == "exit")
			close = 1;
	}
	video.endStream();
	foo->disconnect();
	if(enableInvert)
		delete myInvertFilter;
	if(enableLightMask)
		delete myLightMask;
	return 1;
}

