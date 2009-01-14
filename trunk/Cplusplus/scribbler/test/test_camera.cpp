#include "Scribbler.h"
#include <math.h>
#include <stdio.h>
#include <iostream>
#include <Magick++.h>
using namespace Magick;
using namespace std;

int main(int argc, char ** argv) {

	Scribbler * robot = new Scribbler();

	int status = 0;
	status = robot->connect();
	if(status < 0) {
		return -1;
	}

	cout << "Connected to Robot\n";
	cout << "Proceeding to test camera\n";

	Image robotImage;
	unsigned char * image_buffer;

	cout << "Testing Color, if image looks correct close display\n";
	cout << "If not, try restarting robot, and rerunning test\n";

	image_buffer = robot->takePicture("color")->getRawImage();
	robotImage = Image(256,192,"RGB",CharPixel,image_buffer);
	robotImage.display();
	free(image_buffer);

	cout << "Testing Grayscale(Note Image should look Pink)\n";
	image_buffer = robot->takePicture("gray")->getRawImage();
	robotImage = Image(256,192,"G", CharPixel, image_buffer);
	robotImage.display();
	free(image_buffer);

	cout << "Testing Jpeg Color\n";
	image_buffer = robot->takePicture("jpeg")->getRawImage();
	robotImage = Image(256,192,"RGB", CharPixel, image_buffer);
	robotImage.display();
	free(image_buffer);

	cout << "Testing Jpeg Gray\n";
	image_buffer = robot->takePicture("grayjpeg")->getRawImage();
	robotImage = Image(256,192,"G", CharPixel, image_buffer);
	robotImage.display();
	free(image_buffer);


	cout << "All Camera Test Completed\n";
	status = robot->disconnect();

	return 0;
}

