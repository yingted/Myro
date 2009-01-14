#include "ColorPicture.h"
#include <Magick++.h>
using namespace Magick;

ColorPicture::ColorPicture(unsigned char * data, int width, int height)
	: Picture(width, height) {

	image_data = new unsigned char[width * height * 3];
	for(int i = 0; i < width * height * 3; i++)
		image_data[i] = data[i];

	this->width = width;
	this->height = height;
}

ColorPicture::ColorPicture(int width, int height): 
	Picture(width, height) {

	this->width = width;
	this->height = height;

	image_data = new unsigned char[width * height * 3];
	for(int i = 0; i < width * height * 3; i++) {
		image_data[i] = 0;
	}
}

ColorPicture::ColorPicture(ColorPicture& pic):
	Picture(pic.width, pic.height) {	
	image_data = new unsigned char[pic.width * pic.height * 3];
	for(int i = 0; i < pic.width * pic.height * 3; i++)
		image_data[i] = pic.image_data[i];
}

Pixel ColorPicture::getPixel(int x, int y) {
	Pixel result;
	result.R = image_data[(y * width * 3) + (x * 3)];
	result.G = image_data[(y * width * 3) + (x * 3)+1];
	result.B = image_data[(y * width * 3) + (x * 3)+2];
	return result;
}

void ColorPicture::setPixel(int x, int y, Pixel pix) {
	image_data[(y * width * 3) + (x * 3)] = pix.R;	
	image_data[(y * width * 3) + (x * 3)+1] = pix.G;	
	image_data[(y * width * 3) + (x * 3)+2] = pix.B;	
}

void ColorPicture::show() {
	Image tempImage(width, height, "RGB", CharPixel, image_data);
	tempImage.display();
}

unsigned char * ColorPicture::getRawImage() {
	return image_data;
}
