#include "GrayPicture.h"
#include <Magick++.h>
using namespace Magick;

GrayPicture::GrayPicture(int width, int height) 
	:Picture(width, height) {
	
	image_data = new unsigned char[width * height];
	for(int i = 0; i < width * height; i++)
		image_data = 0;

	this->width = width;
	this->height = height;
}

GrayPicture::~GrayPicture() {
	delete image_data;
}

GrayPicture::GrayPicture(unsigned char * data, int width, int height)
	:Picture(width, height) {
	
	image_data = new unsigned char[width * height];
	for(int i = 0; i < width * height; i++)
		image_data[i] = data[i];

	this->width = width;
	this->height = height;
}

GrayPicture::GrayPicture(GrayPicture& pic)
	:Picture(pic.width, pic.height) {
	
	this->width = pic.width;
	this->height = pic.height;

	image_data = new unsigned char[pic.width * pic.height];

	for(int i = 0; i < pic.width * pic.height; i++)
		image_data[i] = pic.image_data[i];
}

Pixel GrayPicture::getPixel(int x, int y) {
	Pixel result;
	result.R = image_data[(y * width) + x];
	result.G = result.R;
	result.B = result.R;
	return result;
}

void GrayPicture::setPixel(int x, int y, Pixel pix) {
	image_data[(y * width) + x] = pix.R;
}

unsigned char * GrayPicture::getRawImage() {
	return image_data;
}

void GrayPicture::show() {
	unsigned char image_data_color[width * height * 3];
	for(int h = 0; h < height; h++)
		for(int w = 0; w < width; w++) {
			for(int r = 0; r < 2; r++) 
				image_data_color[(h * width * 3) 
					+ (w * 3)] = image_data[(h*width)+w];
		}
	Image tempImage(width, height, "RGB", CharPixel, image_data_color);
	tempImage.display();
}
