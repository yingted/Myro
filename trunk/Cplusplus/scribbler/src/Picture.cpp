#include "Picture.h"

Picture::Picture(int width, int height) {
	this->width =  width;
	this->height = height;
}

Picture::~Picture() {
	//nothing to be done for now
}

int Picture::getHeight() {
	return height;
}

int Picture::getWidth() {
	return width;
}
