#include "ImageWindow.h"
#include <FL/fl_draw.H>
#include <iostream>

ImageWindow::ImageWindow(int width, int height, char * title) 
	: Fl_Window(width, height, title) {
		image = NULL;
}

ImageWindow::ImageWindow(int x, int y, int width,
		int height, char * title)
	: Fl_Window(x, y, width, height, title) {
		image = NULL;
}

void ImageWindow::loadImageSource(unsigned char * data, int width, int height) {
	if(image)
		delete image;
	image = new Fl_RGB_Image((const unsigned char*)data, width, height);

}

void ImageWindow::set_color_mode(int color_mode) {
	this->color_mode = color_mode;
}

void ImageWindow::draw() {
	if(image != NULL) {
		if(color_mode)
			fl_draw_image((unsigned char*)(*(image->data())), 0, 0, image->w(),
					image->h());
		else
			fl_draw_image((unsigned char*)(*(image->data())), 0, 0, image->w(),
					image->h(), 1);
	}
}
