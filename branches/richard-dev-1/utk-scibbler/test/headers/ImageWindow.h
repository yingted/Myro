#ifndef __IMAGE_WINDOW_H__
#define __IMAGE_WINDOW_H__

#include <Fl/Fl_Window.H>
#include <Fl/Fl_Image.H>

class ImageWindow : public Fl_Window {

	public:
		ImageWindow(int width, int height, char * title);
		ImageWindow(int x, int y, int width, int height, char * title);
		void set_color_mode(int color_mode);
		void loadImageSource(unsigned char * data, int width, int height);
		virtual void draw();

	private:

		Fl_RGB_Image* image;
		int color_mode;
};

#endif
