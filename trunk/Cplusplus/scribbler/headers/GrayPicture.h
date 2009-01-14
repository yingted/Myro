#ifndef __GRAY_PICTURE_H__
#define __GRAY_PICTURE_H__

#include "Picture.h"

class GrayPicture: public Picture {

	public: 

		GrayPicture(int width, int height);
		~GrayPicture();

		GrayPicture(unsigned char * data, int width, int height);
		GrayPicture(GrayPicture& pic);

		Pixel getPixel(int x, int y);
		void  setPixel(int x, int y, Pixel pix);
		void  show();

		unsigned char * getRawImage();

	private:

		unsigned char * image_data;
		int width;
		int height;
};

#endif
