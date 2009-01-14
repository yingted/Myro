#ifndef __PICTURE_H__
#define __PICTURE_H__

typedef struct Pixel_t {
	int R;
	int G;
	int B;
} Pixel;

class Picture {

	public:

		Picture(int width, int height);
		~Picture();

		virtual Pixel getPixel(int x, int y)=0;
		virtual void  setPixel(int x, int y, Pixel pix)=0;
		virtual void  show()=0;
		virtual unsigned char * getRawImage()=0;

		int getHeight();
		int getWidth();

	private:

		int width;
		int height;


};

#endif
