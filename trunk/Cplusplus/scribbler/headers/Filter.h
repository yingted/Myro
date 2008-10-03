#ifndef __FILTER_H__
#define __FILTER_H__

/**
 * @class Filter
 *
 * The class is used within the VideoStream package to allow for the
 * creation of various image filters, which can be applied to a stream
 * and the results will be visible in real time.
 *
 * @brief The class defines a frame work creating data filters
 * which can manipulate images, and apply any changes in the image 
 * data back to the image.
 *
 * @Author Richard Edwards
 */

class Filter {

	public:

	/**
	 * @param imageHeight - the height of images to be manipulated.
	 * @param imageWidth - the width of images to be manipulated.
	 * @param colorMode - An integer to determine the current 
	 * color space. (0 - GrayScale, 1 - RGB, 2 - RGB Blob)
	 */

	Filter(int imageHeight, int imageWidth, int colorMode);
	~Filter();

	/**
	 * A function for applying the filter to an image. Note,
	 * one should always invoke applyFilter, and not invoke filter.
	 *
	 * The reasoning behind this will be more clear, when the class
	 * is extended to allow for a filter to yield data from its
	 * application. 
	 */
	void applyFilter(unsigned char *);

	/**
	 * @return Returns the image height.
	 */
	int getHeight();

	/**
	 * @return Returns the image width.
	 */
	int getWidth();

	/**
	 * @return Returns the color Mode.
	 */
	int getColorMode();

	/**
	 * The function used by applyFilter to actually process
	 * an image. However, when developing your own filter,
	 * if you do not overwrite this function, then the filter's
	 * default behavior is to do nothing.
	 */
	virtual void filter(unsigned char *);

	private:

	int imageHeight;
	int imageWidth;
	int colorMode;

};

#endif
