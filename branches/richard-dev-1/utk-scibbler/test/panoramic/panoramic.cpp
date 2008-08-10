/**
 * Image Stitching for Panoramic. 
 *
 * Author Richard Edwards
 */
#include "Scribbler.h"
#include <Magick++.h>
#include <math.h>
#include <iostream>
#include <vector>
using namespace std;
using namespace Magick;

/**
 * Displays and saves the image to the specified file name.
 */
int saveImage(unsigned char * image, int width, int height,
		char * fileName);

/**
 * Takes an array of images and uses the overLap vector to
 * stitch the images together and returns a pointer to the new
 * image. Note the new image's height is 192, and its width
 * can be found in final_width.
 */
unsigned char * 
	buildImage(unsigned char ** imageArray, int numImages, 
				int width, int height, vector< pair<int,int> > overLap,
				int &final_width);

/**
 * Builds a list of all the points for merge the images together.
 *
 * Example:
 * Image List: Image1, Image2, Image3, Image4
 * OverLap List: <Image1 x, Image2 x1>, <Image2 x2, Image3 x1>,
 *				 <Image3 x2, Image4 x>
 *
 */
vector< pair<int,int> > 
	buildMergeList(unsigned char ** imageArray, int numImages);

/**
 * Given two images it determines the best intersection which produces the
 * least amount of error.
 */
pair<int,int> computeOverlap(unsigned char * image1, 
		unsigned char * image2, int image1_offset);


int main(int argc, char ** argv) {
		
	if(argc < 3) {
		fprintf(stderr, "Usuage: ./pana num_images image_name\n");
		return -1;
	}

	int numImages = atoi(argv[1]);
	
	if(numImages < 0) {
		fprintf(stderr, "Invalid Number of Images\n");
		return -1;
	}

	Scribbler * robot = new Scribbler();

	if(robot->connect() < 0) {
	     perror("Failed");
	     return -1;
	}

	unsigned char ** imageArray 
		= (unsigned char**)malloc(sizeof(unsigned char*) * numImages);

	for(int i = 0; i < numImages; i++) {
		imageArray[i] = robot->takePicture("color");
		robot->turnLeft(-0.5, .15); //this will become more complicated
	}

	int final_width = 0;
	unsigned char * panoImage 
		= buildImage(imageArray, numImages, 256, 192, 
				buildMergeList(imageArray, numImages), final_width );
	saveImage(panoImage, final_width, 192, argv[2]);

	return 0;
}

vector< pair<int,int> >
	buildMergeList(unsigned char ** imageArray, int numImages) {

	vector< pair<int,int> > resultVector;
	pair<int,int> tempPair(7,7);
	for(int i = 0; i < numImages-1; i++) {
		//Feed in the overlap point previously used in the image
		//when it was i+1, and is now i. Note initial offset is 7
		//because we want to ignore the black trim on the image.
		tempPair = computeOverlap(imageArray[i], imageArray[i+1],
				tempPair.second+1);
		resultVector.push_back(tempPair);
	}

	return resultVector;
}


pair<int,int> computeOverlap(unsigned char * image1, 
		unsigned char * image2, int image1_offset) {
	int x1=-1, x2=-1;

	//This makes life easier so I can store the current intensity
	//of a given column from image1 to compare with against image2
	double * image1Intensity = (double*)malloc(sizeof(double) * 192);
	memset(image1Intensity, 0, sizeof(double) * 192);

	double tempTotal = 0;
	double squaredError = 0;

	double minSquared;
	int minSquaredSet = 0;

	//computes squared error for the differences in intensity between
	//the two images as image2 is slowly being slided over image1.
	for(int w1 = image1_offset; w1 < 256; w1++) {
		for(int h = 0; h < 192; h++) {
			for(int rgb = 0; rgb < 3; rgb++)
				tempTotal += image1[(h * 256 * 3) + (w1 * 3) + rgb];	
			image1Intensity[h] = tempTotal/3;
			tempTotal = 0;
		}

		for(int w2 = 8; w2 < 256; w2++) {
			for(int h = 0; h < 192; h++) {
				for(int rgb = 0; rgb < 3; rgb++) 
					tempTotal += image2[(h * 256 * 3) + (w2 * 3) + rgb];
				squaredError += pow(image1Intensity[h] - tempTotal/3, 2.0);
				tempTotal = 0;
			}
			if(!minSquaredSet) {
				minSquared = squaredError;
				minSquaredSet = 1;
				x1 = w1;
				x2 = w2;
			}
			else if( squaredError < minSquared ) {
				minSquared = squaredError;
				x1 = w1;
				x2 = w2; 
			}
			squaredError = 0;
		}
	}
	fprintf(stderr, "Min Squared Error: %lf\n", minSquared);
	fprintf(stderr, "X1: %i X2: %i\n", x1, x2);

	pair<int,int> result(x1, x2);

	return result;
}

/**
 * There are many ways to mix the stitching and the overlap computation
 * together, I chose to compute all overlap and then stitch. However,
 * the python version computes overlap, stitches, computes overlap with
 * new image and next image and stitches, etc.
 */
unsigned char * buildImage(unsigned char ** imageArray, 
		int numImages, int width, int height, 
		vector< pair<int,int> > overLap, int& final_width) {
	
	//Break the pairs apart into easy to manage arrays.
	//Note: You could break them apart into vectors, any container you choose.
	int * x2IndexArray = (int*)malloc(sizeof(int) * overLap.size());
	int * x1IndexArray = (int*)malloc(sizeof(int) * overLap.size());
	for(unsigned int i = 0; i < overLap.size(); i++) {
		x2IndexArray[i] = overLap.at(i).second;
		x1IndexArray[i] = overLap.at(i).first;
	}

	//Compute the final width of the images based on the intersections.
	final_width = x1IndexArray[0];
	for(unsigned int i = 0; i < overLap.size(); i++) {
		if( (i + 1) < overLap.size() ) {
			if(x1IndexArray[i+1] - x2IndexArray[i] > 0)
				final_width += (x1IndexArray[i+1] - x2IndexArray[i]);
		}
		else {
			final_width += (width - x2IndexArray[i]);
		}
	}

	unsigned char * image =
		(unsigned char*)malloc(sizeof(unsigned char)
				* (final_width * height * 3));

	int overLapSum = 0;

	for(int h = 0; h < height; h++) {
		overLapSum = 0;
		for(int i = 0, overIndex = 0; i < numImages-1; i++, overIndex++) { 
			
			if(!i) {
				//special case for placing the first image
				for(int w = 0; w < x1IndexArray[overIndex]; w++) {
					for(int rgb = 0; rgb < 3; rgb++)
						image[(h * final_width * 3) 
							+ (w * 3) + (i * width) * 3 + rgb] 
							= imageArray[i][(h * width * 3) + (w * 3) + rgb]; 
				}	
				overLapSum += x1IndexArray[overIndex];
			}
			//general case for placing a non end point image
			if((unsigned int)(overIndex + 1) < overLap.size()) {
				for(int w = x2IndexArray[overIndex], w2 = 0; 
						w < x1IndexArray[overIndex + 1]; 
						w++, w2++) {
					for(int rgb = 0; rgb < 3; rgb++) 
						image[(h * final_width * 3) + (w2 * 3) 
							+ overLapSum * 3 + rgb]
							= imageArray[i+1][(h * width * 3) + (w * 3) + rgb];
				}
				if( x1IndexArray[overIndex + 1] - x2IndexArray[overIndex] > 0 )
					overLapSum += (x1IndexArray[overIndex+1] 
							- x2IndexArray[overIndex]);	
			}
			else {
				//special case for place the last image
				for(int w = x2IndexArray[overIndex], w2 = 0;
						w < width; w++, w2++) {
					for(int rgb = 0; rgb < 3; rgb++) {
						image[(h * final_width * 3) + (w2 * 3)
							+ overLapSum * 3 + rgb]
							= imageArray[i + 1][(h * width * 3) 
							+ (w * 3) +rgb];
					}
				}
			}
		}
	}
	return image;
}

int saveImage(unsigned char * image, int width, int height,
		char * fileName) {
	Image panoImage(width, height, "RGB", CharPixel, image);
	panoImage.write(fileName);
	panoImage.display();
	return 0;
}
