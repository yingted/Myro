#include "VideoStream.h"
#include "ImageWindow.h"
#include <assert.h>
#include <FL/Fl.H>
#include <FL/Fl_Image.H>
#include <FL/Fl_Window.H>
#include <FL/fl_draw.H>

#define image_height 192
#define image_width 256
#define RGB 3

typedef struct Data {
	unsigned char ** image;
	Scribbler * robot;
	int live;
	ImageWindow * imageWindow;

	pthread_mutex_t * refresh_lock;
	pthread_cond_t * signal_display;

	pthread_mutex_t * run_gui_lock;
	pthread_cond_t * signal_run_gui;

	pthread_mutex_t * filterLock;

	int runGUI; 
	int imageReady;

	int image_buffer_write;
	int image_buffer_read; 
	int color_mode;

	std::vector<Filter*> * filters;

} * _data;

void * start_stream(void * data);
void * capture_image(void * data);
void * display_stream(void * data);

VideoStream::VideoStream(Scribbler * scrib, int color_mode) {
	image_buffer 
		= (unsigned char**)malloc(sizeof(unsigned char*) * image_height
				* image_width);
	memset(image_buffer, 0, image_width * image_height);
	this->myScrib = scrib;
	this->color_mode = color_mode;
	this->filters = new std::vector<Filter*>();

	filterLock = (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t));
	pthread_mutex_init(filterLock, NULL);
}

VideoStream::~VideoStream() {
	free(image_buffer);
	free(filterLock);
	filters->clear();
	delete filters;
}

void * start_stream(void * data) {

	Fl_Window * window = new Fl_Window(256,192, "Robot Image");
	ImageWindow * imageWindow = new ImageWindow(0,0,256,192,NULL);

	_data videoData = (_data)data;
	videoData->imageWindow = imageWindow;
	videoData->runGUI = 0;
	videoData->imageReady = 0;
	imageWindow->set_color_mode(videoData->color_mode);

	pthread_t capture_thread;
	pthread_create(&capture_thread, NULL, capture_image, data);


	pthread_t display_thread;
	pthread_create(&display_thread, NULL, display_stream, data);

	while(!videoData->runGUI)
		pthread_cond_wait(videoData->signal_run_gui, videoData->run_gui_lock);
	window->end();
	window->show();

	pthread_mutex_unlock(videoData->run_gui_lock);

	for(;;) {
		if( window->visible() ) {
			if(!Fl::check())
				break;
		}
		else if( !Fl::wait() )
			break;
		videoData->imageWindow->redraw();
	}

	videoData->live = 0;
	pthread_exit(NULL);
}

void * display_stream(void * data) {

	_data videoData = (_data)data;
	pthread_mutex_lock(videoData->run_gui_lock);

	videoData->runGUI = 1;
	//printf("waiting on Picture\n");
	while(!videoData->image[videoData->image_buffer_read]) {
		//printf("IMAGE NOT READY\n");
		pthread_cond_wait(videoData->signal_display, videoData->refresh_lock);
	}

	//printf("Picture Received\n");
	pthread_mutex_lock(videoData->filterLock);	
	for(unsigned int i = 0; i < videoData->filters->size(); i++) {
		videoData->filters->at(i)->
			applyFilter(videoData->image[videoData->image_buffer_read]);
	}
	pthread_mutex_unlock(videoData->filterLock);

	videoData->imageWindow->
		loadImageSource(videoData->image[videoData->image_buffer_read],
				image_width, image_height);

	//printf("Image Constructed Preparing to free buffered Image\n");
	if(videoData->image_buffer_read != 0) {
		free(videoData->image[videoData->image_buffer_read-1]);
		videoData->image[videoData->image_buffer_read-1] = NULL;
	}
	videoData->image_buffer_read = 
		(videoData->image_buffer_read + 1)%(image_height * image_width);

	videoData->imageReady = 0;
	videoData->imageWindow->redraw();

	pthread_mutex_unlock(videoData->refresh_lock);

	pthread_cond_signal(videoData->signal_run_gui);
	pthread_mutex_unlock(videoData->run_gui_lock);

	while(videoData->live) {
		//printf("Waiting on Picture\n");
		while(!videoData->image[videoData->image_buffer_read])
			pthread_cond_wait(videoData->signal_display, 
					videoData->refresh_lock);

		//printf("Picture Received\n");

		pthread_mutex_lock(videoData->filterLock);
		for(unsigned int i = 0; i < videoData->filters->size(); i++) {
			videoData->filters->at(i)->
				applyFilter(videoData->image[videoData->image_buffer_read]);
		}
		pthread_mutex_unlock(videoData->filterLock);

		videoData->imageWindow->
			loadImageSource(videoData->image[videoData->image_buffer_read],
					image_width, image_height);

		if(videoData->image_buffer_read != 0) {
			assert(videoData->image[videoData->image_buffer_read-1] != NULL);
			free(videoData->image[videoData->image_buffer_read-1]);
			videoData->image[videoData->image_buffer_read-1] = NULL;
		}
		videoData->image_buffer_read =
			(videoData->image_buffer_read + 1)%(image_height * image_width);

		videoData->imageReady = 0;
		videoData->imageWindow->redraw();

		//printf("Image Updated\n");

		pthread_mutex_unlock(videoData->refresh_lock);
	}

	pthread_exit(NULL);
}

void * capture_image(void * data) {
	_data videoData = (_data)data;

	unsigned char * tempImageBuffer; 

	while(videoData->live) {
		pthread_mutex_lock(videoData->refresh_lock);	

		//printf("Capturing Image\n");

		switch(videoData->color_mode) {
			case 0: tempImageBuffer
						= videoData->robot
							->takePicture("grayjpeg")->getRawImage();
				break;
			case 1:
					tempImageBuffer
						= videoData
						 ->robot->takePicture("jpeg")->getRawImage();
				break;
			case 2:
					tempImageBuffer
						= videoData->robot
						 ->takePicture("blob")->getRawImage();
				break;
			default: 
			{
				fprintf(stderr, "Invalid Color Mode\n");
				videoData->live = 0;
				pthread_cond_signal(videoData->signal_display);
				pthread_mutex_unlock(videoData->refresh_lock);
				pthread_exit(NULL);
			}
		}

		//printf("Image Loaded into Memory\n");

		videoData->image[videoData->image_buffer_write] = tempImageBuffer;
		videoData->image_buffer_write 
			= (videoData->image_buffer_write+1)%(image_width * image_height);
		videoData->imageReady = 1;

		//printf("Image Captured\n");

		pthread_mutex_unlock(videoData->refresh_lock);
		pthread_cond_signal(videoData->signal_display);
		//if(!videoData->color_mode) legacy!
		usleep(1000); //hack to slow down the capture thread
					  //so that the other threads are scheduled
	}
	pthread_exit(NULL);
}

void VideoStream::startStream() {
	_data videoData = (_data)malloc(sizeof(struct Data));
	videoData->image = image_buffer;
	videoData->robot = myScrib;
	videoData->live = 1;
	videoData->image_buffer_write = 0;
	videoData->image_buffer_read = 0;
	videoData->color_mode = this->color_mode;
	videoData->filters = this->filters;
	videoData->filterLock = this->filterLock;

	this->live = &videoData->live;

	videoData->refresh_lock 
		= (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t));
	pthread_mutex_init(videoData->refresh_lock, NULL);
	videoData->signal_display 
		= (pthread_cond_t*)malloc(sizeof(pthread_cond_t));
	pthread_cond_init(videoData->signal_display, NULL);

	videoData->run_gui_lock
		= (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t));
	pthread_mutex_init(videoData->run_gui_lock, NULL);
	videoData->signal_run_gui
		= (pthread_cond_t*)malloc(sizeof(pthread_cond_t));
	pthread_cond_init(videoData->signal_run_gui, NULL);

	pthread_create(&camThread, NULL, start_stream, (void*)videoData);
}

int VideoStream::addFilter(Filter * filter) {
	pthread_mutex_lock(filterLock);
	int result = 0;
	filters->push_back(filter);
	result = filters->size()-1;
	pthread_mutex_unlock(filterLock);
	return result;
}

int VideoStream::delFilter(int filter_location) {
	if(filter_location < 0 || filter_location > (int)filters->size())
		return -1;

	pthread_mutex_lock(filterLock);
	filters->erase(filters->begin() + filter_location);
	pthread_mutex_unlock(filterLock);

	return 0;
}

void VideoStream::endStream() {
	if(live) 
		*(this->live) = 0;
}

