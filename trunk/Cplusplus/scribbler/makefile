CC = g++
INCLUDES = headers `fltk-config --cxxflags` `Magick++-config --cppflags`
FLTKLIB = `fltk-config --ldflags --libs`
MAGICKLIB = `Magick++-config --ldflags --libs`
CFLAGS = -g -Wall -I$(INCLUDES)
EXECUTABLES = test_motor test_camera video_stream test_blob \
			  lightFollow

all: libscribbler.a libvideostream.a test_motor test_camera test_blob \
	 video_stream

clean:
	rm -f core $(EXECUTABLES) libscribbler.a libvideostream.a *.o

.SUFFIXES: .cpp .o
#.cpp.o:
#	$(CC) $(CFLAGS) -c src/*.cpp

libscribbler.a: src/Myro.cpp src/Scribbler.cpp src/serial.cpp src/Robot.cpp src/ColorPicture.cpp src/GrayPicture.cpp src/Picture.cpp
	$(CC) $(CFLAGS) -c src/Myro.cpp src/Scribbler.cpp src/serial.cpp src/Robot.cpp src/Picture.cpp src/GrayPicture.cpp src/ColorPicture.cpp
	ar ru libscribbler.a Scribbler.o serial.o Robot.o Picture.o GrayPicture.o ColorPicture.o
	ranlib libscribbler.a

test_motor: test/test_motor.cpp libscribbler.a
	$(CC) $(CFLAGS) -c test/test_motor.cpp
	$(CC) -o test_motor test_motor.o libscribbler.a $(MAGICKLIB)

test_camera: test/test_camera.cpp libscribbler.a
	$(CC) $(CFLAGS) -c test/test_camera.cpp
	$(CC) -o test_camera test_camera.o libscribbler.a $(MAGICKLIB)

test_blob: test/test_blob.cpp libscribbler.a libvideostream.a
	$(CC) $(CFLAGS) -c test/test_blob.cpp
	$(CC) -o test_blob test_blob.o libscribbler.a libvideostream.a $(MAGICKLIB) \
	$(FLTKLIB)

libvideostream.a: src/VideoStream.cpp src/ImageWindow.cpp src/Filter.cpp \
				  libscribbler.a
	$(CC) $(CFLAGS) -c src/VideoStream.cpp src/ImageWindow.cpp  \
	src/Filter.cpp
	ar ru libvideostream.a VideoStream.o ImageWindow.o Filter.o libscribbler.a
	ranlib libvideostream.a

video_stream: test/video_stream.cpp libvideostream.a
	$(CC) $(CFLAGS) -c test/video_stream.cpp
	$(CC) -o video_stream video_stream.o libvideostream.a libscribbler.a $(MAGICKLIB) $(FLTKLIB)

