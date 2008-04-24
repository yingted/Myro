CROSS   = arm-elf-
CC      = $(CROSS)gcc
LD      = $(CROSS)ld
AR      = $(CROSS)ar
AS      = $(CROSS)as
CP      = $(CROSS)objcopy
OD	= $(CROSS)objdump
CFLAGS  = -Wall -I./ -c -fno-common -O3 #-msoft-float #-g
AFLAGS  = -ahls -mapcs-32
LDFLAGS = -Map main.map -Tldscript.cmd
CPFLAGS = -O ihex
ODFLAGS	= -x --syms

LIBGCC = -L/usr/local/arm/lib/gcc/arm-elf/4.1.1/ -lgcc
#LIBGCC  = -L/usr/local/lib/gcc/arm-elf/4.1.2 -lgcc

OBJFILES = crt.o grab_frame.o interrupts.o usleep.o uart.o main.o \
	ov7649.o fluke.o scribbler.o firmware_upgrade.o jpeg.o \
	uart_queue.o vic.o constants.o vision.o server-helper.o

all: main.hex

download:
	lpc21isp2 -control main.hex /dev/tty.usbserial 19200 20000

dl2: download

dl3:
	python update-firmware.py main.hex /dev/tty.scribbler5844 fast

main.hex: main.out
	$(CP) $(CPFLAGS) main.out main.hex
	$(OD) $(ODFLAGS) main.out > main.dmp

main.out: $(OBJFILES) ldscript.cmd
	$(LD) $(LDFLAGS) -o main.out $(OBJFILES) $(LIBGCC)

clean:
	-rm -f crt.lst a.lst main.lst grab_frame.lst usleep.lst interrupts.lst main.out main.hex main.map main.dmp $(OBJFILES) *.pyc

