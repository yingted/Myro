from numpy import array 

import Image
import time
import serial

class BufferedRead:
    def __init__(self, serial, size, start = 1):
        self.serial = serial
        self.size = size
        if start:
            self.data = self.serial.read(size)
        else:
            self.data = ""
    def __getitem__(self, position):
        """ Return an element of the string """
        while position >= len(self.data):
            self.data += self.serial.read(self.size - len(self.data))
            #print "      length so far = ", len(self.data), " waiting for total = ", self.size
        return self.data[position]
    def __len__(self):
        """ Lie. Tell them it is this long. """
        return self.size

def conf_window(ser, X_LOW, Y_LOW, X_HIGH, Y_HIGH, X_STEP, Y_STEP):

    print "Configuring window ", X_LOW, Y_LOW, X_HIGH, Y_HIGH, X_STEP, Y_STEP
    ser.write('Y') # 89
    ser.write(chr(X_LOW)) 
    ser.write(chr(Y_LOW)) 
    ser.write(chr(X_HIGH))
    ser.write(chr(Y_HIGH))
    ser.write(chr(X_STEP))
    ser.write(chr(Y_STEP))

def grab_window(ser, lx, ly, ux, uy, xstep, ystep):
    height = (uy - ly) / ystep
    width = (ux - lx) / xstep
    size = width * height
    print "grabbing image width = ", width, "height = ", height
    ser.write('X') # 88
    line = BufferedRead(ser, size)
    buffer = array([0] * (height * width * 3), 'B')
    for i in range(height):
        for j in range(width):
            if j >= 3:
                # go to the left for other values
                vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3
            else:
                # go to the right for other values
                vy = 1; vu = 2; y1v = 3; y1u = 1; uy = 1; uv = 2; y2u = 3; y2v = 1
                                   #   0123 0123 0123
            if ((j % 4) == 0): #0 #3
                Y = line[i * width + j]
                V = line[i * width + j + y1v]
                U = line[i * width + j + y1u]
            elif ((j % 4) == 1): #1 #0
                U = line[i * width + j]
                Y = line[i * width + j + uy]
                V = line[i * width + j + uv]
            elif ((j % 4) == 2): #2 #1
                Y = line[i * width + j]
                U = line[i * width + j + y2u]
                V = line[i * width + j + y2v]
            elif ((j % 4) == 3): #3 #2
                V = line[i * width + j]
                Y = line[i * width + j + vy]
                U = line[i * width + j + vu]
            U = (ord(U) - 128)       
            V = (ord(V) - 128)
            Y = ord(Y)
            buffer[(i * width + j) * 3 + 0] = max(min(Y + 1.13983 * V, 255), 0)
            buffer[(i * width + j) * 3 + 1] = max(min(Y - 0.39466*U-0.58060*V, 255), 0)
            buffer[(i * width + j) * 3 + 2] = max(min(Y + 2.03211*U, 255), 0)
        return Image.frombuffer("RGB", (width, height), buffer,
                                "raw", "RGB", 0, 1)
      
def grab_image(ser, width = 256, height = 192):
    buffer = array([0] * (height * width * 3), 'B')
    oldtimeout = ser.timeout
    ser.setTimeout(.01)
    ser.write('R') # 82
    size= width*height
    line = BufferedRead(ser, size, start = 0)
    #create the image from the YUV layer
    for i in range(height):
        for j in range(width):   
            if j >= 3:
                # go to the left for other values
                vy = -1; vu = -2; y1v = -1; y1u = -3; uy = -1; uv = -2; y2u = -1; y2v = -3
            else:
                # go to the right for other values
                vy = 1; vu = 2; y1v = 3; y1u = 1; uy = 1; uv = 2; y2u = 3; y2v = 1
                                   #   0123 0123 0123
            if ((j % 4) == 0): #3 #2   VYUY VYUY VYUY
                V = line[i * width + j] 
                Y = line[i * width + j + vy]
                U = line[i * width + j + vu]
            elif ((j % 4) == 1): #0 #3
                Y = line[i * width + j]
                V = line[i * width + j + y1v]
                U = line[i * width + j + y1u]
            elif ((j % 4) == 2): #1 #0
                U = line[i * width + j]
                Y = line[i * width + j + uy]
                V = line[i * width + j + uv]
            elif ((j % 4) == 3): #2 #1
                Y = line[i * width + j]
                U = line[i * width + j + y2u]
                V = line[i * width + j + y2v]
            U = (ord(U) - 128)       
            V = (ord(V) - 128)
            Y = ord(Y)
            buffer[(i * width + j) * 3 + 0] = max(min(Y + 1.13983 * V, 255), 0)
            buffer[(i * width + j) * 3 + 1] = max(min(Y - 0.39466*U-0.58060*V, 255), 0)
            buffer[(i * width + j) * 3 + 2] = max(min(Y + 2.03211*U, 255), 0)
    ser.setTimeout(oldtimeout)
    retval = Image.frombuffer("RGB", (width, height), buffer,
                              "raw", "RGB", 0, 1)
    return retval

def conf_rle(ser,
             delay = 90, smooth_thresh = 4,
             y_low=0, y_high=254,
             u_low=51, u_high=136,
             v_low=190, v_high=254):

    print "Configuring blobs"
    ser.write('T') # 84
    ser.write(chr(delay))
    ser.write(chr(smooth_thresh))
    ser.write(chr(y_low)) 
    ser.write(chr(y_high))
    ser.write(chr(u_low)) 
    ser.write(chr(u_high))
    ser.write(chr(v_low)) 
    ser.write(chr(v_high))

def grab_rle(ser):
    width = 256
    height = 192    
    blobs = array([0] * (height * width), 'B')
    ser.setTimeout(10)
    ser.flushInput()
    ser.write('S') # 83
    time.sleep(1)
    size=ord(ser.read(1))
    size = (size << 8) | ord(ser.read(1))
    print "Grabbing RLE image size =", size
    line = BufferedRead(ser, size)
    px = 0
    counter = 0
    val = 128
    inside = True
    for i in range(0, height, 1):
        for j in range(0, width, 4):
            if (counter < 1 and px < len(line)):
                counter = ord(line[px])     
                px += 1
                counter = (counter << 8) | ord(line[px])        
                px += 1

                if (inside):
                    val = 0
                    inside = False
                else:
                    val = 1
                    inside = True
            for z in range(0,4):
                blobs[i * width + j+z] = val # gray value
            counter -= 1
    retval = Image.frombuffer("1", (width, height), blobs,
                              "raw", "1", 0, 1)
    return retval

def read_2byte(ser):
    hbyte = ord(ser.read(1))
    lbyte = ord(ser.read(1))
    lpyte = (hbyte << 8) | lbyte
    return lbyte
    
def get_ir_left(ser):
    ser.write('U') # 85
    return read_2byte(ser)

def read_mem(ser, addr):
    ser.write('`') # 96
    ser.write(chr((addr >> 8) & 0xFF))
    ser.write(chr(addr & 0xFF))
    return ord(ser.read(1))

def write_mem(ser, addr, byte):
    ser.write('_') # 95
    ser.write(chr((addr >> 8) & 0xFF))
    ser.write(chr(addr & 0xFF))
    ser.write(chr(byte))
    
def get_ir_right(ser):
    ser.write('Z') # 90
    return read_2byte(ser)

def get_ir_middle(ser):
    ser.write('V') # 86
    return read_2byte(ser)

def get_battery(ser):
    ser.write('^') # 94
    return read_2byte(ser)

def set_ir_power(ser, power):
    ser.write('[') # 91
    ser.write(chr(power))

def set_led1_on(ser):
    ser.write('s') # 115 CONFLICT!

def set_led1_off(ser):
    ser.write('t') # 116

def set_led2(ser, value):
    ser.write('u') # 117
    ser.write(chr(value))

def yuv2rgb(Y, U, V):
    R = int(Y + (1.4075 * (V - 128)))
    G = int(Y - (0.3455 * (U - 128)) - (0.7169 * (V - 128)))
    B = int(Y + (1.7790 * (U - 128)))
    return [max(min(v,255),0) for v in (R, G, B)]

def rgb2yuv(R, G, B):
    Y = int(0.299 * R + 0.587 * G + 0.114 * B)
    U = int(-0.169 * R - 0.332 * G + 0.500 * B + 128)
    V = int( 0.500 * R - 0.419 * G - 0.0813 * B + 128)
    return [max(min(v,255),0) for v in (Y, U, V)]
