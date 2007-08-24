from dongle import *

s = serial.Serial("/dev/tty.scribbler", timeout=10)

print "Product ID of Camera (should be 0x76) 0x%x" % get_cam_param(s, 0x0A)
print "Version of Camera (should be 0x48) 0x%x" % get_cam_param(s, 0x0B)

print "Turning on White Balance, Gain Control, and Exposure Control"

set_cam_param(s, CAM_COMA, CAM_COMA_WHITE_BALANCE_ON)
set_cam_param(s, CAM_COMB, CAM_COMB_GAIN_CONTROL_ON | CAM_COMB_EXPOSURE_CONTROL_ON)

print "Reading COMA (should be 0x%x) 0x%x" % (CAM_COMA_WHITE_BALANCE_ON,
                                              get_cam_param(s, CAM_COMA))
print "Reading COMB (should be 0x%x) 0x%x" % (CAM_COMB_GAIN_CONTROL_ON |
                                              CAM_COMB_EXPOSURE_CONTROL_ON,
                                              get_cam_param(s, CAM_COMB))
before = time.time()
img = grab_image(s)
after = time.time()
print "t = ", (after - before)
img.show()

print "Turning off White Balance, Gain Control, and Exposure Control"

set_cam_param(s, CAM_COMA, CAM_COMA_WHITE_BALANCE_OFF)
set_cam_param(s, CAM_COMB, (CAM_COMB_GAIN_CONTROL_OFF & CAM_COMB_EXPOSURE_CONTROL_OFF))

print "Reading COMA (should be 0x%x) 0x%x" % (CAM_COMA_WHITE_BALANCE_OFF,
                                              get_cam_param(s, CAM_COMA))
print "Reading COMB (should be 0x%x) 0x%x" % (CAM_COMB_GAIN_CONTROL_OFF &
                                              CAM_COMB_EXPOSURE_CONTROL_OFF,
                                              get_cam_param(s, CAM_COMB))

before = time.time()
img = grab_image(s)
after = time.time()
print "t = ", (after - before)
img.show()
