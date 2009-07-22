# File:         Robonova.py
# Description:  Python controller program for testing Robonova walk
#               engine Actual walk module using ZMP, sensor feedback,
#               etc. to set joint angles
# Authors:      Daniel Lee, Teyvonia Thomas, Fabien.Rohrer@Cyberbotics.com,
#               Ashley Gavin
# Note:         Some functions were used from the nao_python controller 
#               written by Fabien.Rohrer@Cyberbotics.com

from math import *
from numpy import *
from numpy.linalg import *
from ikinematics import *
from controller import Robot, Accelerometer, Camera, DistanceSensor, \
                       Emitter, Gyro, LED, Motion, Receiver, Servo, \
                       TouchSensor

class Robonova(Robot):
  """
  Main class to explore walking on the Robonova Webots model.
  """
  def __init__(self):
    Robot.__init__(self)
    # define the motion which is currently playing
    self.currentlyPlaying = None
    self.key = 0
    self.timeStep = 40
    self.joints = []

    self.findAndEnableDevices()
    
    self.f = finfo(float) 
    ###AshGavs edit
    self.fsr = self.getFsr()
    self.idle = True
    self.stepStop = True
    self.stepStart = True
    self.stepCount = 0
    self.stepSign = -1 # -1 for right support, +1 for left support

    self.tStep = 0.5
    self.upPhase = 0.3
    self.downPhase = 0.8

    # Nao details:
    self.bodyHeight = 0.28
    self.dHeight = 0.00
    self.tZmp = 0.17
    self.stepHeight = 0.025
    self.bodyRoll = 0*pi/180
    self.bodyTilt = 0*pi/180

    self.supportX = 0.015
    self.supportY = 0

    self.velCurrent = array([0., 0., 0.0]) 
    self.velCommand = array([0., 0., 0.]) 
    #self.setVelocity(array([1., 0, 0.]))
    self.velMax = 0.06
    self.velScale = array([1., 2.5, .15])
    self.velChange = 0.25

    self.velOdometry = array([0., 0., 0.])
    self.odometry = array([0., 0., 0.])
    self.odometryScale = 0.99

    #   self.footX = 0
    self.footX = -self.supportX
    self.footY = 0.0525
    self.uLeft0 = array([self.footX, self.footY, 0.])
    self.uLeft = self.uLeft0
    self.uLeft1 = self.uLeft
    self.uLeft2 = self.uLeft

    self.uRight0 = array([self.footX, -self.footY, 0.])
    self.uRight = self.uRight0
    self.uRight1 = self.uRight
    self.uRight2 = self.uRight

    self.uBody = array([0., 0., 0.])
    self.uBody1 = self.uBody
    self.uBody2 = self.uBody

    self.qLegs = zeros((12,1),float)
    self.legsIndex = range(6,18)#[6:18] #changed 7 to 6
    self.qLegsIndexStart = 6
    self.qLegsIndexStop = 18
    self.hsupT = array([.8, .8, .8, .8, .6, .6])
    self.hardnessSupport = self.hsupT.transpose()
    self.hswingT = array([.8, .6, .6, .6, .2, .2])
    self.hardnessSwing = self.hswingT.transpose()
    self.qArms = zeros((8,1),float)
    self.qArmsT = array([120, 15, -90, -60, 120, -15, 90, 60])
    self.qArmsT2 = self.qArmsT.transpose() 
    self.qArms0 = (pi/180)*self.qArmsT2
    print "self.qArms0: "
    print self.qArms0
    self.armsIndex1Start = 0
    self.armsIndex1Stop = 3
    
    self.armsIndex2Start = 13 
    self.armsIndex2Stop = 15
  
    self.hardnessArms = ones((8,1),float)*.2

    #self.setJointName("LAnklePitch", "RAnklePitch", -.2)
    #self.setJointName("LHipPitch", "RHipPitch", -.2)
    #self.setJointName("LKneePitch", "RKneePitch", -.2)
    #self.setJointName("LHipPitch", "RHipPitch", -.3)
    #self.setJointName("LHipRoll", "RHipRoll", -.2)

    self.t0 = self.getTime() 
    self.tPhase = 0.
    self.phShift = 0.
    
    self.start()  
    self.printHelp()
    
    #Move Left
    self.setVelocity([0.0, 0.03, 0.0]) 
   
    while not self.key:
      self.key = self.keyboardGetKey()      
      self.update()
      self.simulationStep()   
    
  def printHelp(self):
    print
    print '----------------Robonova.py ----------------'
    print '[Up][Down]: move one step forward/backwards'
    print '[<-][->]: side step left/right'
    print '[Shift] + [<-][->]: turn left/right'
    print '[0]: Stop'
    print '[1]: Start'
    print '[Home][End]: camera selection (high/low)'
    print '[2][3][4]: change all leds RGB color'
    print '[0]: turn all leds off'
    print '[A]: print Acceleration'
    print '[G]: print Gyro values'
    print '[F]: print Foot Sensors'
    print '[B]: print Foot Bumpers'
    print '[U]: print Ultrasound Sensors'
    print '[I]: print Camera Image'
    print '[T]: print Time'
    print '[P][S][C][D][E][M][V][N]: Move Head'
    print '----------------Robonova.py ----------------'

  # the accelerometer axes are oriented as on the real robot
  # however the sign of the returned values may be opposite
  ### AshGavs edit (comment out accelerometer)
  def printAcceleration(self):
    acc = self.accelerometer.getValues()
    print
    print '----------accelerometer----------'
    print 'acceleration: [ x y z ] = ' + str(acc)
    print '----------accelerometer----------'

  # the gyro axes are oriented as on the real robot
  # however the sign of the returned values may be opposite
  ### AshGavs edit (comment out gyro)
  def printGyro(self):
    vel = self.gyro.getValues()
    print
    print '----------gyro----------'
    # z value is meaningless due to the orientation of the Gyro
    print 'angular velocity: [ x y ] = ' + str(vel[0:2])
    print '----------gyro----------'
    

  def printFootSensors(self):
    newtons = 0.0
    fsv = [[],[]] # force sensor values
    
    for i in range(0,len(self.fsr[0])):
      fsv[0] = self.fsr[0][i].getValue()
      fsv[1] = self.fsr[1][i].getValue()
      newtons += fsv[0][i] + fsv[1][i]
    
    print
    print '----------foot sensors----------'
    print '   left       right'
    print '+--------+ +--------+'
    print '|'  + str(round(fsv[0][0],1)) + \
          '  ' + str(round(fsv[0][1],1)) + \
          '| |'+ str(round(fsv[1][0],1)) + \
          '  ' + str(round(fsv[1][1],1)) + \
          '|  front'
    print '|        | |        |'
    print '|'  + str(round(fsv[0][3],1)) + \
          '  ' + str(round(fsv[0][2],1)) + \
          '| |'+ str(round(fsv[1][3],1)) + \
          '  ' + str(round(fsv[1][2],1)) + \
          '|  back'
    print '+--------+ +--------+'
    print 'total: ' + str(newtons) + \
          ' Newtons, ' + str(newtons/9.81) + ' kilograms'
    print '----------foot sensors----------'

  def printFootBumpers(self):
    ll = self.lfootlbumper.getValue()
    lr = self.lfootrbumper.getValue()
    rl = self.rfootlbumper.getValue()
    rr = self.rfootrbumper.getValue()
    print
    print '----------foot bumpers----------'
    print '   left       right'
    print '+--------+ +--------+'
    print '|'  + str(ll) + '  ' + str(lr) + '| |'+ str(rl) + '  ' + str(rr) + '|'
    print '|        | |        |'
    print '|        | |        |'
    print '+--------+ +--------+'
    print '----------foot bumpers----------'

  def printUltrasoundSensors(self):
    dist = []
    for i in range(0, len(self.us)):
      dist.append(self.us[i].getValue())

    print
    print '-----ultrasound sensors-----'
    print 'top:   left: ' + str(dist[2]) + \
          ' m, right ' + str(dist[0]) + ' m'
    print 'bottom left: ' + str(dist[3]) + \
          ' m, right ' + str(dist[1]) + ' m'
    print '-----ultrasound sensors-----'

  def printCameraImage(self):
    scaled = 2 # defines by which factor the image is subsampled
    width = self.camera.getWidth()
    height = self.camera.getHeight()
    
    # read rgb pixel values from the camera
    image = self.camera.getImageArray()
    
    print
    print '----------camera image (grey levels)---------'
    print 'original resolution: ' + str(width) + ' x ' + \
           str(height) + ', scaled to ' + str(width/scaled) + \
           ' x ' + str(height/scaled)
    for x in range(0, width/scaled):
      line = ''
      for y in range(0, height/scaled):
        grey = image[x*scaled][y*scaled][0] + \
               image[x*scaled][y*scaled][1] + \
               image[x*scaled][y*scaled][2]
        grey = grey / 3
        grey = grey * 9 / 255 # between 0 and  instead of 0 and 255
        line = line + str(int(grey))
      print line
    print '----------camera image----------'
    

  def walkForward(self):
    self.setVelocity([.03, 0., 0.])
    
  def walkBackward(self):
    self.setVelocity([-.03, 0., 0.])
    
  def stepLeft(self):
    self.setVelocity([0., 0.01, 0])
    
  def stepRight(self):
    self.setVelocity([0., -0.01, 0])    

  def spinLeft(self):
    self.setVelocity([0., 0.0, 0.01])
    
  def spinRight(self):
    self.setVelocity([0., 0.0, -0.01])
    
  def walkDiagonallyForward(self):
    self.setVelocity([.03, 0.03, 0.])
    
  def walkDiagonallyBackward(self):
    self.setVelocity([-.03, -0.03, 0.])
    
  def runCommand(self, key):
    if key == ord('0'):
      self.stop()      
    elif key == ord('1'):
      self.start()    
    if key == Robot.KEYBOARD_LEFT:
      self.stepLeft()
    elif key == Robot.KEYBOARD_RIGHT:
      self.stepRight()
    elif key == Robot.KEYBOARD_UP:
      print "Walk Forward"
      self.walkForward()      
    elif key == Robot.KEYBOARD_DOWN:
      self.walkBackward()
    elif key == Robot.KEYBOARD_LEFT | Robot.KEYBOARD_SHIFT:
      self.spinLeft()      
    elif key == Robot.KEYBOARD_RIGHT| Robot.KEYBOARD_SHIFT:
      self.spinRight	 
    ###AshGavs edit (comment out gyro and accelerometer)
    elif key == ord('A'):
      self.printAcceleration()
    elif key == ord('G'):
      self.printGyro()
    elif key == ord('F'):
      self.printFootSensors()
    elif key == ord('B'):
      self.printFootBumpers()
    elif key == ord('U'):
      self.printUltrasoundSensors()
    elif key == ord('I'):
      self.printCameraImage()
    elif key == ord('T'):
      self.printTime() 
    ###AshGavs Edit (Coment Out cameraSelect)
    elif key == Robot.KEYBOARD_HOME:
      self.cameraSelect.setPosition(0)
    elif key == Robot.KEYBOARD_END:
      self.cameraSelect.setPosition(1)
    elif key == ord('2'):
      self.setAllLedsColor(0xff0000) # red
    elif key == ord('3'):
      self.setAllLedsColor(0x00ff00) # green
    elif key == ord('4'):
      self.setAllLedsColor(0x0000ff) # blue
    elif key == ord('5'):
      self.setAllLedsColor(0x000000) # off

    #Move head to various positions
    #0 is head yaw and 1 is head pitch
    ###AshGavs Edit (comment out head movement)
    elif key == ord('P'):
      self.setJoint(array([0,1]), array([0, -10])*pi/180)
    elif key == ord('S'):
      self.setJoint(array([0,1]), array([45, -10])*pi/180)
    elif key == ord('C'):
      self.setJoint(array([0,1]), array([-45, -10])*pi/180)
    elif key == ord('D'):
      self.setJoint(array([0,1]), array([0, -35])*pi/180)
    elif key == ord('E'):
      elf.setJoint(array([0,1]), array([45, -35])*pi/180)
    elif key == ord('M'):
      self.setJoint(array([0,1]), array([-45, -35])*pi/180)
    elif key == ord('N'):
      self.setJoint(array([0,1]), array([0, 20])*pi/180)
    elif key == ord('V'):
      self.setJoint(array([0,1]), array([45, 20])*pi/180)

  def run(self):
    if self.key:
      self.runCommand(self.key)
    self.update()
    self.simulationStep()
    self.key = self.keyboardGetKey()

### Originally from walk.py

  def update(self):
    if self.idle:
      return
    t = self.getTime()
    dt = t - self.t0
    self.t0 = t
    self.tPhase = self.tPhase + dt
    if (self.tPhase > self.tStep):
      self.odometry = self.odometry + self.odometryScale*self.velOdometry
      # Start new step
      self.tPhase = mod(self.tPhase, self.tStep)
      self.stepStart = True
 
    if self.stepStart:
     print "stepStart..."
     if self.stepStop:
        print "stepStop!"
        self.pLeft = array([self.uLeft[0], self.uLeft[1], 0, 0, 0, self.uLeft[2]])
       
        self.pRight = array([self.uRight[0], self.uRight[1], 0, 0, 0, self.uRight[2]])
        self.pBody = array([self.uBody[0], self.uBody[1], self.bodyHeight, 0, 0, self.uBody[2]])
        self.qLegs[0:6] = ikineLegs('LLeg',self.pLeft,self.pBody)
        self.qLegs[6:12] = ikineLegs('RLeg',self.pRight,self.pBody)
        self.setLegs('joint',self.qLegs)
        
        self.velCurrent = array([0., 0., 0.])
        self.idle = True
        return
    print "walking.."
    self.stepCount = self.stepCount + 1
    self.stepSign = -self.stepSign
    self.phShift = 0
    self.velCurrent = self.velCurrent + self.velChange*(self.velCommand - self.velCurrent)
    self.uBody1 = self.uBody2
    self.uLeft1 = self.uLeft2
    self.uRight1 = self.uRight2

    self.duBodyTarget = self.targetScrew(self.velCurrent, 1.5)
    
    self.uBodyTarget = self.poseRelative(self.duBodyTarget, self.uBody1)
    if (self.stepSign > 0):
      self.uRight2 = self.poseRelative(self.uRight0, self.uBodyTarget)
      if (self.uRight2[2] > self.uLeft2[2]):
        self.uRight2[2] = self.uLeft2[2]
   
      else:
        self.uLeft2 = self.poseRelative(self.uLeft0, self.uBodyTarget)
        # Do not pigeon-toe feet
        if (self.uLeft2[2] < self.uRight2[2]):
          self.uLeft2[2] = self.uRight2[2]
    
      self.uLegsAve = .5*(self.uLeft2+self.uRight2)

      self.uBody2 = self.poseRelative(-.5*(self.uLeft0+self.uRight0), self.uLegsAve)

      self.velOdometry = self.uBody2-self.uBody1

      self.stepStart = False
    
      

    self.phBody = self.tPhase/self.tStep
    self.phSwing = (self.phBody-self.upPhase)/(self.downPhase-self.upPhase)


    self.phSwing = self.phSwing + self.phShift
    self.phSwing = max(min(self.phSwing, 1), 0)


    self.swingZ = sin(pi*self.phSwing)
    self.swingX = self.phSwing

    self.bodyHeight = self.bodyHeight + self.dHeight*cos(2*pi*self.phBody)
    self.bodyTilt = self.bodyTilt
    self.bodyRoll = -self.stepSign*self.bodyRoll*sin(pi*self.phSwing)

    if (self.stepSign > 0):
      self.uLeft = self.uLeft1
      self.pLeft = array([self.uLeft[0], self.uLeft[1], 0, 0, 0, self.uLeft[2]])
      self.uRight = self.uRight1 + self.swingX*(self.uRight2-self.uRight1)
      self.pRight = array([self.uRight[0], self.uRight[1], self.stepHeight*self.swingZ, 0, 0, self.uRight[2]])

      self.uSupport = self.poseRelative([self.supportX, self.supportY, 0], self.uLeft)
        
    else:
      self.uRight = self.uRight1
      self.pRight = [self.uRight[0], self.uRight[1], 0, 0, 0, self.uRight[2]]
      self.uLeft = self.uLeft1 + self.swingX*(self.uLeft2-self.uLeft1)
      self.pLeft = [self.uLeft[0], self.uLeft[1], self.stepHeight*self.swingZ, 0, 0, self.uLeft[2]]

      self.uSupport = self.poseRelative([self.supportX, -self.supportY, 0], self.uRight)


    self.uZmp = self.zmpCom(self.phBody, self.uBody1-self.uSupport, self.uBody2-self.uSupport, self.tZmp/self.tStep)
    self.uBody = self.uSupport + self.uZmp

    self.uBody[2] = .5*(self.uLeft[2]+self.uRight[2])
    self.pBody = [self.uBody[0], self.uBody[1], self.bodyHeight, self.bodyRoll, self.bodyTilt, self.uBody[2]]


          
    if (self.stepSign > 0):
      self.setIkineLegs(self.qLegs, 0, 6,'LLeg', self.pLeft, self.pBody)      
      self.setIkineLegs(self.qLegs, 6, 12,'RLeg', self.pRight, self.pBody,self.qLegs[0])
    else:
      self.setIkineLegs(self.qLegs, 6, 12,'RLeg', self.pRight, self.pBody)
      self.setIkineLegs(self.qLegs, 0, 6,'LLeg', self.pLeft, self.pBody,self.qLegs[6])
  
    self.setLegs('joint',self.qLegs)  
    raise KeyboardInterrupt
    self.qArms = self.qArms0
    self.qArms[0] =  2*(self.qLegs[8]+ 97.4 *pi/180)
    self.qArms[3] = self.qArms0[3]  
    self.qArms[4] = 2*(self.qLegs[2] + 97.4*pi/180)
    self.qArms[7]  = self.qArms0[7]      
    self.setArms('joint',self.armsIndex1Start, self.armsIndex1Stop,self.qArms)
    self.setArms('joint',self.armsIndex2Start, self.armsIndex2Stop,self.qArms)
    
  def setIkineLegs(self, legs, startIndex, stopIndex, legName, pos, bod, *varArg):
    self.legsBefore = ikineLegs(legName, pos, bod, varArg)

    if startIndex == 0:
      for i in range(startIndex, stopIndex):      
        legs[i] = self.legsBefore[i]
      return legs
    else:
      for i in range(startIndex, stopIndex):
        legs[i] = self.legsBefore[i-6]
      return legs

  def start(self):
    self.idle = False
    self.stepStart = True
    self.stepStop = False
    self.t0 = self.getTime()

  def stop(self):
    self.stepStop = True
    
  def done(self):
    return self.idle

  def setVelocity(self,vCommand):
    self.velScale = array([1., 2.5, .15])
    self.vCommandT = array(vCommand)
    self.vCommand = self.vCommandT.transpose()
    self.vCommandS = ((dot(self.velScale,self.vCommand))**2)
    self.velMax = 0.06
      
    self.vMag = sqrt(self.vCommandS.sum())
    
    self.c = min(self.velMax/self.vMag, 1)
    self.vCommand = self.c*self.vCommand
    self.velCommand = self.vCommand
    return self.velCommand

  def poseRelative(self, du, u0):
    """
    Returns the relative delta du from position u0.
    """
    yaw = u0[2] 
    cy = cos(yaw)
    sy = sin(yaw)    
    u1 = u0.copy()
    u1[0] = u0[0] + cy*du[0] - sy*du[1]
    u1[1] = u0[1] + sy*du[0] + cy*du[1]
    u1[2] = u0[2] + du[2]
    return u1

  def targetScrew(self, v, n):
    """
    Computes target pose after n steps of v using screw
    """
    A = n
    B = 0
    alpha = .5 * v[2]
    if abs(alpha) > self.f.eps:
      A = .5 * (sin((2 * n - 1) * alpha)/sin(alpha) + 1)
      B = .5 * (cos((2 * n - 1) * alpha) - cos(alpha))/sin(alpha)
    delX =  A * v[0] + B * v[1] 
    delY = -B * v[0] + A * v[1] 
    delTh = n * v[2]
    return array([delX, delY, delTh])

  def zmpCom(self, t, x0, x1, tau):
    ph = t * 1.0/tau 
    x = x0 * cosh(ph) + (x1 - x0 * cosh(1/tau)) * sinh(ph)/sinh(1/tau)
    return x

  def findAndEnableDevices(self):
    # camera
    self.camera = self.getCamera('camera')
    self.camera.enable(4*self.timeStep)
    
    # camera selection (high/low)
    #AshGavs Edit (comment out camera select)
    ###self.cameraSelect = self.getServo('CameraSelect')
    ###self.cameraSelect.setPosition(0.70) 

    # move arms along the body  #mod
    self.leftShoulderPitch = self.getServo("LShoulderPitch")
    #self.leftShoulderPitch.setPosition(1.5)
    self.rightShoulderPitch = self.getServo("RShoulderPitch")
    #self.rightShoulderPitch.setPosition(1.5)

    ###AshGavs Edit (comment out head)
    ###self.headYaw = self.getServo("HeadYaw")
    #self.headYaw.enablePosition(self.timeStep)
    ###self.headPitch = self.getServo("HeadPitch")
    #self.headPitch.enablePosition(self.timeStep)
    
    # accelerometer
    ###AshGavs edit (comment out accelerometer)
    ###self.accelerometer = self.getAccelerometer('accelerometer')
    ###self.accelerometer.enable(self.timeStep)
    
    # gyro
    ###AshGavs edit (comment out gyro)
    ###self.gyro = self.getGyro('gyro')
    ###self.gyro.enable(self.timeStep)

    self.jointNames = [### AshGavs (comment out head)'HeadYaw', 'HeadPitch', 
                      'LShoulderPitch', 'LShoulderRoll',
                      ###'LElbowYaw',
                      'LElbowRoll', 
                      ###'LHipYawPitch', 
                      'LHipRoll', 'LHipPitch', 
                      'LKneePitch', 'LAnklePitch', 'LAnkleRoll', 
                      ###'RHipYawPitch', 
                      'RHipRoll', 'RHipPitch', 
                      'RKneePitch', 'RAnklePitch', 'RAnkleRoll',
                      'RShoulderPitch', 'RShoulderRoll',
                      ###'RElbowYaw', 
                      'RElbowRoll'
                      ]
            
    
    
    self.jointMaxForce = array([###AshGavs(comment out head)2.27, 2.61, 
                        2.27, 2.61, ###2.27, \
                        2.61, ###11.97, 
                        11.97, 7.78, 7.78, 7.78, \
                        11.97, ###11.97, 
                        11.97, 7.78, 7.78, 7.78, \
                        11.97, 2.27, 2.61, ###2.27
                        2.61
                        ])
                        
    for i in range(0, len(self.jointNames)):
      self.joints.append(self.getServo(self.jointNames[i]))
      self.joints[i].enablePosition(self.timeStep)
      #self.joints.append(0.0)
                        
                        
    # ultrasound sensors
    ###AshGavs Edit (comment out all unneccesary sensors)
    ###self.us = []
    ###usNames = ['US/TopRight','US/BottomRight','US/TopLeft','US/BottomLeft']
    ###for i in range(0, len(usNames)):
      ###self.us.append(self.getDistanceSensor(usNames[i]))
      ###self.us[i].enable(self.timeStep)
    
    # foot sensors
    self.fsr = [[], []] # [left, right]
    fsrNames = [['LFsrFL', 'LFsrFR', 'LFsrBR', 'LFsrBL'], # Left Sensors 
                ['RFsrFL', 'RFsrFR', 'RFsrBR', 'RFsrBL']] # Right Sensors
    for i in range(0, len(fsrNames)):
      for j in range(0, len(fsrNames[0])):
        self.fsr[i].append(0) ## self.getTouchSensor(fsrNames[i][j]))
        ## self.fsr[i][j].enable(self.timeStep)
    
    # foot bumpers
    ###self.lfootlbumper = self.getTouchSensor('LFoot/Bumper/Left')
    ###self.lfootrbumper = self.getTouchSensor('LFoot/Bumper/Right')
    ###self.rfootlbumper = self.getTouchSensor('RFoot/Bumper/Left')
    ###self.rfootrbumper = self.getTouchSensor('RFoot/Bumper/Right')
    ###self.lfootlbumper.enable(self.timeStep)
    ###self.lfootrbumper.enable(self.timeStep)
    ###self.rfootlbumper.enable(self.timeStep)
    ###self.rfootrbumper.enable(self.timeStep)
    
    # There are 7 controlable LED groups in Webots
    ###self.leds = []
    ###self.leds.append(self.getLED('ChestBoard/Led'))
    ###self.leds.append(self.getLED('RFoot/Led'))
    ###self.leds.append(self.getLED('LFoot/Led'))
    ###self.leds.append(self.getLED('Face/Led/Right'))
    ###self.leds.append(self.getLED('Face/Led/Left'))
    ###self.leds.append(self.getLED('Ears/Led/Right'))
    ###self.leds.append(self.getLED('Ears/Led/Left'))
    
    # emitter & receiver
    ###self.emitter = self.getEmitter('emitter')
    ###self.receiver = self.getReceiver('receiver')
    ###self.receiver.enable(self.timeStep)
    
    # for sending 'move' request to Supervisor
    ###self.superEmitter = self.getEmitter('super_emitter')
    
    # keyboard
    self.keyboardEnable(10*self.timeStep) 
    
  def simulationStep(self):
    # simulationStep changed to simulationStep
    if self.step(self.timeStep) == -1:
      self.terminate()    
    
  def printTime(self):
    #getTime
    self.time = self.getTime()
    print self.time

  def getRGB(self):
    # read rgb pixel values from the camera
    # ret = wb_camera_get_image(self.camera)
    image = self.camera.getImageArray()
    return image

  def setJoint(self, jointIndex, position):
    for i in range(len(jointIndex)):
      self.joints[jointIndex[i]].setPosition(position[i])

  def setJoint1(self, index1, index2, position):
      if type(position) == ndarray:      
        position = position.flatten()
        self.joints[index1-1].setPosition(position[0])  
        self.joints[index2-1].setPosition(position[0])
      else:
     
        self.joints[index1-1].setPosition(position)
        self.joints[index2-1].setPosition(position)    

  def setJointName(self, name1, name2, position):
    index1 = self.jointNames.index(name1)
    index2 = self.jointNames.index(name2)
    if name1 == "LAnkleRoll" and name2 == "RAnkleRoll":
      self.joints[index1].setPosition(-position)
      self.joints[index2].setPosition(-position)
    elif name1 == "LKneePitch" and name2 == "RKneePitch":
      self.joints[index1].setPosition(-position)
      self.joints[index2].setPosition(position)
    elif name1 == "LShoulderPitch" and name2 == "RShoulderPitch":
      self.joints[index1].setPosition(-position)
      self.joints[index2].setPosition(position)
    elif name1 == "LHipPitch" and name2 == "RHipPitch":
      self.joints[index1].setPosition(-position)
      self.joints[index2].setPosition(position)
    elif name1 == "LShoulderRoll" and name2 == "RShoulderRoll":
      self.joints[index1].setPosition(position)
      self.joints[index2].setPosition(position)
    elif name1 == "LElbowRoll" and name2 == "RElbowRoll":
      self.joints[index1].setPosition(position)
      self.joints[index2].setPosition(position)
    elif name1 == "LHipRoll" and name2 == "RHipRoll":
      self.joints[index1].setPosition(-position)
      self.joints[index2].setPosition(-position)
    else:
      self.joints[index1].setPosition(position)
      self.joints[index2].setPosition(-position)  
  
  def setArms(self, joint, startIndex, endIndex, position):
    if startIndex == 0:
      positionIndex = 0
    else:
      positionIndex = 3
    for index in range(startIndex, endIndex):
      position = position.flatten()
      self.joints[index].setPosition(position[positionIndex]) 
      positionIndex += 1

  def setLegs(self, joint, position):
    # This function sets the leg joint positions
    # input args: joint - name of joint; position - array of joint positions
     
    positionIndex = 0
    for index in range(2, 12):
      position = position.flatten()
      self.joints[index].setPosition(position[positionIndex]) 
      self.jointNames.index(name1)
      positionIndex += 1

  def getJoint(self, index):  
    self.position = self.getServo(self.jointNames[index])
    return self.position

  #def setHardness(self, hardness, index):
    # This can make simulation unstable!
    #for i in range(0,len(self.joints)):
    # make args variable to set hardness of more than one joints at a time
    #self.joints[index].setMotorForce(hardness * self.jointMaxForce[index])
      
  
  ### AshGavs Edit (comment out unneccesary sensors)
  ###def getAcc(self):
    ###self.acc = self.accelerometer.getValues()
    ###return self.acc

  def getFsr(self):
    fsv = array([[],[]]) # force sensor values
    for i in range(0, len(self.fsr[0])):
      fsv[0]= 1 #fsr[0][i].getValue()
      fsv[1]= 1 #fsr[1][i].getValue()
    return fsv

  ###def getGps(self):
   ### self.gpsVal = self.gps.getValues()
    
    ###return self.gpsVal

  ###def setAllLedsColor(self,rgb):
    # these leds take RGB values
    ###for i in range(0, len(self.leds)):
    ###  self.leds[i].set(rgb)

    # ear leds are single color (blue)
    # and take values between 0 - 255
    ###self.leds[5].set(rgb & 0xFF)
    ###self.leds[6].set(rgb & 0xFF)

controller = Robonova()

while True:
  controller.run()
