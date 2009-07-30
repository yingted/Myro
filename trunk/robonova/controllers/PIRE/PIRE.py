# File:         PIRE.py
# Description:  Python controller program for testing Nao walk engine in the Robotstadium contest.
# Authors:      Daniel Lee, Ashley Gavin, Doug Blank, Teyvonia Thomas, Fabien.Rohrer@Cyberbotics.com
# Note:         Some functions were used from the nao_python controller written by Fabien.Rohrer@Cyberbotics.com
#               Dr. Lee wrote this algorithm in MATLAB
#               Teyvonia Thomas translated Dr. Lee's algorithm to Python
#               Doug Blank and Ashley Gavin rewrote the Python code and abstracted the methods as well as
#               abstracting Dr. Lee's algorithm

from controller import Robot, Accelerometer, Camera, DistanceSensor, \
                       Emitter, Gyro, LED, Motion, Receiver, Servo, \
                       TouchSensor
from math import *
from numpy import *
from numpy.linalg import *
import sys
sys.path.append("/home/compsci/Desktop/webots/projects/robots/robonova/kinematics/")
from kinelib import *
from ikinelegs import *

class WebotController(Robot):

  def __init__(self):
    Robot.__init__(self)

    #timestep info
    self.timeStep = 40
    self.f = finfo(float) 

    #Choose Humanoid Robot
    self.bot = Nao(self, self.timeStep)
  
    #Keep track of starting and stopping of walk cycle
    self.idle = True
    self.stepStop = True
    self.stepStart = True
    self.stepCount = 0
    self.stepSign = -1 # -1 for right support, +1 for left support

    self.tStep = 0.5
    self.upPhase = 0.3
    self.downPhase = 0.8
    
    self.t0 = self.getTime()
    self.t = 0
    self.dt = 0
    self.tPhase = 0.
    self.phShift = 0.
    self.key = 0

    # define the motion which is currently playing
    self.currentlyPlaying = None
    self.start()  
   
  
    #Print Menu
    self.bot.printHelp()
    self.walkForward()

    while not self.key:
      self.key = self.keyboardGetKey()      
      self.bot.update()
      self.simulationStep()   
    
    

  #Walk Commands, controlled by velocity
  ##maybe change to self.bot.setVelocity
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
    
  
  def stopMoving(self):
    self.pLeft = array([self.bot.uLeft[0], self.bot.uLeft[1], 0, 0, 0, self.bot.uLeft[2]])
    self.pRight = array([self.bot.uRight[0], self.bot.uRight[1], 0, 0, 0, self.bot.uRight[2]])
    self.pBody = array([self.bot.uBody[0], self.bot.uBody[1], self.bot.bodyHeight, 0, 0, self.bot.uBody[2]])
    pos = ikineLegs('LLeg',self.pLeft,self.pBody,())
    ###############################################
    for p in range(6):
      self.bot.qLegs[p] = pos[p]
    pos = ikineLegs('RLeg',self.pRight,self.pBody,())
    #################################################
    for p in range(6):
      self.bot.qLegs[6 + p] = pos[p]
    self.setLegs('joint',self.bot.qLegs)
    self.bot.velCurrent = array([0., 0., 0.])
    self.idle = True
    

  def setIkineLegs(self, legs, startIndex, stopIndex, legName, pos, bod, *varArg):
    #self.legsBefore = ikineLegs(legName, pos, bod, varArg)
    posMatrix = trPosition6D(pos)
    oldLegs = ikine(self.bot.legChain, posMatrix, m=[1,1,1,1,1,0])
    self.legsBefore = [0.0, oldLegs[0,0], oldLegs[0,1], oldLegs[0,2], oldLegs[0,3], oldLegs[0,4]]
    if startIndex == 0:
      for i in range(startIndex, stopIndex):      
        legs[i] = self.legsBefore[i]
    else:
      for i in range(startIndex, stopIndex):
        legs[i] = self.legsBefore[i-6]
    return legs

  def setVelocity(self,vCommand):
    self.bot.velScale = array([1., 2.5, .15])
    self.vCommandT = array(vCommand)
    self.vCommand = self.vCommandT.transpose()
    self.vCommandS = ((dot(self.bot.velScale,self.vCommand))**2)
    self.bot.velMax = 0.06
      
    self.vMag = sqrt(self.vCommandS.sum())
    
    self.c = min(self.bot.velMax/self.vMag, 1)
    self.vCommand = self.c*self.vCommand
    self.bot.velCommand = self.vCommand
    return self.bot.velCommand

  def poseRelative(self, du, u0):
    g = u0.copy()
    self.yaw = u0[2] 
    self.cy = cos(self.yaw)
    self.sy = sin(self.yaw)    
    self.u1 = g
    self.u1[0] = u0[0] + self.cy*du[0] - self.sy*du[1]
    self.u1[1] = u0[1] + self.sy*du[0] + self.cy*du[1]
    self.u1[2] = u0[2] + du[2]
    
    return self.u1


  def targetScrew(self,v, n):
    # Compute target pose after n steps of v using screw
    self.A = n
    self.B = 0
    self.alpha = .5*v[2]
    if abs(self.alpha) > self.f.eps:
      self.A = .5*(sin((2*n-1)*self.alpha)/sin(self.alpha) + 1)
      self.B = .5*(cos((2*n-1)*self.alpha) - cos(self.alpha))/sin(self.alpha)

    self.delX = self.A*v[0] + self.B*v[1] 
    self.delY = -self.B*v[0] + self.A*v[1] 
    self.delTh = n*v[2]
    self.uTarget = array([self.delX, self.delY, self.delTh])
    return self.uTarget

  def zmpCom(self,t, x0, x1, tau):
    self.ph = t*1.0/tau 
    self.x = x0*cosh(self.ph) + (x1-x0*cosh(1/tau))*sinh(self.ph)/sinh(1/tau)
    return self.x
  
  #Step Cycle Functions
  def run(self):
    if self.key:
      self.bot.runCommand(self.key)
    self.bot.update()
    self.simulationStep()

    self.key = self.keyboardGetKey()

  def start(self):
    self.idle = False
    self.stepStop = False
    self.stepStart = True
    self.t0 = self.getTime()
    
  def stop(self):
    self.stepStop = True
  
  def simulationStep(self):
    if self.step(self.timeStep) == -1:
      self.terminate()    

  def printTime(self):
    print self.getTime()


  #Joint Setting Functions  
  def setArms(self, joint, startIndex, endIndex, position):
    if startIndex == 2:
      positionIndex = 0
    else:
      positionIndex = 4
    for index in range(startIndex, endIndex):
      position = position.flatten()
      self.bot.joints[index].setPosition(position[positionIndex]) 
      positionIndex += 1


  def setLegs(self, joint, position):
    positionIndex = 0
    for index in range(6, 18):
      position = position.flatten()
      self.bot.joints[index].setPosition(position[positionIndex]) 
      positionIndex += 1

class FootSensorArray:

  def __init__(self, timeStep, webot):
    # foot sensors
    self.timeStep = timeStep
    self.webot = webot
    self.footSensors = [[], []]
    self.fsrNames = [['LFsrFL', 'LFsrFR', 'LFsrBR', 'LFsrBL'],
                     ['RFsrFL', 'RFsrFR', 'RFsrBR', 'RFsrBL']]
    for i in range(0, len(self.fsrNames)):
      for j in range(0, len(self.fsrNames[0])):
        self.footSensors[i].append(webot.getTouchSensor(self.fsrNames[i][j]))
        self.footSensors[i][j].enable(self.timeStep)
    
  def update(self):
    retval = array([[],[]]) # force sensor values
    for i in range(0,len(self.fsr[0])):
      retval[0].append(self.footSensors[0][i].getValue())
      retval[1].append(self.footSensors[1][i].getValue())
    return retval

  def __repr__(self):
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


class Nao:
  def __init__(self, webot, timeStep):
    self.timeStep = timeStep
    self.webot = webot
    self.joints = []
    self.findAndEnableDevices()
    self.initializeBody()

  def initializeBody(self):
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
    self.velMax = 0.06
    self.velScale = array([1., 2.5, .15])
    self.velChange = 0.25

    self.odometry = array([0., 0., 0.])
    self.odometryScale = 0.99

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
    self.qArms = zeros((8,1),float)
    self.qArmsT = array([120, 15, -90, -60, 120, -15, 90, 60])
    self.qArmsT2 = self.qArmsT.transpose() 
    self.qArms0 = (pi/180)*self.qArmsT2
    self.armsIndex1Start = 2
    self.armsIndex1Stop = 6
    
    self.armsIndex2Start = 18 
    self.armsIndex2Stop = 22

    #Alternate Inverse Kinematics System
    L = []
    L.append(Link(A=0.0)) #Hip Roll
    L.append(Link(A=0.1001)) #Hip Pitch
    L.append(Link(A=0.1027)) #Knee Pitch
    L.append(Link(A=0.0)) #Ankle Pitch
    L.append(Link(A=0.0)) #Ankle Roll
    self.legChain = Chain(L,name='Leg')
     

  #Update the walk Cycle
  def update(self):
    if self.webot.idle:
      return    
    self.webot.t = self.webot.getTime()
    self.webot.dt = self.webot.t - self.webot.t0
    self.webot.t0 = self.webot.t
    self.webot.tPhase = self.webot.tPhase + self.webot.dt
    if (self.webot.tPhase > self.webot.tStep):
      self.odometry = self.odometry + self.odometryScale*self.Odometry
      # Start new step
      self.webot.tPhase = mod(self.webot.tPhase, self.webot.tStep)
      self.webot.stepStart = True
 

    if self.webot.stepStart:
      if self.webot.stepStop:
        self.webot.stopMoving()
        return
    
      self.webot.stepCount = self.webot.stepCount + 1
      self.webot.stepSign = -self.webot.stepSign
      self.webot.phShift = 0
      self.velCurrent = self.velCurrent + self.velChange*(self.velCommand - self.velCurrent)
      self.uBody1 = self.uBody2
      self.uLeft1 = self.uLeft2
      self.uRight1 = self.uRight2

      self.duBodyTarget = self.webot.targetScrew(self.velCurrent, 1.5)
      
      self.uBodyTarget = self.webot.poseRelative(self.duBodyTarget, self.uBody1)
      if (self.webot.stepSign > 0):
        self.uRight2 = self.webot.poseRelative(self.uRight0, self.uBodyTarget)
        if (self.uRight2[2] > self.uLeft2[2]):
          self.uRight2[2] = self.uLeft2[2]
   
      else:
        self.uLeft2 = self.webot.poseRelative(self.uLeft0, self.uBodyTarget)
        # Do not pigeon-toe feet
        if (self.uLeft2[2] < self.uRight2[2]):
          self.uLeft2[2] = self.uRight2[2]
    
      self.uLegsAve = .5*(self.uLeft2+self.uRight2)

      self.uBody2 = self.webot.poseRelative(-.5*(self.uLeft0+self.uRight0), self.uLegsAve)
      self.Odometry = self.uBody2-self.uBody1

      self.webot.stepStart = False


    self.phBody = self.webot.tPhase/self.webot.tStep
    self.phSwing = (self.phBody-self.webot.upPhase)/(self.webot.downPhase-self.webot.upPhase)


    self.phSwing = self.phSwing + self.webot.phShift
    self.phSwing = max(min(self.phSwing, 1), 0)


    self.swingZ = sin(pi*self.phSwing)
    self.swingX = self.phSwing

    self.bodyHeight = self.bodyHeight + self.dHeight*cos(2*pi*self.phBody)
    self.bodyTilt = self.bodyTilt
    self.bodyRoll = -self.webot.stepSign*self.bodyRoll*sin(pi*self.phSwing)

    if (self.webot.stepSign > 0):
      self.uLeft = self.uLeft1
      self.pLeft = array([self.uLeft[0], self.uLeft[1], 0, 0, 0, self.uLeft[2]])
      self.uRight = self.uRight1 + self.swingX*(self.uRight2-self.uRight1)
      self.pRight = array([self.uRight[0], self.uRight[1], self.stepHeight*self.swingZ, 0, 0, self.uRight[2]])

      self.uSupport = self.webot.poseRelative([self.supportX, self.supportY, 0], self.uLeft)
        
    else:
      self.uRight = self.uRight1
      self.pRight = [self.uRight[0], self.uRight[1], 0, 0, 0, self.uRight[2]]
      self.uLeft = self.uLeft1 + self.swingX*(self.uLeft2-self.uLeft1)
      self.pLeft = [self.uLeft[0], self.uLeft[1], self.stepHeight*self.swingZ, 0, 0, self.uLeft[2]]

      self.uSupport = self.webot.poseRelative([self.supportX, -self.supportY, 0], self.uRight)
 

    self.uZmp = self.webot.zmpCom(self.phBody, self.uBody1-self.uSupport, self.uBody2-self.uSupport, self.tZmp/self.webot.tStep)
    self.uBody = self.uSupport + self.uZmp

    self.uBody[2] = .5*(self.uLeft[2]+self.uRight[2])
    self.pBody = [self.uBody[0], self.uBody[1], self.bodyHeight, self.bodyRoll, self.bodyTilt, self.uBody[2]]

    if (self.webot.stepSign > 0):
      self.webot.setIkineLegs(self.qLegs, 0, 6,'LLeg', self.pLeft, self.pBody)      
      #######################
      self.webot.setIkineLegs(self.qLegs, 6, 12,'RLeg', self.pRight, self.pBody)

    else:
      self.webot.setIkineLegs(self.qLegs, 6, 12,'RLeg', self.pRight, self.pBody)
      #######################
      self.webot.setIkineLegs(self.qLegs, 0, 6,'LLeg', self.pLeft, self.pBody)


    self.webot.setLegs('joint',self.qLegs)     
    self.qArms = self.qArms0
    self.qArms[0] =  2*(self.qLegs[8]+ 97.4 *pi/180)
    self.qArms[3] = self.qArms0[3]  
    self.qArms[4] = 2*(self.qLegs[2] + 97.4*pi/180)
    self.qArms[7]  = self.qArms0[7]      
    self.webot.setArms('joint',self.armsIndex1Start, self.armsIndex1Stop,self.qArms)
    self.webot.setArms('joint',self.armsIndex2Start, self.armsIndex2Stop,self.qArms)

  def findAndEnableDevices(self): 
    # camera
    self.camera = self.webot.getCamera('camera')
    self.camera.enable(4*self.timeStep)
    
    # camera selection (high/low)
    self.cameraSelect = self.webot.getServo('CameraSelect')
    self.cameraSelect.setPosition(0.70) 

    # move arms along the body
    self.leftShoulderPitch = self.webot.getServo("LShoulderPitch")
    self.rightShoulderPitch = self.webot.getServo("RShoulderPitch")
  
    self.headYaw = self.webot.getServo("HeadYaw")
    self.headPitch = self.webot.getServo("HeadPitch")
  
    # accelerometer
    self.accelerometer = self.webot.getAccelerometer('accelerometer')
    self.accelerometer.enable(self.timeStep)
    
    # gyro
    self.gyro = self.webot.getGyro('gyro')
    self.gyro.enable(self.timeStep)

    self.jointNames = ['HeadYaw', #1
                      'HeadPitch', #2
                      'LShoulderPitch', #3
                      'LShoulderRoll', #4
                      'LElbowYaw', #5
                      'LElbowRoll', #6
                      'LHipYawPitch', #7
                      'LHipRoll',   #8
                      'LHipPitch', #9
                      'LKneePitch', #10
                      'LAnklePitch', #11
                      'LAnkleRoll',  #12
                      'RHipYawPitch', #13
                      'RHipRoll', #14
                      'RHipPitch', #15
                      'RKneePitch', #16
                      'RAnklePitch', #17
                      'RAnkleRoll', #18
                      'RShoulderPitch', #19 
                      'RShoulderRoll', #20
                      'RElbowYaw', #21
                      'RElbowRoll'] #22

    self.armJoints = ['LShoulderPitch', 'LShoulderRoll', 'LElbowYaw', 'LElbowRoll',
                      'RShoulderPitch', 'RShoulderRoll', 'RElbowYaw', 'RElbowRoll' ]

    self.legJoints = [ 'LHipYawPitch', 'LHipRoll',  'LHipPitch', 
                       'LKneePitch', 'LAnklePitch', 'LAnkleRoll',
                      'RHipYawPitch', 'RHipRoll', 'RHipPitch',
                      'RKneePitch', 'RAnklePitch','RAnkleRoll']
    
    self.otherJoints = ['HeadYaw', 'HeadPitch']
                        
    for i in range(0, len(self.jointNames)):
      self.joints.append(self.webot.getServo(self.jointNames[i]))
      self.joints[i].enablePosition(self.timeStep)
                        
    # ultrasound sensors
    self.us = []
    usNames = ['US/TopRight','US/BottomRight','US/TopLeft','US/BottomLeft']
    for i in range(0, len(usNames)):
      self.us.append(self.webot.getDistanceSensor(usNames[i]))
      self.us[i].enable(self.timeStep)
    
    # foot bumpers
    self.lfootlbumper = self.webot.getTouchSensor('LFoot/Bumper/Left')
    self.lfootrbumper = self.webot.getTouchSensor('LFoot/Bumper/Right')
    self.rfootlbumper = self.webot.getTouchSensor('RFoot/Bumper/Left')
    self.rfootrbumper = self.webot.getTouchSensor('RFoot/Bumper/Right')
    self.lfootlbumper.enable(self.timeStep)
    self.lfootrbumper.enable(self.timeStep)
    self.rfootlbumper.enable(self.timeStep)
    self.rfootrbumper.enable(self.timeStep)
    
    # There are 7 controlable LED groups in Webots
    self.leds = []
    self.leds.append(self.webot.getLED('ChestBoard/Led'))
    self.leds.append(self.webot.getLED('RFoot/Led'))
    self.leds.append(self.webot.getLED('LFoot/Led'))
    self.leds.append(self.webot.getLED('Face/Led/Right'))
    self.leds.append(self.webot.getLED('Face/Led/Left'))
    self.leds.append(self.webot.getLED('Ears/Led/Right'))
    self.leds.append(self.webot.getLED('Ears/Led/Left'))
    
    # emitter & receiver
    self.emitter = self.webot.getEmitter('emitter')
    self.receiver = self.webot.getReceiver('receiver')
    self.receiver.enable(self.timeStep)
    
    # for sending 'move' request to Supervisor
    self.superEmitter = self.webot.getEmitter('super_emitter')
    
    # keyboard
    self.webot.keyboardEnable(10*self.timeStep)

        
  def printHelp(self):
    print
    print '----------nao_soccer_player_red.py ----------'
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
    print '----------nao_soccer_player_red.py----------'

  # the accelerometer axes are oriented as on the real robot
  # however the sign of the returned values may be opposite
  def printAcceleration(self):
    acc = self.accelerometer.getValues()
    print
    print '----------accelerometer----------'
    print 'acceleration: [ x y z ] = ' + str(acc)
    print '----------accelerometer----------'

  # the gyro axes are oriented as on the real robot
  # however the sign of the returned values may be opposite
  def printGyro(self):
    vel = self.gyro.getValues()
    print
    print '----------gyro----------'
    # z value is meaningless due to the orientation of the Gyro
    print 'angular velocity: [ x y ] = ' + str(vel[0:2])
    print '----------gyro----------'
    
  #Printing Functions for Nao
  def printFootSensors(self):

    print self.footSensorArray

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

  
  #Nao commands and helpers
  def runCommand(self, key):
    if key == ord('0'):
      self.webot.stop()      
    elif key == ord('1'):
      self.start()    
    if key == Robot.KEYBOARD_LEFT:
      self.webot.stepLeft()
    elif key == Robot.KEYBOARD_RIGHT:
      self.webot.stepRight()
    elif key == Robot.KEYBOARD_UP:
      self.webot.walkForward()      
    elif key == Robot.KEYBOARD_DOWN:
      self.webot.walkBackward()
    elif key == Robot.KEYBOARD_LEFT | Robot.KEYBOARD_SHIFT:
      self.webot.spinLeft()      
    elif key == Robot.KEYBOARD_RIGHT| Robot.KEYBOARD_SHIFT:
      self.webot.spinRight	  
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


    #Head Control
    elif key == ord('P'):
      self.setJoint(array([0,1]), array([0, -10])*pi/180)

    elif key == ord('S'):
      self.setJoint(array([0,1]), array([45, -10])*pi/180)

   
    elif key == ord('C'):
      self.setJoint(array([0,1]), array([-45, -10])*pi/180)

   
    elif key == ord('D'):
       self.setJoint(array([0,1]), array([0, -35])*pi/180)

    elif key == ord('E'):
      self.setJoint(array([0,1]), array([45, -35])*pi/180)

     
    elif key == ord('M'):
      self.setJoint(array([0,1]), array([-45, -35])*pi/180)

    elif key == ord('N'):
      self.setJoint(array([0,1]), array([0, 20])*pi/180)

    elif key == ord('V'):
      self.setJoint(array([0,1]), array([45, 20])*pi/180)

  def setJoint(self, jointIndex, position):
    for i in range(len(jointIndex)):
      self.joints[jointIndex[i]].setPosition(position[i])

       

  def getRGB(self):
    # read rgb pixel values from the camera
    # ret = wb_camera_get_image(self.camera)
    image = self.webot.camera.getImageArray()
    return image

  def getAcc(self):
    self.acc = self.webot.accelerometer.getValues()
    return self.acc

  def getGps(self):
    self.gpsVal = self.webot.gps.getValues()
    
    return self.gpsVal

  def setAllLedsColor(self,rgb):
    # these leds take RGB values
    for i in range(0, len(self.leds)):
      self.webot.leds[i].set(rgb)

    # ear leds are single color (blue)
    # and take values between 0 - 255
    self.webot.leds[5].set(rgb & 0xFF)
    self.webot.leds[6].set(rgb & 0xFF)

class Robonova:
  def __init__(self, webot, timeStep):
    self.webot = webot
    self.timeStep = timeStep
    self.joints = []
    self.findAndEnableDevices()
    self.initializeBody()

  def initializeBody(self):
    self.bodyHeight = 0
    self.dHeight = 0
    self.tZmp = 0
    self.stepHeight = 0
    self.bodyRoll = 0
    self.bodyTilt = 0

    self.supportX = 0
    self.supportY = 0
    
    self.velCurrent = array([0., 0., 0.0]) 
    self.velCommand = array([0., 0., 0.]) 
    self.velMax = 0
    self.velScale = array([0, 0, 0])
    self.velChange = 0

    self.odometry = array([0., 0., 0.])
    self.odometryScale = 0

    self.footX = -self.supportX
    self.footY = 0

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

    self.qLegs = zeros((12,1),float) #not real
    self.qArms = zeros((8,1),float) #not real
    self.qArmsT = array([0,0,0,0])
    self.qArmsT2 = self.qArmsT.transpose() 
    self.qArms0 = 0
    self.armsIndex1Start = 0
    self.armsIndex1Stop = 2
    self.armsIndex2Start = 13 
    self.armsIndex2Stop = 15
     

  #Update the walk Cycle
  def update(self):
    #TOO BE MADE
    print "noUpdate"

  def findAndEnableDevices(self): 
    # camera
    self.camera = self.webot.getCamera('camera')
    self.camera.enable(4*self.timeStep)
    
    # move arms along the body
    self.leftShoulderPitch = self.webot.getServo("LShoulderPitch")
    self.rightShoulderPitch = self.webot.getServo("RShoulderPitch")
  

    self.jointNames = ['LShoulderPitch',
                      'LShoulderRoll',  
                      'LElbowRoll',
                      'LHipRoll',  
                      'LHipPitch', 
                      'LKneePitch', 
                      'LAnklePitch',
                      'LAnkleRoll',  
                      'RHipRoll',
                      'RHipPitch',
                      'RKneePitch',
                      'RAnklePitch',
                      'RAnkleRoll', 
                      'RShoulderPitch',  
                      'RShoulderRoll', 
                      'RElbowRoll']

    for i in range(0, len(self.jointNames)):
      self.joints.append(self.webot.getServo(self.jointNames[i]))
      self.joints[i].enablePosition(self.timeStep)
                        
  #keyboard
    self.webot.keyboardEnable(10*self.timeStep) 
  def printHelp(self):
    print "noHelp"
    #TO BE MADE


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

  
  #Nao commands and helpers
  def runCommand(self, key):
  #TO BE MADE
    print "noCommands"

controller = WebotController()

while True:
  controller.run()
