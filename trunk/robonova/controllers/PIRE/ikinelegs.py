# File:        ikineLegs.py
# Description: Contains Inverse kinematics for Nao legs

from numpy import *
from math import *
from numpy.linalg import inv

def rotX(a):
  ca = cos(a)
  sa = sin(a)
  r  = array([[1., 0., 0.], [0., ca*1.0, -sa*1.0], [0., sa*1.0, ca*1.0]])  
  return r

def rotY(a):
  ca = cos(a)
  sa = sin(a)
  r = array([[ca*1.0, 0., sa*1.0], [0., 1., 0.], [-sa*1.0, 0., ca*1.0]])
  return r

def rotYawPitchLeft(a):
  # Left YawPitch motor transform
  ca = cos(a)
  sa = sin(a)
  r = array([[ca, sa/sqrt(2), sa/sqrt(2)], [-sa/sqrt(2), .5*(1+ca), .5*(-1+ca)], [-sa/sqrt(2), .5*(-1+ca), .5*(1+ca)]])
  return r
  
def rotYawPitchRight(a):

  ca = cos(a)
  sa = sin(a)
  r = array([[ca, -sa/sqrt(2), sa/sqrt(2)], [sa/sqrt(2), .5*(1+ca), .5*(1-ca)], [-sa/sqrt(2), .5*(1-ca), .5*(1+ca)]])

  return r


def trPosition6D(p):
  # Returns 4x4 homogeneous transform matrix from 6D position parameters
  if len(p) < 6:
    p[5] = 0
    
  x = p[0]
  y = p[1]
  z = p[2]
  wx = p[3]
  wy = p[4]
  wz = p[5]

  # tr = trans(x,y,z)*rotZ(wz)*rotY(wy)*rotX(wx)

  cwx = cos(wx)
  swx = sin(wx)
  cwy = cos(wy)
  swy = sin(wy)
  cwz = cos(wz)
  swz = sin(wz)

  tr = array([[cwy*cwz,  swx*swy*cwz-cwx*swz, cwx*swy*cwz+swx*swz, x], [cwy*swz, swx*swy*swz+cwx*cwz, \
      cwx*swy*swz-swx*cwz, y], [-swy, swx*cwy, cwx*cwy, z], [0., 0., 0., 1.]])

      
  return tr



def ikineLegs(*args):

  # ikineLegs(chain, pChain, pBody, hipYawPitch)
  
  # Return joint angles using inverse kinematics for Nao legs
  # chain = 'LLeg' or 'RLeg'
  # pChain = Position6D description of end effector
  # pBody = Position6D description of Nao wait
  # hipYawPitch = [] or set position of hipYawPitch motor


  chain = args[0]
  pChain = args[1]
  pBody = args[2]

  if len(args) > 3 and size(args[3]) == 0:

    hipYawPitch = args[3]
  elif len(args) > 3 and size(args[3]) != 0:
    hipYawPitch = args[3][0]  
  
  if args < 4:
    hipYawPitch = array([])


  if args < 3:
    pBody = zeros((6,1))

  chainU = chain[0]

  
  if chainU.upper() == 'L':
    left = 1
  else:
    left = 0

 

  hipOffsetY = .050
  hipOffsetZ = .085
  thighLength = .1001
  tibiaLength = .1027
  footHeight = .046


  trChain = trPosition6D(pChain)
  trBody = trPosition6D(pBody)

  trBody_Chain = dot(inv(trBody),trChain)
  trChain_Body = dot(inv(trChain),trBody)

  
  
  if left:
    pHO = array([0, hipOffsetY, -hipOffsetZ, 1])
    pHipOffset = pHO.transpose()

  else:
    pHO = array([0, -hipOffsetY, -hipOffsetZ, 1])
    pHipOffset = pHO.transpose()


  pLegT = array([0, 0, footHeight, 0])
  pLeg = dot(trChain_Body,pHipOffset) - pLegT.transpose()

  
  pLeg_old = (pLeg[0:3])**2 
  dLeg = pLeg_old.sum()
  cKnee = .5*(dLeg - tibiaLength**2 - thighLength**2)/(tibiaLength*thighLength)
  
  kneePitch = acos(min(max(cKnee, -1), 1))
  ankleRoll = atan2(pLeg[1], pLeg[2]) 

  
  num = finfo(float)
  lLeg = max(sqrt(dLeg), num.eps)
  pitch0 = asin(thighLength*sin(kneePitch)/lLeg)
  anklePitch = asin(-pLeg[0]/lLeg) - pitch0 


  if size(hipYawPitch) == 0: 
    rHip = rotY(anklePitch+kneePitch)*rotX(ankleRoll)*trChain_Body[0:3,0:3]
    if left:
      hipYawPitch = atan2(sqrt(2)*rHip[1][0], rHip[1][1]+rHip[1][2])
    else:
      hipYawPitch = atan2(-sqrt(2)*rHip[1][0], rHip[1][1]-rHip[1][2])
    

  pAnkleT = array([0, 0, footHeight, 1])
  pAnkle = dot(trBody_Chain,pAnkleT.transpose()) - pHipOffset

  if left:
    pAnkleT = rotYawPitchLeft(hipYawPitch)
    pAnkle = dot(pAnkleT.transpose(),pAnkle[0:3])
  else:
    pAnkleT =rotYawPitchRight(hipYawPitch)

    pAnkle = dot(pAnkleT.transpose(),pAnkle[0:3])


  hipRoll = atan2(pAnkle[1],-pAnkle[2]) 
  pitch1 = asin(tibiaLength*sin(kneePitch)/lLeg)
  hipPitch = asin(-pAnkle[0]/lLeg) - pitch1 
  if type(hipYawPitch) == ndarray:
    hipYawPitch = hipYawPitch[0]
  qT = array([hipYawPitch, hipRoll, hipPitch, kneePitch, anklePitch, ankleRoll])
  print "AFTER: ",qT
  q = qT.transpose()


  return q



