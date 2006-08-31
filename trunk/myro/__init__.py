"""
Myro Base Classes.
(c) 2006, Institute for Personal Robots in Education
http://roboteducation.org/
Distributed under a Shared Source License
"""

__REVISION__ = "$Revision$"
__VERSION__  = __REVISION__.split()[1]
__AUTHOR__   = "Doug Blank <dblank@brynmawr.edu>"

class Robot(object):
    def __init__(self):
        """
        Base robot class.
        """
        pass

    def forward(self, amount):
        raise AttributeError, "this method needs to be written"

    def backward(self, amount):
        raise AttributeError, "this method needs to be written"

    def left(self, amount):
        raise AttributeError, "this method needs to be written"

    def right(self, amount):
        raise AttributeError, "this method needs to be written"

    def move(self, translate, rotate):
        raise AttributeError, "this method needs to be written"

    def translate(self, amount):
        raise AttributeError, "this method needs to be written"

    def rotate(self, amount):
        raise AttributeError, "this method needs to be written"
        

class ScribblerRobot(Robot):
    Robot.__init__(self)
    
