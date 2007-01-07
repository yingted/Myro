import Tkinter

class Joystick(Tkinter.Toplevel):

   def __init__(self, parent = None, robot = None):
      Tkinter.Toplevel.__init__(self, parent)
      self.debug = 0
      self._running = 0
      self.robot = robot
      self.parent = parent
      self.wm_title('Joystick')
      self.protocol('WM_DELETE_WINDOW',self.destroy)
      self.frame = Tkinter.Frame(self)
      label = Tkinter.Label(self.frame, text = "Forward")
      label.pack(side = "top")
      label = Tkinter.Label(self.frame, text = "Reverse")
      label.pack(side = "bottom")
      label = Tkinter.Label(self.frame, text = "Turn\nLeft")
      label.pack(side = "left")
      label = Tkinter.Label(self.frame, text = "Turn\nRight")
      label.pack(side = "right")
      self.canvas = Tkinter.Canvas(self.frame,
                                   width = 220,
                                   height = 220,
                                   bg = 'white')
      self.initHandlers()
      self.canvas.pack(side=Tkinter.BOTTOM)

      self.circle_dim = (10, 10, 210, 210) #x0, y0, x1, y1
      self.circle = self.canvas.create_oval(self.circle_dim, fill = 'white')
      self.canvas.create_oval(105, 105, 115, 115, fill='black')

      self.frame.pack()
      self.translate = 0.0
      self.rotate = 0.0
      self.threshold = 0.10

   def initHandlers(self):
      self.canvas.bind("<ButtonRelease-1>", self.canvas_clicked_up)
      self.canvas.bind("<Button-1>", self.canvas_clicked_down)
      self.canvas.bind("<B1-Motion>", self.canvas_moved)

   def getValue(self, event = None):
      return self.translate, self.rotate

   def move(self, translate, rotate):
      self.translate = translate
      if self.translate < 0.0:
         self.translate += self.threshold
      elif self.translate > 0.0:
         self.translate -= self.threshold
      self.rotate = rotate
      if self.rotate < 0.0:
         self.rotate += self.threshold
      elif self.rotate > 0.0:
         self.rotate -= self.threshold
      if self.debug:
         print self.translate, self.rotate
      if self.robot != None:
         self.robot.lock.acquire()
         self.robot.move(self.translate, self.rotate)
         self.robot.lock.release()

   def canvas_clicked_up(self, event):
      self.canvas.delete("lines")
      self.move(0.0, 0.0)

   def drawArrows(self, x, y, trans, rotate):
      if trans == 0:
         self.canvas.create_line(110, 110, 110, y, width=3, fill="blue", tag="lines")
      else:
         self.canvas.create_line(110, 110, 110, y, width=3, fill="blue", tag="lines", arrowshape = (10, 10, 3), arrow = "last")
      if rotate == 0:
         self.canvas.create_line(110, 110, x, 110, width=3, fill="red", tag="lines")
      else:
         self.canvas.create_line(110, 110, x, 110, width=3, fill="red", tag="lines", arrowshape = (10, 10, 3), arrow = "last")

   def canvas_clicked_down(self, event):
      if self.in_circle(event.x, event.y):
         trans, rotate = self.calc_tr(event.x, event.y)
         self.drawArrows(event.x, event.y, trans, rotate)
         self.move(trans, rotate)

   def canvas_moved(self, event):
      if self.in_circle(event.x, event.y):
         self.canvas.delete("lines")
         trans, rotate = self.calc_tr(event.x, event.y)
         self.drawArrows(event.x, event.y, trans, rotate)         
         self.move(trans, rotate)

   def stop(self):
      self.move(0.0, 0.0)

   def in_circle(self, x, y):
      r2 = ((self.circle_dim[2] - self.circle_dim[0])/2)**2
           
      center = ((self.circle_dim[2] + self.circle_dim[0])/2,
                (self.circle_dim[3] + self.circle_dim[1])/2)
      #x in?
      dist2 = (center[0] - x)**2 + (center[1] - y)**2
      if (dist2 < r2):
         return 1
      else:
         return 0

   def calc_tr(self, x, y):
      #right is negative
      center = ((self.circle_dim[2] + self.circle_dim[0])/2,
                (self.circle_dim[3] + self.circle_dim[1])/2)
      rot = float(center[0] - x) / float(center[0] - self.circle_dim[0])
      trans = float(center[1] - y) / float(center[1] - self.circle_dim[1])
      if abs(rot) < self.threshold:
         rot = 0.0
      if abs(trans) < self.threshold:
         trans = 0.0
      return (trans, rot)

   def destroy(self):
      """Hides the device view window."""
      self.withdraw()
      if self._running:
         if "quit" in dir(self.parent):
            self.parent.quit()
         if "destroy" in dir(self.parent):
            self.parent.destroy()

if __name__ == '__main__':
   app = Tkinter.Tk()
   app.withdraw()
   joystick = Joystick(parent = app)
   app.mainloop()
