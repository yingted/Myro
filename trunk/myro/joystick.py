import Tkinter, time

class Joystick(Tkinter.Toplevel):

   def __init__(self, parent = None, robot = None, showSensors = 0):
      Tkinter.Toplevel.__init__(self, parent)
      self.debug = 0
      self._running = 0
      self.robot = robot
      self.showSensors = showSensors
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
      self.widgets = {}
      if self.showSensors:
         newFrame = Tkinter.Frame(self, relief=Tkinter.RAISED, borderwidth=2)
         self.addWidgets(newFrame, ("Light", 3), ("Line", 2), ("IR", 2), ("Stall", 1))
         newFrame.pack(side="bottom", fill="both", expand="y")
      self.initHandlers()
      self.canvas.pack(side=Tkinter.BOTTOM)

      self.circle_dim = (10, 10, 210, 210) #x0, y0, x1, y1
      self.circle = self.canvas.create_oval(self.circle_dim, fill = 'white')
      self.canvas.create_oval(105, 105, 115, 115, fill='black')

      self.frame.pack()
      self.translate = 0.0
      self.rotate = 0.0
      self.threshold = 0.10
      self.delay = 0.05 # in seconds
      self.running = 0

   def destroy(self):
       self.running = 0
       Tkinter.Toplevel.destroy(self)

   def addWidgets(self, window, *items):
      for name, size in items:
         text = name + ":"
         frame = Tkinter.Frame(window)
         self.widgets[name + ".label"] = Tkinter.Label(frame, text=text, width=10)
         self.widgets[name + ".label"].pack(side="left")
         for i in range(size):
            self.widgets["%s%d.entry" % (name, i)] = Tkinter.Entry(frame, bg="white", width = 10)
            self.widgets["%s%d.entry" % (name, i)].insert(0, "")
            self.widgets["%s%d.entry" % (name, i)].pack(side="right", fill="both", expand="y")
         frame.pack(side="bottom", fill="both", expand="y")

   def updateWidget(self, name, pos, value):
      """Updates the device view window."""
      if not self.showSensors: return
      try:
          self.widgets["%s%d.entry" % (name, pos)].delete(0,'end')
          self.widgets["%s%d.entry" % (name, pos)].insert(0,value)
      except: pass

   def minorloop(self, delay = None): # in milliseconds
       """
       As opposed to mainloop. This is a simple loop that works
       in IDLE.
       """
       self.running = 1
       lastUpdated = 0
       lastData = []
       while self.running:
           if self.robot and self.showSensors:
               data = self.robot.getLastSensors()
               now = time.time()
               if data != lastData or now - lastUpdated > 1:
                   if now - lastUpdated > 1:
                       data = self.robot.getAll()
                   light = data.get("light", [0, 0, 0])
                   line = data.get("line", [0, 0])
                   ir = data.get("ir", [0, 0])
                   stall = [data.get("stall", 0)]
                   for i in range(len(light)):
                      self.updateWidget("Light", i, light[i])
                   for i in range(len(line)):
                      self.updateWidget("Line", i, line[i])
                   for i in range(len(ir)):
                      self.updateWidget("IR", i, ir[i])
                   for i in range(len(stall)):
                      self.updateWidget("Stall", i, stall[i])
                   lastUpdated = time.time()
                   lastData = data
           self.update()
           start = time.time()
           time.sleep(self.delay)

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
         #self.robot.lock.acquire()
         self.robot.move(self.translate, self.rotate)
         #self.robot.lock.release()

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

if __name__ == '__main__':
   app = Tkinter.Tk()
   app.withdraw()
   joystick = Joystick(parent = app)
   app.mainloop()
