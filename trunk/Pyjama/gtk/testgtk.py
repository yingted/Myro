import clr
clr.AddReference("Graphics.dll")
import Graphics
Graphics.init()
win = Graphics.GraphWin("Title")
print win.Title
win.Title = "New Title"
button = Graphics.Button("Press Me!")
win.Add(button)
Graphics.Show(button)
