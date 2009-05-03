

SimpleApp.exe: Resources.resources SimpleApp.Form1.resources
	MONO_PATH=../libs /usr/local/bin/gmcs -recurse:*.cs \
	-r:System.Data -r:System.Drawing -r:System.Windows.Forms \
	-r:../libs/IronTextBox -r:../libs/IronPython \
	-res:Resources.resources \
	-res:SimpleApp.Form1.resources \
	-r:../libs/Microsoft.Scripting -out:SimpleApp.exe

Resources.resources: 
	resgen Properties/Resources.resx Resources.resources

SimpleApp.Form1.resources:
	resgen Form1.resx SimpleApp.Form1.resources

run: SimpleApp.exe
	MONO_PATH=../libs/ /usr/local/bin/mono SimpleApp.exe

clean:
	rm -f *.resources *.exe *~
