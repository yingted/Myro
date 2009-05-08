
IronTextBox.dll: ./Paths.cs ./IPEWrapper.cs ./IronTextBox.cs ./Properties/AssemblyInfo.cs
	MONO_PATH=../libs /usr/local/bin/gmcs \
	./Paths.cs \
	./IPEWrapper.cs \
	./IronTextBox.cs \
	./Properties/AssemblyInfo.cs \
	-r:System.Data \
	-r:System.Drawing \
	-r:System.Windows.Forms \
	-r:../libs/IronPython \
	-r:../libs/IronRuby \
	-r:../libs/Microsoft.Scripting \
	-target:library -out:IronTextBox.dll
	cp IronTextBox.dll ../libs/

clean:
	rm -f *~ IronTextBox.dll ../libs/IronTextBox.dll


