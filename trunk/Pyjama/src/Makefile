## Makefile for building Pyjama with Mono

pyjama.exe: 
	MONO_PATH=../libs:/usr/lib/mono/2.0/ \
	 /usr/bin/gmcs \
	-recurse:*.cs \
	-main:Pyjama.Program \
	-out:pyjama.exe \
	-r:Microsoft.VisualBasic.dll \
	-r:System.Windows.Forms \
	-r:System.Drawing \
	-r:Mono.Security \
	-r:IronPython.dll \
	-r:IronPython.Modules.dll \
	-r:Microsoft.Scripting.Core.dll \
	-r:Microsoft.Scripting.dll \
	-res:res/Pyjama.FontControl.resources \
	-res:res/Pyjama.AboutDialog.resources \
	-res:res/Pyjama.OptionsDialog.resources \
	-res:res/Pyjama.DocumentPage.resources \
	-res:res/Pyjama.DocumentManager.resources \
	-res:res/Pyjama.Console.resources \
	-res:res/Pyjama.BaseDialog.resources \
	-res:res/Pyjama.BaseForm.resources \
	-res:res/Pyjama.PyjamaForm.resources \
	-res:res/Pyjama.Properties.Resources.resources \
	-res:Properties/Settings.settings

resx: 
	mkdir -p res
	resgen Dialogs/OptionFrames/FontControl.resx res/Pyjama.FontControl.resources
	resgen Dialogs/AboutDialog.resx res/Pyjama.AboutDialog.resources
	resgen Dialogs/OptionsDialog.resx res/Pyjama.OptionsDialog.resources
	resgen Controls/DocumentPage.resx res/Pyjama.DocumentPage.resources
	resgen Controls/DocumentManager.resx res/Pyjama.DocumentManager.resources
	resgen Controls/Console.resx res/Pyjama.Console.resources
	resgen BaseDialog.resx res/Pyjama.BaseDialog.resources
	resgen BaseForm.resx res/Pyjama.BaseForm.resources
	resgen PyjamaForm.resx res/Pyjama.PyjamaForm.resources
	cd Properties; MONO_IOMAP=all resgen Resources.resx ../res/Pyjama.Properties.Resources.resources

run: pyjama.exe
	XMODIFIERS= MONO_PATH=../libs /usr/bin/mono pyjama.exe

clean:
	$(RM) res/Pyjama.BaseDialog.resources \
		res/Pyjama.BaseForm.resources \
		res/Pyjama.Console.resources \
		res/Pyjama.DocumentManager.resources \
		res/Pyjama.DocumentPage.resources \
		res/Pyjama.AboutDialog.resources \
		res/Pyjama.FontControl.resources \
		res/Pyjama.OptionsDialog.resources \
		res/Pyjama.Resources.resources \
		res/Pyjama.PyjamaForm.resources \
		pyjama.exe

