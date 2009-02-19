MONO_IOMAP=all

pyjama.exe: 
	MONO_PATH=../libs/DLR gmcs \
	-recurse:*.cs \
	-main:Pyjama.Program \
	-out:pyjama.exe \
	-r:Microsoft.VisualBasic.dll \
	-r:System.Windows.Forms \
	-r:System.Drawing \
	-r:Mono.Security \
	-r:IronPython.dll \
	-r:IronPython.Modules.dll \
	-r:IronPythonTest.dll \
	-r:Microsoft.Scripting.Core.dll \
	-r:Microsoft.Scripting.dll \
	-res:Dialogs/OptionFrames/FontControl.resources \
	-res:Dialogs/AboutDialog.resources \
	-res:Dialogs/OptionsDialog.resources \
	-res:Controls/DocumentPage.resources \
	-res:Controls/DocumentManager.resources \
	-res:Controls/Console.resources \
	-res:BaseDialog.resources \
	-res:BaseForm.resources \
	-res:PyjamaForm.resources \
	-res:Properties/Resources.resources



resx:
	resgen Dialogs/OptionFrames/FontControl.resx
	resgen Dialogs/AboutDialog.resx
	resgen Dialogs/OptionsDialog.resx
	resgen Controls/DocumentPage.resx
	resgen Controls/DocumentManager.resx
	resgen Controls/Console.resx
	resgen BaseDialog.resx
	resgen BaseForm.resx
	resgen PyjamaForm.resx
	cd Properties; resgen Resources.resx

clean:
	$(RM) BaseDialog.resources \
		BaseForm.resources \
		Controls/Console.resources \
		Controls/DocumentManager.resources \
		Controls/DocumentPage.resources \
		Dialogs/AboutDialog.resources \
		Dialogs/OptionFrames/FontControl.resources \
		Dialogs/OptionsDialog.resources \
		Properties/Resources.resources \
		PyjamaForm.resources \
		pyjama.exe

remove:
	rm Dialogs/NewProjectDialog.resx
	rm Dialogs/NewProjectDialog.Designer.cs
	rm Dialogs/NewProjectDialog.cs
	rm Dialogs/NewFileDialog.resx
	rm Dialogs/NewFileDialog.Designer.cs
	rm Dialogs/NewFileDialog.cs
	rm Controls/CodeEditorIDETextBox.Designer.cs
	rm Controls/CodeEditorIDETextBox.cs
