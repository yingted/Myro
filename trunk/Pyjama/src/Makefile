## Makefile for building Pyjama with Mono

run: pyjama.exe
	XMODIFIERS= MONO_PATH=../libs mono --debug pyjama.exe

res:
	mkdir -p res

res/Pyjama.FontControl.resources: Dialogs/OptionFrames/FontControl.resx
	resgen Dialogs/OptionFrames/FontControl.resx res/Pyjama.FontControl.resources

res/Pyjama.AboutDialog.resources: Dialogs/AboutDialog.resx
	resgen Dialogs/AboutDialog.resx res/Pyjama.AboutDialog.resources

res/Pyjama.OptionsDialog.resources: Dialogs/OptionsDialog.resx
	resgen Dialogs/OptionsDialog.resx res/Pyjama.OptionsDialog.resources

res/Pyjama.DocumentPage.resources: Controls/DocumentPage.resx
	resgen Controls/DocumentPage.resx res/Pyjama.DocumentPage.resources

res/Pyjama.DocumentManager.resources: Controls/DocumentManager.resx
	resgen Controls/DocumentManager.resx res/Pyjama.DocumentManager.resources

res/Pyjama.Console.resources: Controls/Console.resx
	resgen Controls/Console.resx res/Pyjama.Console.resources

res/Pyjama.BaseDialog.resources: BaseDialog.resx
	resgen BaseDialog.resx res/Pyjama.BaseDialog.resources

res/Pyjama.BaseForm.resources: BaseForm.resx
	resgen BaseForm.resx res/Pyjama.BaseForm.resources

res/Pyjama.PyjamaForm.resources: PyjamaForm.resx
	resgen PyjamaForm.resx res/Pyjama.PyjamaForm.resources

res/Pyjama.Properties.Resources.resources: Properties/Resources.resx
	cd Properties; MONO_IOMAP=all resgen Resources.resx ../res/Pyjama.Properties.Resources.resources

pyjama.exe: res res/Pyjama.FontControl.resources \
		res/Pyjama.AboutDialog.resources \
		res/Pyjama.OptionsDialog.resources \
		res/Pyjama.DocumentPage.resources \
		res/Pyjama.DocumentManager.resources \
		res/Pyjama.Console.resources \
		res/Pyjama.BaseDialog.resources \
		res/Pyjama.BaseForm.resources \
		res/Pyjama.PyjamaForm.resources \
		res/Pyjama.Properties.Resources.resources \
		ActiveCodeFile.cs \
		ApplicationInformation.cs \
		ApplicationOptions.cs \
		BaseDialog.Designer.cs \
		BaseDialog.cs \
		BaseForm.Designer.cs \
		BaseForm.cs \
		CodeBlock.cs \
		Controls/Console.Designer.cs \
		Controls/Console.cs \
		Controls/Document.Designer.cs \
		Controls/Document.cs \
		Controls/DocumentInput.cs \
		Controls/DocumentManager.Designer.cs \
		Controls/DocumentManager.cs \
		Controls/DocumentPage.cs \
		Controls/IDocument.cs \
		Dialogs/AboutDialog.Designer.cs \
		Dialogs/AboutDialog.cs \
		Dialogs/OptionFrames/FontControl.Designer.cs \
		Dialogs/OptionFrames/FontControl.cs \
		Dialogs/OptionsDialog.Designer.cs \
		Dialogs/OptionsDialog.cs \
		IMainForm.cs \
		IronTextBox.cs \
		LanguageSettings.cs \
		MonoEnvironment.cs \
		PrintDocument.cs \
		Program.cs \
		Properties/AssemblyInfo.cs \
		Properties/Resources.Designer.cs \
		Properties/Settings.Designer.cs \
		Properties/Settings.settings \
		PyjamaForm.Designer.cs \
		PyjamaForm.cs \
		PyjamaFormController.cs \
		PyjamaInterfaces.cs \
		ShortcutKeyDispatcher.cs \
		UserSettings.cs 
	MONO_PATH=../libs \
	gmcs -debug -define:MONO \
	./ActiveCodeFile.cs \
	./ApplicationInformation.cs \
	./ApplicationOptions.cs \
	./BaseDialog.Designer.cs \
	./BaseDialog.cs \
	./BaseForm.Designer.cs \
	./BaseForm.cs \
	./CodeBlock.cs \
	./Controls/Console.Designer.cs \
	./Controls/Console.cs \
	./Controls/Document.Designer.cs \
	./Controls/Document.cs \
	./Controls/DocumentInput.cs \
	./Controls/DocumentManager.Designer.cs \
	./Controls/DocumentManager.cs \
	./Controls/DocumentPage.cs \
	./Controls/IDocument.cs \
	./Dialogs/AboutDialog.Designer.cs \
	./Dialogs/AboutDialog.cs \
	./Dialogs/OptionFrames/FontControl.Designer.cs \
	./Dialogs/OptionFrames/FontControl.cs \
	./Dialogs/OptionsDialog.Designer.cs \
	./Dialogs/OptionsDialog.cs \
	./IMainForm.cs \
	./IronTextBox.cs \
	./LanguageSettings.cs \
	./MonoEnvironment.cs \
	./PrintDocument.cs \
	./Program.cs \
	./Properties/AssemblyInfo.cs \
	./Properties/Resources.Designer.cs \
	./Properties/Settings.Designer.cs \
	./PyjamaForm.Designer.cs \
	./PyjamaForm.cs \
	./PyjamaFormController.cs \
	./PyjamaInterfaces.cs \
	./ShortcutKeyDispatcher.cs \
	./UserSettings.cs \
	-main:Pyjama.Program \
	-out:pyjama.exe \
	-r:System.Windows.Forms \
	-r:System.Drawing \
	-r:Mono.Security \
	-r:IronPython.dll \
	-r:IronPython.Modules.dll \
	-r:IronRuby.dll \
	-r:IronRuby.Libraries.dll \
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

clean:
	$(RM) -rf res pyjama.exe

